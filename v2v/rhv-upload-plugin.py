# -*- python -*-
# oVirt or RHV upload nbdkit plugin used by ‘virt-v2v -o rhv-upload’
# Copyright (C) 2018 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

import builtins
import functools
import inspect
import json
import logging
import socket
import ssl
import sys
import time

from http.client import HTTPSConnection, HTTPConnection
from urllib.parse import urlparse

import ovirtsdk4 as sdk
import ovirtsdk4.types as types

# Timeout to wait for oVirt disks to change status, or the transfer
# object to finish initializing [seconds].
timeout = 5*60

# Parameters are passed in via a JSON doc from the OCaml code.
# Because this Python code ships embedded inside virt-v2v there
# is no formal API here.
params = None

def config(key, value):
    global params

    if key == "params":
        with builtins.open(value, 'r') as fp:
            params = json.load(fp)
        debug("using params: %s" % params)
    else:
        raise RuntimeError("unknown configuration key '%s'" % key)

def config_complete():
    if params is None:
        raise RuntimeError("missing configuration parameters")

def debug(s):
    if params['verbose']:
        print(s, file=sys.stderr)
        sys.stderr.flush()

def read_password():
    """
    Read the password from file.
    """
    with builtins.open(params['output_password'], 'r') as fp:
        data = fp.read()
    return data.rstrip()

def parse_username():
    """
    Parse out the username from the output_conn URL.
    """
    parsed = urlparse(params['output_conn'])
    return parsed.username or "admin@internal"

def failing(func):
    """
    Decorator marking the handle as failed if any expection is raised in the
    decorated function.  This is used in close() to cleanup properly after
    failures.
    """
    @functools.wraps(func)
    def wrapper(h, *args):
        try:
            return func(h, *args)
        except:
            h['failed'] = True
            raise

    return wrapper

def open(readonly):
    connection = sdk.Connection(
        url = params['output_conn'],
        username = parse_username(),
        password = read_password(),
        ca_file = params['rhv_cafile'],
        log = logging.getLogger(),
        insecure = params['insecure'],
    )

    # Use the local host is possible.
    host = find_host(connection) if params['rhv_direct'] else None
    disk = create_disk(connection)

    transfer = create_transfer(connection, disk, host)
    try:
        destination_url = parse_transfer_url(transfer)
        http = create_http(destination_url)
        options = get_options(http, destination_url)
        http = optimize_http(http, host, options)
    except:
        cancel_transfer(connection, transfer)
        raise

    debug("imageio features: flush=%(can_flush)r trim=%(can_trim)r "
          "zero=%(can_zero)r unix_socket=%(unix_socket)r"
          % options)

    # Save everything we need to make requests in the handle.
    return {
        'can_flush': options['can_flush'],
        'can_trim': options['can_trim'],
        'can_zero': options['can_zero'],
        'needs_auth': options['needs_auth'],
        'connection': connection,
        'disk_id': disk.id,
        'transfer': transfer,
        'failed': False,
        'highestwrite': 0,
        'http': http,
        'path': destination_url.path,
    }

@failing
def can_trim(h):
    return h['can_trim']

@failing
def can_flush(h):
    return h['can_flush']

@failing
def get_size(h):
    return params['disk_size']

# Any unexpected HTTP response status from the server will end up calling this
# function which logs the full error, and raises a RuntimeError exception.
def request_failed(r, msg):
    status = r.status
    reason = r.reason
    try:
        body = r.read()
    except EnvironmentError as e:
        body = "(Unable to read response body: %s)" % e

    # Log the full error if we're verbose.
    debug("unexpected response from imageio server:")
    debug(msg)
    debug("%d: %s" % (status, reason))
    debug(body)

    # Only a short error is included in the exception.
    raise RuntimeError("%s: %d %s: %r" % (msg, status, reason, body[:200]))

# For documentation see:
# https://github.com/oVirt/ovirt-imageio/blob/master/docs/random-io.md
# For examples of working code to read/write from the server, see:
# https://github.com/oVirt/ovirt-imageio/blob/master/daemon/test/server_test.py

@failing
def pread(h, count, offset):
    http = h['http']
    transfer = h['transfer']

    headers = {"Range": "bytes=%d-%d" % (offset, offset+count-1)}
    if h['needs_auth']:
        headers["Authorization"] = transfer.signed_ticket

    http.request("GET", h['path'], headers=headers)

    r = http.getresponse()
    # 206 = HTTP Partial Content.
    if r.status != 206:
        request_failed(r,
                       "could not read sector offset %d size %d" %
                       (offset, count))

    return r.read()

@failing
def pwrite(h, buf, offset):
    http = h['http']
    transfer = h['transfer']

    count = len(buf)
    h['highestwrite'] = max(h['highestwrite'], offset+count)

    http.putrequest("PUT", h['path'] + "?flush=n")
    if h['needs_auth']:
        http.putheader("Authorization", transfer.signed_ticket)
    # The oVirt server only uses the first part of the range, and the
    # content-length.
    http.putheader("Content-Range", "bytes %d-%d/*" % (offset, offset+count-1))
    http.putheader("Content-Length", str(count))
    http.endheaders()

    try:
        http.send(buf)
    except BrokenPipeError:
        pass

    r = http.getresponse()
    if r.status != 200:
        request_failed(r,
                       "could not write sector offset %d size %d" %
                       (offset, count))

    r.read()

@failing
def zero(h, count, offset, may_trim):
    http = h['http']

    # Unlike the trim and flush calls, there is no 'can_zero' method
    # so nbdkit could call this even if the server doesn't support
    # zeroing.  If this is the case we must emulate.
    if not h['can_zero']:
        emulate_zero(h, count, offset)
        return

    # Construct the JSON request for zeroing.
    buf = json.dumps({'op': "zero",
                      'offset': offset,
                      'size': count,
                      'flush': False}).encode()

    headers = {"Content-Type": "application/json",
               "Content-Length": str(len(buf))}

    http.request("PATCH", h['path'], body=buf, headers=headers)

    r = http.getresponse()
    if r.status != 200:
        request_failed(r,
                       "could not zero sector offset %d size %d" %
                       (offset, count))

    r.read()

def emulate_zero(h, count, offset):
    http = h['http']
    transfer = h['transfer']

    # qemu-img convert starts by trying to zero/trim the whole device.
    # Since we've just created a new disk it's safe to ignore these
    # requests as long as they are smaller than the highest write seen.
    # After that we must emulate them with writes.
    if offset+count < h['highestwrite']:
        http.putrequest("PUT", h['path'])
        if h['needs_auth']:
            http.putheader("Authorization", transfer.signed_ticket)
        http.putheader("Content-Range",
                       "bytes %d-%d/*" % (offset, offset+count-1))
        http.putheader("Content-Length", str(count))
        http.endheaders()

        try:
            buf = bytearray(128*1024)
            while count > len(buf):
                http.send(buf)
                count -= len(buf)
            http.send(memoryview(buf)[:count])
        except BrokenPipeError:
            pass

        r = http.getresponse()
        if r.status != 200:
            request_failed(r,
                           "could not write zeroes offset %d size %d" %
                           (offset, count))

        r.read()

@failing
def trim(h, count, offset):
    http = h['http']

    # Construct the JSON request for trimming.
    buf = json.dumps({'op': "trim",
                      'offset': offset,
                      'size': count,
                      'flush': False}).encode()

    headers = {"Content-Type": "application/json",
               "Content-Length": str(len(buf))}

    http.request("PATCH", h['path'], body=buf, headers=headers)

    r = http.getresponse()
    if r.status != 200:
        request_failed(r,
                       "could not trim sector offset %d size %d" %
                       (offset, count))

    r.read()

@failing
def flush(h):
    http = h['http']

    # Construct the JSON request for flushing.
    buf = json.dumps({'op': "flush"}).encode()

    headers = {"Content-Type": "application/json",
               "Content-Length": str(len(buf))}

    http.request("PATCH", h['path'], body=buf, headers=headers)

    r = http.getresponse()
    if r.status != 200:
        request_failed(r, "could not flush")

    r.read()

def close(h):
    http = h['http']
    connection = h['connection']
    transfer = h['transfer']
    disk_id = h['disk_id']

    # This is sometimes necessary because python doesn't set up
    # sys.stderr to be line buffered and so debug, errors or
    # exceptions printed previously might not be emitted before the
    # plugin exits.
    sys.stderr.flush()

    http.close()

    # If the connection failed earlier ensure we cancel the trasfer. Canceling
    # the transfer will delete the disk.
    if h['failed']:
        try:
            cancel_transfer(connection, transfer)
        finally:
            connection.close()
        return

    # Try to finalize the transfer. On errors the transfer may be paused by the
    # system, and we need to cancel the transfer to remove the disk.
    try:
        finalize_transfer(connection, transfer, disk_id)
    except:
        cancel_transfer(connection, transfer)
        raise
    else:
        # Write the disk ID file.  Only do this on successful completion.
        with builtins.open(params['diskid_file'], 'w') as fp:
            fp.write(disk_id)
    finally:
        connection.close()

# Modify http.client.HTTPConnection to work over a Unix domain socket.
# Derived from uhttplib written by Erik van Zijst under an MIT license.
# (https://pypi.org/project/uhttplib/)
# Ported to Python 3 by Irit Goihman.

class UnixHTTPConnection(HTTPConnection):
    def __init__(self, path, timeout=socket._GLOBAL_DEFAULT_TIMEOUT):
        self.path = path
        HTTPConnection.__init__(self, "localhost", timeout=timeout)

    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        if self.timeout is not socket._GLOBAL_DEFAULT_TIMEOUT:
            self.sock.settimeout(timeout)
        self.sock.connect(self.path)

# oVirt SDK operations

def find_host(connection):
    """Return the current host object or None."""
    try:
        with builtins.open("/etc/vdsm/vdsm.id") as f:
            vdsm_id = f.readline().strip()
    except Exception as e:
        # This is most likely not an oVirt host.
        debug("cannot read /etc/vdsm/vdsm.id, using any host: %s" % e)
        return None

    debug("hw_id = %r" % vdsm_id)

    system_service = connection.system_service()
    storage_name = params['output_storage']
    data_centers = system_service.data_centers_service().list(
        search='storage.name=%s' % storage_name,
        case_sensitive=True,
    )
    if len(data_centers) == 0:
        # The storage domain is not attached to a datacenter
        # (shouldn't happen, would fail on disk creation).
        debug("storange domain (%s) is not attached to a DC" % storage_name)
        return None

    datacenter = data_centers[0]
    debug("datacenter = %s" % datacenter.name)

    hosts_service = system_service.hosts_service()
    hosts = hosts_service.list(
        search="hw_id=%s and datacenter=%s and status=Up"
               % (vdsm_id, datacenter.name),
        case_sensitive=True,
    )
    if len(hosts) == 0:
        # Couldn't find a host that's fulfilling the following criteria:
        # - 'hw_id' equals to 'vdsm_id'
        # - Its status is 'Up'
        # - Belongs to the storage domain's datacenter
        debug("cannot find a running host with hw_id=%r, "
              "that belongs to datacenter '%s', "
              "using any host" % (vdsm_id, datacenter.name))
        return None

    host = hosts[0]
    debug("host.id = %r" % host.id)

    return types.Host(id = host.id)

def create_disk(connection):
    """
    Create a new disk for the transfer and wait until the disk is ready.

    Returns disk object.
    """
    system_service = connection.system_service()
    disks_service = system_service.disks_service()

    if params['disk_format'] == "raw":
        disk_format = types.DiskFormat.RAW
    else:
        disk_format = types.DiskFormat.COW

    disk = disks_service.add(
        disk = types.Disk(
            # The ID is optional.
            id = params.get('rhv_disk_uuid'),
            name = params['disk_name'],
            description = "Uploaded by virt-v2v",
            format = disk_format,
            # XXX For qcow2 disk on block storage, we should use the estimated
            # size, based on qemu-img measure of the overlay.
            initial_size = params['disk_size'],
            provisioned_size = params['disk_size'],
            # XXX Ignores params['output_sparse'].
            # Handling this properly will be complex, see:
            # https://www.redhat.com/archives/libguestfs/2018-March/msg00177.html
            sparse = True,
            storage_domains = [
                types.StorageDomain(
                    name = params['output_storage'],
                )
            ],
        )
    )

    debug("disk.id = %r" % disk.id)

    # Wait till the disk moved from LOCKED state to OK state, as the transfer
    # can't start if the disk is locked.

    disk_service = disks_service.disk_service(disk.id)
    endt = time.time() + timeout
    while True:
        time.sleep(1)
        disk = disk_service.get()
        if disk.status == types.DiskStatus.OK:
            break
        if time.time() > endt:
            raise RuntimeError(
                "timed out waiting for disk %s to become unlocked" % disk.id)

    return disk

def create_transfer(connection, disk, host):
    """
    Create image transfer and wait until the transfer is ready.

    Returns a transfer object.
    """
    system_service = connection.system_service()
    transfers_service = system_service.image_transfers_service()

    extra = {}
    if transfer_supports_format():
        extra["format"] = types.DiskFormat.RAW

    transfer = transfers_service.add(
        types.ImageTransfer(
            disk = types.Disk(id = disk.id),
            host = host,
            inactivity_timeout = 3600,
            **extra,
        )
    )

    # At this point the transfer owns the disk and will delete the disk if the
    # transfer is canceled, or if finalizing the transfer fails.

    debug("transfer.id = %r" % transfer.id)

    # Get a reference to the created transfer service.
    transfer_service = transfers_service.image_transfer_service(transfer.id)

    # Wait until transfer's phase change from INITIALIZING to TRANSFERRING. On
    # errors transfer's phase can change to PAUSED_SYSTEM or FINISHED_FAILURE.
    # If the transfer was paused, we need to cancel it to remove the disk,
    # otherwise the system will remove the disk and transfer shortly after.

    endt = time.time() + timeout
    while True:
        time.sleep(1)
        try:
            transfer = transfer_service.get()
        except sdk.NotFoundError:
            # The system has removed the disk and the transfer.
            raise RuntimeError("transfer %s was removed" % transfer.id)

        if transfer.phase == types.ImageTransferPhase.FINISHED_FAILURE:
            # The system will remove the disk and the transfer soon.
            raise RuntimeError(
                "transfer %s has failed" % transfer.id)

        if transfer.phase == types.ImageTransferPhase.PAUSED_SYSTEM:
            transfer_service.cancel()
            raise RuntimeError(
                "transfer %s was paused by system" % transfer.id)

        if transfer.phase == types.ImageTransferPhase.TRANSFERRING:
            break

        if transfer.phase != types.ImageTransferPhase.INITIALIZING:
            transfer_service.cancel()
            raise RuntimeError(
                "unexpected transfer %s phase %s"
                % (transfer.id, transfer.phase))

        if time.time() > endt:
            transfer_service.cancel()
            raise RuntimeError(
                "timed out waiting for transfer %s" % transfer.id)

    return transfer

def cancel_transfer(connection, transfer):
    """
    Cancel a transfer, removing the transfer disk.
    """
    debug("canceling transfer %s" % transfer.id)
    transfer_service = (connection.system_service()
                            .image_transfers_service()
                            .image_transfer_service(transfer.id))
    transfer_service.cancel()

def finalize_transfer(connection, transfer, disk_id):
    """
    Finalize a transfer, making the transfer disk available.

    If finalizing succeeds, transfer's phase will change to FINISHED_SUCCESS
    and the transer's disk status will change to OK.  On errors, the transfer's
    phase will change to FINISHED_FAILURE and the disk status will change to
    ILLEGAL and it will be removed. In both cases the transfer entity will be
    removed shortly after.

    If oVirt fails to finalize the transfer, transfer's phase will change to
    PAUSED_SYSTEM. In this case the disk's status will change to ILLEGAL and it
    will not be removed.

    For simplicity, we track only disk's status changes.

    For more info see:
    - http://ovirt.github.io/ovirt-engine-api-model/4.4/#services/image_transfer
    - http://ovirt.github.io/ovirt-engine-sdk/master/types.m.html#ovirtsdk4.types.ImageTransfer
    """
    debug("finalizing transfer %s" % transfer.id)
    transfer_service = (connection.system_service()
                            .image_transfers_service()
                            .image_transfer_service(transfer.id))

    start = time.time()

    transfer_service.finalize()

    disk_service = (connection.system_service()
                        .disks_service()
                        .disk_service(disk_id))

    while True:
        time.sleep(1)
        try:
            disk = disk_service.get()
        except sdk.NotFoundError:
            # Disk verification failed and the system removed the disk.
            raise RuntimeError(
                "transfer %s failed: disk %s was removed"
                % (transfer.id, disk_id))

        if disk.status == types.DiskStatus.ILLEGAL:
            # Disk verification failed or transfer was paused by the system.
            raise RuntimeError(
                "transfer %s failed: disk is ILLEGAL" % transfer.id)

        if disk.status == types.DiskStatus.OK:
            debug("transfer %s finalized in %.3f seconds"
                  % (transfer.id, time.time() - start))
            break

        if time.time() > start + timeout:
            raise RuntimeError(
                "timed out waiting for transfer %s to finalize"
                % transfer.id)

def transfer_supports_format():
    """
    Return True if transfer supports the "format" argument, enabing the NBD
    bakend on imageio side, which allows uploading to qcow2 images.

    This feature was added in ovirt 4.3. We assume that the SDK version matches
    engine version.
    """
    sig = inspect.signature(types.ImageTransfer)
    return "format" in sig.parameters

# oVirt imageio operations

def parse_transfer_url(transfer):
    """
    Returns a parsed transfer url, preferring direct transfer if possible.
    """
    if params['rhv_direct']:
        if transfer.transfer_url is None:
            raise RuntimeError("direct upload to host not supported, "
                               "requires ovirt-engine >= 4.2 and only works "
                               "when virt-v2v is run within the oVirt/RHV "
                               "environment, eg. on an oVirt node.")
        return urlparse(transfer.transfer_url)
    else:
        return urlparse(transfer.proxy_url)

def create_http(url):
    """
    Create http connection for transfer url.

    Returns HTTPConnection.
    """
    if url.scheme == "https":
        context = \
            ssl.create_default_context(purpose = ssl.Purpose.SERVER_AUTH,
                                       cafile = params['rhv_cafile'])
        if params['insecure']:
            context.check_hostname = False
            context.verify_mode = ssl.CERT_NONE

        return HTTPSConnection(url.hostname, url.port, context = context)
    elif url.scheme == "http":
        return HTTPConnection(url.hostname, url.port)
    else:
        raise RuntimeError("unknown URL scheme (%s)" % url.scheme)

def get_options(http, url):
    """
    Send OPTIONS request to imageio server and return options dict.
    """
    http.request("OPTIONS", url.path)
    r = http.getresponse()
    data = r.read()

    if r.status == 200:
        j = json.loads(data)
        features = j["features"]
        return {
            # New imageio never used authentication.
            "needs_auth": False,
            "can_flush": "flush" in features,
            "can_trim": "trim" in features,
            "can_zero": "zero" in features,
            "unix_socket": j.get('unix_socket'),
        }

    elif r.status == 405 or r.status == 204:
        # Old imageio servers returned either 405 Method Not Allowed or
        # 204 No Content (with an empty body).
        return {
            # Authentication was required only when using old imageio proxy.
            # Can be removed when dropping support for oVirt < 4.2.
            "needs_auth": not params['rhv_direct'],
            "can_flush": False,
            "can_trim": False,
            "can_zero": False,
            "unix_socket": None,
        }
    else:
        raise RuntimeError("could not use OPTIONS request: %d: %s" %
                           (r.status, r.reason))

def optimize_http(http, host, options):
    """
    Return an optimized http connection using unix socket if we are connected
    to imageio server on the local host and it features a unix socket.
    """
    unix_socket = options['unix_socket']

    if host is not None and unix_socket is not None:
        try:
            http = UnixHTTPConnection(unix_socket)
        except Exception as e:
            # Very unlikely failure, but we can recover by using the https
            # connection.
            debug("cannot create unix socket connection, using https: %s" % e)
        else:
            debug("optimizing connection using unix socket %r" % unix_socket)

    return http
