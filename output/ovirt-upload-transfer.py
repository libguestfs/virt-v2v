# -*- python -*-
# oVirt upload start transfer used by ‘virt-v2v -o ovirt-upload’
# Copyright (C) 2018-2025 Red Hat Inc.
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

import inspect
import json
import logging
import sys
import time
from contextlib import closing
from urllib.parse import urlparse, urlunparse

import ovirtsdk4 as sdk
import ovirtsdk4.types as types

# Timeout to wait for oVirt disks to change status, or the transfer
# object to finish initializing [seconds].
timeout = 5 * 60


def debug(s):
    if params['verbose']:
        print(s, file=sys.stderr)
        sys.stderr.flush()


def find_host(connection):
    """Return the current host object or None."""
    try:
        with open("/etc/vdsm/vdsm.id") as f:
            vdsm_id = f.readline().strip()
    except FileNotFoundError:
        # Expected condition when running on non-oVirt host.
        debug("not an oVirt host, using non-oVirt host")
        return None
    except Exception as e:
        # Unexpected but we can degrade to remote transfer.
        debug(f"warning: cannot read host id, using non-oVirt host: {e}")
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
        debug("storage domain (%s) is not attached to a DC" % storage_name)
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

    return types.Host(id=host.id)


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
        disk=types.Disk(
            id=params['disk_uuid'],
            name=params['disk_name'],
            description="Uploaded by virt-v2v",
            format=disk_format,
            # XXX For qcow2 disk on block storage, we should use the estimated
            # size, based on qemu-img measure of the overlay.
            initial_size=params['disk_size'],
            provisioned_size=params['disk_size'],
            # Handling this properly will be complex, see:
            # https://www.redhat.com/archives/libguestfs/2018-March/msg00177.html
            sparse=True,
            storage_domains=[
                types.StorageDomain(
                    name=params['output_storage'],
                )
            ],
        )
    )

    debug("disk.id = %r" % disk.id)

    # Wait till the disk moved from LOCKED state to OK state, as the transfer
    # can't start if the disk is locked.

    disk_service = disks_service.disk_service(disk.id)
    endt = time.monotonic() + timeout
    while True:
        time.sleep(1)
        disk = disk_service.get()
        if disk.status == types.DiskStatus.OK:
            break
        if time.monotonic() > endt:
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
            disk=types.Disk(id=disk.id),
            host=host,
            inactivity_timeout=3600,
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

    endt = time.monotonic() + timeout
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

        if time.monotonic() > endt:
            transfer_service.cancel()
            raise RuntimeError(
                "timed out waiting for transfer %s" % transfer.id)

    return transfer


def transfer_supports_format():
    """
    Return True if transfer supports the "format" argument, enabling the NBD
    backend on imageio side, which allows uploading to qcow2 images.

    This feature was added in ovirt 4.3. We assume that the SDK version matches
    engine version.
    """
    sig = inspect.signature(types.ImageTransfer)
    return "format" in sig.parameters


def get_transfer_url(transfer):
    """
    Returns the transfer url, preferring direct transfer if possible.
    """
    if params['ovirt_direct']:
        if transfer.transfer_url is None:
            raise RuntimeError("direct upload to host not supported, "
                               "requires ovirt-engine >= 4.2 and only works "
                               "when virt-v2v is run within the oVirt "
                               "environment, eg. on an oVirt node.")
        return transfer.transfer_url
    else:
        return transfer.proxy_url


# Parameters are passed in via a JSON doc from the OCaml code.
# Because this Python code ships embedded inside virt-v2v there
# is no formal API here.
params = None

if len(sys.argv) != 2:
    raise RuntimeError("incorrect number of parameters")

# Parameters are passed in via a JSON document.
with open(sys.argv[1], 'r') as fp:
    data = fp.read()

try:
    params = json.loads(data)
except ValueError as e:
    raise RuntimeError(f"Cannot parse params {data!r}: {e}").with_traceback(
        e.__traceback__
    ) from None

# What is passed in is a password file, read the actual password.
with open(params['output_password'], 'r') as fp:
    output_password = fp.read()
output_password = output_password.rstrip()

# Parse out the username from the output_conn URL.
parsed = urlparse(params['output_conn'])
username = parsed.username or "admin@internal"
netloc = f"{parsed.hostname:parsed.port}" if parsed.port else parsed.hostname

connection = sdk.Connection(
    url=urlunparse(parsed._replace(netloc=netloc)),
    username=username,
    password=output_password,
    ca_file=params['ovirt_cafile'],
    log=logging.getLogger(),
    insecure=params['insecure'],
)

with closing(connection):
    # Use the local host if possible.
    host = find_host(connection) if params['ovirt_direct'] else None
    disk = create_disk(connection)

    transfer = create_transfer(connection, disk, host)
    destination_url = get_transfer_url(transfer)

# Send the destination URL, transfer ID, and host flag back to OCaml code.
results = {
    "transfer_id": transfer.id,
    "destination_url": destination_url,
    "is_ovirt_host": host is not None,
}
json.dump(results, sys.stdout)
