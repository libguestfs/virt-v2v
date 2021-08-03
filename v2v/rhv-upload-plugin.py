# -*- python -*-
# oVirt or RHV upload nbdkit plugin used by ‘virt-v2v -o rhv-upload’
# Copyright (C) 2018-2021 Red Hat Inc.
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

import json
import queue
import socket
import ssl

from contextlib import contextmanager
from http.client import HTTPSConnection, HTTPConnection
from urllib.parse import urlparse

import nbdkit

# Using version 2 supporting the buffer protocol for better performance.
API_VERSION = 2

# Maximum number of connection to imageio server. Based on testing with imageio
# client, this give best performance.
MAX_CONNECTIONS = 4

# Required parameters.
size = None
url = None

# Optional parameters.
cafile = None
insecure = False
is_ovirt_host = False

# List of options read from imageio server.
options = None

# Pool of HTTP connections.
pool = None


# Parse parameters.
def config(key, value):
    global cafile, url, is_ovirt_host, insecure, size

    if key == "cafile":
        cafile = value
    elif key == "insecure":
        insecure = value.lower() in ['true', '1']
    elif key == "is_ovirt_host":
        is_ovirt_host = value.lower() in ['true', '1']
    elif key == "size":
        size = int(value)
    elif key == "url":
        url = urlparse(value)
    else:
        raise RuntimeError("unknown configuration key '%s'" % key)


def config_complete():
    # These parameters are required.
    if url is None:
        raise RuntimeError("url parameter was not set")
    if size is None:
        raise RuntimeError("size parameter was not set")


def after_fork():
    global options, pool

    http = create_http(url)
    options = get_options(http, url)
    http.close()

    nbdkit.debug("imageio features: flush=%(can_flush)r "
                 "zero=%(can_zero)r unix_socket=%(unix_socket)r "
                 "max_readers=%(max_readers)r max_writers=%(max_writers)r"
                 % options)

    pool = create_http_pool(url, options)


# This function is not actually defined before nbdkit 1.28, but it
# doesn't particularly matter if we don't close the pool because
# clients should call flush().
def cleanup():
    close_http_pool(pool)


def thread_model():
    """
    Using parallel model to speed up transfer with multiple connections to
    imageio server.
    """
    return nbdkit.THREAD_MODEL_PARALLEL


def open(readonly):
    return 1


def can_trim(h):
    return False


def can_flush(h):
    return options['can_flush']


def can_fua(h):
    # imageio flush feature is is compatible with NBD_CMD_FLAG_FUA.
    return options['can_flush']


def get_size(h):
    return size


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
    nbdkit.debug("unexpected response from imageio server:")
    nbdkit.debug(msg)
    nbdkit.debug("%d: %s" % (status, reason))
    nbdkit.debug(body)

    # Only a short error is included in the exception.
    raise RuntimeError("%s: %d %s: %r" % (msg, status, reason, body[:200]))


# For documentation see:
# https://github.com/oVirt/ovirt-imageio/blob/master/docs/random-io.md
# For examples of working code to read/write from the server, see:
# https://github.com/oVirt/ovirt-imageio/blob/master/daemon/test/server_test.py
def pread(h, buf, offset, flags):
    count = len(buf)
    headers = {"Range": "bytes=%d-%d" % (offset, offset + count - 1)}

    with http_context(pool) as http:
        http.request("GET", url.path, headers=headers)

        r = http.getresponse()
        # 206 = HTTP Partial Content.
        if r.status != 206:
            request_failed(r,
                           "could not read sector offset %d size %d" %
                           (offset, count))

        content_length = int(r.getheader("content-length"))
        if content_length != count:
            # Should never happen.
            request_failed(r,
                           "unexpected Content-Length offset %d size %d got %d" %
                           (offset, count, content_length))

        with memoryview(buf) as view:
            got = 0
            while got < count:
                n = r.readinto(view[got:])
                if n == 0:
                    request_failed(r,
                                   "short read offset %d size %d got %d" %
                                   (offset, count, got))
                got += n


def pwrite(h, buf, offset, flags):
    count = len(buf)

    flush = "y" if (options['can_flush'] and (flags & nbdkit.FLAG_FUA)) else "n"

    with http_context(pool) as http:
        http.putrequest("PUT", url.path + "?flush=" + flush)
        # The oVirt server only uses the first part of the range, and the
        # content-length.
        http.putheader("Content-Range", "bytes %d-%d/*" %
                       (offset, offset + count - 1))
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


def zero(h, count, offset, flags):
    # Unlike the trim and flush calls, there is no 'can_zero' method
    # so nbdkit could call this even if the server doesn't support
    # zeroing.  If this is the case we must emulate.
    if not options['can_zero']:
        emulate_zero(h, count, offset, flags)
        return

    flush = bool(options['can_flush'] and (flags & nbdkit.FLAG_FUA))

    # Construct the JSON request for zeroing.
    buf = json.dumps({'op': "zero",
                      'offset': offset,
                      'size': count,
                      'flush': flush}).encode()

    headers = {"Content-Type": "application/json",
               "Content-Length": str(len(buf))}

    with http_context(pool) as http:
        http.request("PATCH", url.path, body=buf, headers=headers)

        r = http.getresponse()
        if r.status != 200:
            request_failed(r,
                           "could not zero sector offset %d size %d" %
                           (offset, count))

        r.read()


def emulate_zero(h, count, offset, flags):
    flush = "y" if (options['can_flush'] and (flags & nbdkit.FLAG_FUA)) else "n"

    with http_context(pool) as http:
        http.putrequest("PUT", url.path + "?flush=" + flush)
        http.putheader("Content-Range",
                       "bytes %d-%d/*" % (offset, offset + count - 1))
        http.putheader("Content-Length", str(count))
        http.endheaders()

        try:
            buf = bytearray(128 * 1024)
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


def flush(h, flags):
    # Construct the JSON request for flushing.
    buf = json.dumps({'op': "flush"}).encode()

    headers = {"Content-Type": "application/json",
               "Content-Length": str(len(buf))}

    # Wait until all inflight requests are completed, and send a flush
    # request for all imageio connections.
    for http in iter_http_pool(pool):
        http.request("PATCH", url.path, body=buf, headers=headers)

        r = http.getresponse()
        if r.status != 200:
            request_failed(r, "could not flush")

        r.read()


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

# Connection pool.
def create_http_pool(url, options):
    pool = queue.Queue()

    count = min(options["max_readers"],
                options["max_writers"],
                MAX_CONNECTIONS)

    nbdkit.debug("creating http pool connections=%d" % count)

    unix_socket = options["unix_socket"] if is_ovirt_host else None

    for i in range(count):
        http = create_http(url, unix_socket=unix_socket)
        pool.put(http)

    return pool


@contextmanager
def http_context(pool):
    """
    Context manager yielding an imageio http connection from the pool. Blocks
    until a connection is available.
    """
    http = pool.get()
    try:
        yield http
    finally:
        pool.put(http)


def iter_http_pool(pool):
    """
    Wait until all inflight requests are done, and iterate on imageio
    connections.

    The pool is empty during iteration. New requests issued during iteration
    will block until iteration is done.
    """
    locked = []

    # Lock the pool by taking the connection out.
    while len(locked) < pool.qsize():
        locked.append(pool.get())

    try:
        for http in locked:
            yield http
    finally:
        # Unlock the pool by puting the connection back.
        for http in locked:
            pool.put(http)


def close_http_pool(pool):
    """
    Wait until all inflight requests are done, close all connections and remove
    them from the pool.

    No request can be served by the pool after this call.
    """
    nbdkit.debug("closing http pool")

    locked = []

    while len(locked) < pool.qsize():
        locked.append(pool.get())

    for http in locked:
        http.close()


def create_http(url, unix_socket=None):
    """
    Create http connection for transfer url.

    Returns HTTPConnection.
    """
    if unix_socket:
        nbdkit.debug("creating unix http connection socket=%r" % unix_socket)
        try:
            return UnixHTTPConnection(unix_socket)
        except Exception as e:
            # Very unlikely, but we can recover by using https.
            nbdkit.debug("cannot create unix socket connection: %s" % e)

    if url.scheme == "https":
        context = \
            ssl.create_default_context(purpose=ssl.Purpose.SERVER_AUTH,
                                       cafile=cafile)
        if insecure:
            context.check_hostname = False
            context.verify_mode = ssl.CERT_NONE

        nbdkit.debug("creating https connection host=%s port=%s" %
                     (url.hostname, url.port))
        return HTTPSConnection(url.hostname, url.port, context=context)
    elif url.scheme == "http":
        nbdkit.debug("creating http connection host=%s port=%s" %
                     (url.hostname, url.port))
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
            "can_flush": "flush" in features,
            "can_zero": "zero" in features,
            "unix_socket": j.get('unix_socket'),
            "max_readers": j.get("max_readers", 1),
            "max_writers": j.get("max_writers", 1),
        }

    elif r.status == 405 or r.status == 204:
        # Old imageio servers returned either 405 Method Not Allowed or
        # 204 No Content (with an empty body).
        return {
            "can_flush": False,
            "can_zero": False,
            "unix_socket": None,
            "max_readers": 1,
            "max_writers": 1,
        }
    else:
        raise RuntimeError("could not use OPTIONS request: %d: %s" %
                           (r.status, r.reason))
