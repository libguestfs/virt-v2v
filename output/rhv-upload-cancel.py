# -*- python -*-
# oVirt or RHV upload cancel used by ‘virt-v2v -o rhv-upload’
# Copyright (C) 2019-2021 Red Hat Inc.
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
import logging
import sys
from contextlib import closing
from urllib.parse import urlparse, urlunparse

import ovirtsdk4 as sdk
import ovirtsdk4.types as types


def debug(s):
    if params['verbose']:
        print(s, file=sys.stderr)
        sys.stderr.flush()


# Parameters are passed in via a JSON doc from the OCaml code.
# Because this Python code ships embedded inside virt-v2v there
# is no formal API here.
params = None

if len(sys.argv) != 2:
    raise RuntimeError("incorrect number of parameters")

# Parameters are passed in via a JSON document.
with open(sys.argv[1], 'r') as fp:
    params = json.load(fp)

# What is passed in is a password file, read the actual password.
with open(params['output_password'], 'r') as fp:
    output_password = fp.read()
output_password = output_password.rstrip()

# Parse out the username from the output_conn URL.
parsed = urlparse(params['output_conn'])
username = parsed.username or "admin@internal"
netloc = f"{parsed.hostname:parsed.port}" if parsed.port else parsed.hostname

# Connect to the server.
connection = sdk.Connection(
    url=urlunparse(parsed._replace(netloc=netloc)),
    username=username,
    password=output_password,
    ca_file=params['rhv_cafile'],
    log=logging.getLogger(),
    insecure=params['insecure'],
)

with closing(connection):
    system_service = connection.system_service()
    image_transfers_service = system_service.image_transfers_service()

    # Try to cancel the transfers.  This should delete the associated disk.
    for id in params['transfer_ids']:
        try:
            transfer_service = \
                image_transfers_service.image_transfer_service(id)
            transfer_service.cancel()
        except sdk.NotFoundError:
            debug("unexpected error: transfer id %s not found" % id)
        except Exception:
            if params['verbose']:
                traceback.print_exc()

    disks_service = system_service.disks_service()

    # In case we didn't associate a disk with a transfer and as a last
    # resort, delete the disk too.
    for uuid in params['disk_uuids']:
        try:
            disk_service = disks_service.disk_service(uuid)
            disk_service.remove()
        except (sdk.NotFoundError, sdk.Error):
            # We expect these exceptions so ignore them.
            pass
        except Exception:
            if params['verbose']:
                traceback.print_exc()
