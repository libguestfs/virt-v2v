# -*- python -*-
# oVirt or RHV upload finalize used by ‘virt-v2v -o rhv-upload’
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
import logging
import sys
import time
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


def finalize_transfer(connection, transfer_id, disk_id):
    """
    Finalize a transfer, making the transfer disk available.

    If finalizing succeeds, the transfer's disk status will change to OK
    and transfer's phase will change to FINISHED_SUCCESS. Unfortunately,
    the disk status is modified before the transfer finishes, and oVirt
    may still hold a lock on the disk at this point.

    The only way to make sure that the disk is unlocked, is to wait
    until the transfer phase switches FINISHED_SUCCESS. Unfortunately
    oVirt makes this hard to use because the transfer is removed shortly
    after switching the phase to the final phase. However if the
    transfer was removed, we can be sure that the disk is not locked,
    since oVirt releases the locks before removing the transfer.

    On errors, the transfer's phase will change to FINISHED_FAILURE and
    the disk status will change to ILLEGAL and it will be removed. Again
    the transfer will be removed shortly after that.

    If oVirt fails to finalize the transfer, transfer's phase will
    change to PAUSED_SYSTEM. In this case the disk's status will change
    to ILLEGAL and it will not be removed.

    oVirt 4.4.7 made waiting for transfer easier by keeping transfers
    after they complete, but we must support older versions so we have
    generic code that work with any version.

    For more info see:
    - http://ovirt.github.io/ovirt-engine-api-model/4.4/#services/image_transfer
    - http://ovirt.github.io/ovirt-engine-sdk/master/types.m.html#ovirtsdk4.types.ImageTransfer
    """
    debug("finalizing transfer %s" % transfer_id)
    transfer_service = (connection.system_service()
                        .image_transfers_service()
                        .image_transfer_service(transfer_id))

    start = time.monotonic()

    transfer_service.finalize()

    while True:
        time.sleep(1)
        try:
            transfer = transfer_service.get()
        except sdk.NotFoundError:
            # Transfer was removed (ovirt < 4.4.7). We need to check the
            # disk status to understand if the transfer was successful.
            # Due to the way oVirt does locking, we know that the disk
            # is unlocked at this point so we can check only once.

            debug("transfer %s was removed, checking disk %s status"
                  % (transfer_id, disk_id))

            disk_service = (connection.system_service()
                            .disks_service()
                            .disk_service(disk_id))

            try:
                disk = disk_service.get()
            except sdk.NotFoundError:
                raise RuntimeError(
                    "transfer %s failed: disk %s was removed"
                    % (transfer_id, disk_id))

            debug("disk %s is %s" % (disk.id, disk.status))

            if disk.status == types.DiskStatus.OK:
                break

            raise RuntimeError(
                "transfer %s failed: disk is %s" % (transfer_id, disk.status))
        else:
            # Transfer exists, check if it reached one of the final
            # phases, or we timed out.

            debug("transfer %s is %s" % (transfer.id, transfer.phase))

            if transfer.phase == types.ImageTransferPhase.FINISHED_SUCCESS:
                break

            if transfer.phase == types.ImageTransferPhase.FINISHED_FAILURE:
                raise RuntimeError(
                    "transfer %s has failed" % (transfer_id,))

            if transfer.phase == types.ImageTransferPhase.PAUSED_SYSTEM:
                raise RuntimeError(
                    "transfer %s was paused by system" % (transfer.id,))

            if time.monotonic() > start + timeout:
                raise RuntimeError(
                    "timed out waiting for transfer %s to finalize, "
                    "transfer is %s"
                    % (transfer.id, transfer.phase))

    debug("transfer %s finalized in %.3f seconds"
          % (transfer_id, time.monotonic() - start))


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

# Finalize all the transfers.
for (transfer_id, disk_id) in zip(params['transfer_ids'], params['disk_uuids']):
    finalize_transfer(connection, transfer_id, disk_id)

connection.close()
