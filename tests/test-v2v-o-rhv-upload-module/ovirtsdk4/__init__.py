# -*- python -*-
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

# Fake ovirtsdk4 module used as a test harness.
# See v2v/test-v2v-o-rhv-upload.sh

class Error(Exception):
    pass


class NotFoundError(Error):
    pass


class Connection(object):
    def __init__(
            self,
            url=None,
            username=None,
            password=None,
            ca_file=None,
            log=None,
            insecure=False,
            debug=True,
    ):
        pass

    def close(self):
        pass

    def follow_link(self, objs):
        return objs

    def system_service(self):
        return SystemService()


class SystemService(object):
    def clusters_service(self):
        return ClustersService()

    def data_centers_service(self):
        return DataCentersService()

    def disks_service(self):
        return DisksService()

    def jobs_service(self):
        return JobsService()

    def image_transfers_service(self):
        return ImageTransfersService()

    def storage_domains_service(self):
        return StorageDomainsService()

    def vms_service(self):
        return VmsService()


class ClusterService(object):
    def get(self):
        return types.Cluster()


class ClustersService(object):
    def cluster_service(self, id):
        return ClusterService()


class DataCentersService(object):
    def list(self, search=None, case_sensitive=False):
        return [types.DataCenter()]


class DiskService(object):
    def __init__(self, disk_id):
        self._disk_id = disk_id

    def get(self):
        return types.Disk(id=self._disk_id)

    def remove(self):
        pass


class DisksService(object):
    def add(self, disk=None):
        disk.id = "756d81b0-d5c0-41bc-9bbe-b343c3fa3490"
        return disk

    def disk_service(self, disk_id):
        return DiskService(disk_id)


class JobsService(object):
    def list(self, search=None):
        return [types.Job()]


class ImageTransferService(object):
    def __init__(self):
        self._finalized = False

    def cancel(self):
        pass

    def get(self):
        if self._finalized:
            raise NotFoundError
        else:
            return types.ImageTransfer()

    def finalize(self):
        self._finalized = True


class ImageTransfersService(object):
    def add(self, transfer):
        return transfer

    def image_transfer_service(self, id):
        return ImageTransferService()


class StorageDomainsService(object):
    def list(self, search=None, case_sensitive=False):
        return [StorageDomain()]


class VmsService(object):
    def add(self, vm, query=None):
        return vm

    def list(self, search=None):
        return []
