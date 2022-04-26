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

import os
from enum import Enum

imageio_port = os.getenv("IMAGEIO_PORT")
assert imageio_port is not None


class Architecture(Enum):
    UNDEFINED = "undefined"
    X86_64 = "x86_64"

    def __init__(self, arch):
        self._arch = arch

    def __str__(self):
        return self._arch


class Cpu(object):
    architecture = Architecture.X86_64


class Cluster(object):
    id = "2e97537b-a783-4706-af9e-75cb2e032dcd"
    name = "Default"
    cpu = Cpu()


class Configuration(object):
    def __init__(self, type=None, data=None):
        pass


class ConfigurationType(Enum):
    OVA = 'ova'
    OVF = 'ovf'

    def __init__(self, image):
        self._image = image

    def __str__(self):
        return self._image


class DiskFormat(Enum):
    COW = "cow"
    RAW = "raw"

    def __init__(self, image):
        self._image = image

    def __str__(self):
        return self._image


class DiskStatus(Enum):
    ILLEGAL = "illegal"
    LOCKED = "locked"
    OK = "ok"

    def __init__(self, image):
        self._image = image

    def __str__(self):
        return self._image


class Disk(object):
    def __init__(
            self,
            id=None,
            name=None,
            description=None,
            format=None,
            initial_size=None,
            provisioned_size=None,
            sparse=False,
            storage_domains=None
    ):
        self.id = id

    status = DiskStatus.OK


class ImageTransferPhase(Enum):
    CANCELLED = 'cancelled'
    FINALIZING_FAILURE = 'finalizing_failure'
    FINALIZING_SUCCESS = 'finalizing_success'
    FINISHED_FAILURE = 'finished_failure'
    FINISHED_SUCCESS = 'finished_success'
    INITIALIZING = 'initializing'
    PAUSED_SYSTEM = 'paused_system'
    PAUSED_USER = 'paused_user'
    RESUMING = 'resuming'
    TRANSFERRING = 'transferring'
    UNKNOWN = 'unknown'

    def __init__(self, image):
        self._image = image

    def __str__(self):
        return self._image


class ImageTransfer(object):
    def __init__(
            self,
            disk=None,
            host=None,
            inactivity_timeout=None,
    ):
        pass

    id = "e26ac8ab-7090-4d5e-95ad-e707b511a359"
    phase = ImageTransferPhase.TRANSFERRING
    transfer_url = "http://localhost:" + imageio_port + "/"


class Initialization(object):
    def __init__(self, configuration):
        pass


class JobStatus(Enum):
    ABORTED = "aborted"
    FAILED = "failed"
    FINISHED = "finished"
    STARTED = "started"
    UNKNOWN = "unknown"

    def __init__(self, image):
        self._image = image

    def __str__(self):
        return self._image


class Job(object):
    description = "Fake job"
    status = JobStatus.FINISHED


class StorageDomain(object):
    def __init__(self, name=None):
        pass

    id = "ba87af68-b630-4211-a73a-694c1a689405"
    name = "Storage"


class Vm(object):
    def __init__(
            self,
            cluster=None,
            initialization=None
    ):
        pass


class DataCenter(object):
    id = "31d8c73b-554b-4958-bb04-9ce97f0849e1"
    name = "DC"
    storage_domains = [StorageDomain()]
    clusters = [Cluster()]
