#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2026 Red Hat Inc.
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
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# Test that --key ...:error rejects encrypted partitions.

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/fedora-luks-on-lvm.img
requires test -f $f

if $VG virt-v2v --debug-gc -i disk $f -o null \
       --key all:error ; then
    echo "FAIL: --key all:error succeeded but should have failed"
    exit 1
fi

if $VG virt-v2v --debug-gc -i disk $f -o null \
       --key /dev/Volume-Group/Root:error \
       --key /dev/Volume-Group/Logical-Volume-1:key:FEDORA-LV1 \
       --key /dev/Volume-Group/Logical-Volume-2:key:FEDORA-LV2 \
       --key /dev/Volume-Group/Logical-Volume-3:key:FEDORA-LV3; then
    echo "FAIL: --key .../Root:error succeeded but should have failed"
    exit 1
fi
