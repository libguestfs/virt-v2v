#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2025 Red Hat Inc.
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

# Test virt-v2v boot order for multi-disk Windows guests.
#
# Windows is installed on the second disk (sdb).  After conversion,
# the second disk should get boot order 1 in the output XML.

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -s ../test-data/phony-guests/windows-multi-disk-sda.img
requires test -s ../test-data/phony-guests/windows-multi-disk-sdb.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=test-windows-multi-disk-boot-order.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" windows-multi-disk \
    -o local -os $d \
    --root /dev/sdb2

# Test the libvirt XML metadata and disks were created.
test -f $d/windows-multi-disk.xml
test -f $d/windows-multi-disk-sda
test -f $d/windows-multi-disk-sdb

# Check that the second disk (sdb, the boot disk) gets boot order 1
# and the first disk (sda, blank data) does not.

xml=$d/windows-multi-disk.xml

# The sda disk must NOT have boot order 1.
if grep -A5 'windows-multi-disk-sda' "$xml" | grep -q "boot order='1'"; then
    echo "FAIL: disk 1 (sda) should not have boot order 1"
    exit 1
fi

# The sdb disk (Windows root) must have boot order 1.
if ! grep -A5 'windows-multi-disk-sdb' "$xml" | grep -q "boot order='1'"; then
    echo "FAIL: expected boot order 1 for disk 2 (sdb)"
    exit 1
fi
