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

# Test virt-v2v (Phony) Windows conversion with UEFI.

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/win2k22-uefi.img
requires test -s $f

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=test-windows-conversion-uefi.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc -vx \
    -i disk $f \
    -o local -os $d

# Test the libvirt XML metadata and a disk was created.
xml=$d/win2k22-uefi.xml
test -f $xml
test -f $d/win2k22-uefi-sda

cat $xml

# Check that it's really UEFI.
grep 'firmware.*efi' $xml
