#!/bin/bash -
# libguestfs virt-v2v test script
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
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# Test -o kubevirt + -oo disk option.

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/fedora.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
fedora=../test-data/phony-guests/fedora.img

d=test-o-kubevirt-oo-disk.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

# Run virt-v2v -o kubevirt.
$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" fedora \
    -o kubevirt -oo disk=$d/disk.img -os $d

ls -l $d

cat $d/fedora.yaml

# Check the disk was created.
test -f $d/disk.img

# Check the default path was _not_ created.
! test -f $d/fedora-sda
