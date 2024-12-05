#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2024 Red Hat Inc.
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

# Test --parallel option.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
windows=../test-data/phony-guests/windows.img
requires test -f $windows

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-i-disk-parallel.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

truncate -s $((100*1024*1024)) $d/disk-2.img $d/disk-3.img $d/disk-4.img

$VG virt-v2v --debug-gc \
    --parallel=2 \
    -i disk \
    $windows \
    $d/disk-2.img \
    $d/disk-3.img \
    $d/disk-4.img \
    -o local -os $d

# Test the libvirt XML metadata and output disks were created.
test -f $d/windows.xml
test -f $d/windows-sda
test -f $d/windows-sdb
test -f $d/windows-sdc
test -f $d/windows-sdd