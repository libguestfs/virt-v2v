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

# Test -o local -of qcow2 -oo compressed.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -s ../test-data/phony-guests/windows.img

requires nbdcopy --version

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-o-local-qcow2-compressed.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i disk ../test-data/phony-guests/windows.img \
    -o local -of qcow2 -oo compressed -os $d

# Test the libvirt XML metadata and a disk was created.
ls -l $d
test -f $d/windows.xml
test -f $d/windows-sda
