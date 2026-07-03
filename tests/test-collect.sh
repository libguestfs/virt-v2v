#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2024-2026 Red Hat Inc.
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

# Test virt-v2v --collect and --collect-output options.

source ./functions.sh
set -e
set -x

skip_if_skipped
windows=../test-data/phony-guests/windows.img
requires test -s $windows

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-collect.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

output=$d/output.tar.xz
files=$d/output.tar.xz.files

$VG virt-v2v --debug-gc \
    -i disk $windows \
    -o null \
    --collect /windows/system32/config:windows/system32 \
    --collect-output $output

# Test the tarball was created and contains some expected files/dirs.
file $output

tar -Jtf $output > $files
cat $files

grep -i windows/system32/config $files
grep -i windows/system32/config/SYSTEM $files
