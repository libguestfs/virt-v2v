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

# Test virt-v2v-show-roots with an encrypted guest.

unset CDPATH
export LANG=C

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/fedora-luks-on-lvm.img
requires test -f $f

requires virt-inspector --version

d=$PWD/test-open-encrypted.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

out="$d/out"

keys="--key /dev/Volume-Group/Root:key:FEDORA-Root \
      --key /dev/Volume-Group/Logical-Volume-1:key:FEDORA-LV1 \
      --key /dev/Volume-Group/Logical-Volume-2:key:FEDORA-LV2 \
      --key /dev/Volume-Group/Logical-Volume-3:key:FEDORA-LV3"

$VG virt-v2v-open --debug-gc -i disk $f \
    --run "virt-inspector --format=raw @@ $keys > $out"
cat $out

# Expect certain elements to be present.
grep '^<operatingsystems>' $out
grep '<operatingsystem>' $out
grep '<root>/dev/mapper/luks-' $out
grep '<distro>fedora</distro>' $out
grep '<osinfo>fedora14</osinfo>' $out
