#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2018-2024 Red Hat Inc.
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

# Test -o kubevirt using phony Fedora guest.

set -e
set -x

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/fedora.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
fedora=../test-data/phony-guests/fedora.img

d=test-v2v-o-kubevirt-fedora.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

# Run virt-v2v -o kubevirt.
$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" fedora \
    -o kubevirt -on test -os $d

cat $d/test.yaml

# check the disk was created.
test -f $d/test-sda

# Remove some parts of the yaml which change between runs.
sed -i \
    -e 's,^\(# generated by virt-v2v\).*,\1,' \
    -e 's,\(path: \).*/\(test-sda\),\1\2,' \
    -e 's,\(virt-v2v-version: \).*,\1,' \
    $d/test.yaml

# Compare yaml to the expected output.
diff -u test-v2v-o-kubevirt-fedora.yaml.expected $d/test.yaml
