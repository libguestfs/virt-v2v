#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2015-2025 Red Hat Inc.
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

# Test converting a guest with a floppy disk.
# https://bugzilla.redhat.com/show_bug.cgi?id=1309706

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/windows.img
requires test -f ../test-data/phony-guests/blank-disk.img

libvirt_uri="test://$abs_builddir/test-floppy.xml"

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win"

d=test-floppy.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" windows \
    -o local -os $d

# Test the libvirt XML metadata was created.
test -f $d/windows.xml

# Grab just the <disk>..</disk> output and compare it to what we
# expect.  https://stackoverflow.com/questions/16587218
awk '/<disk /{p=1;print;next} p&&/<\/disk>/{p=0;print;next} ;p' \
    $d/windows.xml |
    grep -v '<source file' > $d/disks

if ! diff -u "$srcdir/test-floppy.expected" $d/disks; then
    echo "$0: unexpected disk assignments"
    cat $d/disks
    exit 1
fi
