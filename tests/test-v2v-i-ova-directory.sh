#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2021 Red Hat Inc.
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

# Test -i ova option with a directory.

set -e

$TEST_FUNCTIONS
skip_if_skipped
skip_if_backend uml
skip_unless_phony_guest windows.img

skip_unless_libvirt_minimum_version 3 1 0

export VIRT_TOOLS_DATA_DIR="$top_srcdir/test-data/fake-virt-tools"
export VIRTIO_WIN="$top_srcdir/test-data/fake-virtio-win"

d=test-v2v-i-ova-directory.d
rm -rf $d
mkdir $d

vmdk=test-ova.vmdk
ovf=test-v2v-i-ova.ovf
mf=test-ova.mf

qemu-img convert $top_builddir/test-data/phony-guests/windows.img \
         -O vmdk $d/$vmdk
cp "$srcdir/$ovf" $d/$ovf
sha1=`do_sha1 $d/$ovf`
echo "SHA1($ovf)= $sha1" > $d/$mf
sha256=`do_sha256 $d/$vmdk`
echo "SHA256($vmdk)= $sha256" >> $d/$mf

$VG virt-v2v --debug-gc \
    -i ova $d \
    -o null

rm -rf $d
