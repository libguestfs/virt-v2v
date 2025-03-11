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

# Test -i ova option with a directory.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/windows.img
requires test -f $f
requires test -s $f

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=test-i-ova-directory.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

vmdk=test-ova.vmdk
ovf=test-i-ova.ovf
mf=test-ova.mf

qemu-img convert $f -O vmdk $d/$vmdk
cp "$srcdir/$ovf" $d/$ovf
sha1=`do_sha1 $d/$ovf`
echo "SHA1($ovf)= $sha1" > $d/$mf
sha256=`do_sha256 $d/$vmdk`
echo "SHA256($vmdk)= $sha256" >> $d/$mf

$VG virt-v2v --debug-gc \
    -i ova $d \
    -o null
