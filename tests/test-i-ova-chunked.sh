#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2025 Red Hat Inc.
# Copyright (C) 2026 Erico Mendonca <erico.mendonca@gmail.com>
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

# Test -i ova option with an OVA file containing a DSP0243 chunked
# disk (ovf:chunkSize present).  Unlike a VMware snapshot chain
# (test-i-ova-snapshots.sh), all of the chunks must be concatenated to
# reconstruct the disk, otherwise the partition table and boot sector
# (which live in chunk 0) are lost and inspection finds no OS.

unset CDPATH
export LANG=C

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/windows.img
requires test -f $f
requires test -s $f

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=test-i-ova-chunked.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

vmdk=test-ova.vmdk
ovf=test-i-ova.ovf
mf=test-ova.mf
ova=test-ova.ova
raw=TestOva-sda

pushd $d

# Convert the guest to VMDK and split it into three chunks named
# <href>.NNNNNNNNN as described in DSP0243.
qemu-img convert $abs_builddir/$f -O vmdk $vmdk
size=`stat -c %s $vmdk`
chunk=$(( (size + 2) / 3 ))
split -b $chunk -d -a 9 $vmdk $vmdk.
rm $vmdk
test `ls $vmdk.* | wc -l` -eq 3

# Reuse the ordinary test OVF, adding ovf:chunkSize to the <File>.
sed -e "s,<File ovf:href=\"$vmdk\",& ovf:chunkSize=\"$chunk\"," \
    < "$abs_srcdir/$ovf" > $ovf
grep -q "ovf:chunkSize=\"$chunk\"" $ovf

# The manifest lists each chunk by name (as VMware does).
sha1=`do_sha1 $ovf`
echo "SHA1($ovf)= $sha1" > $mf
for c in $vmdk.*; do
    sha256=`do_sha256 $c`
    echo "SHA256($c)= $sha256" >> $mf
done

tar -cf $ova $ovf $mf $vmdk.*
rm -rf $ovf $mf $vmdk.*

popd

$VG virt-v2v --debug-gc \
    -i ova $d/$ova \
    -o local -of raw -os $d

# If the chunks were not reassembled correctly, virt-v2v fails above
# ("no root device found") because chunk 0 holds the partition table.
test -f $d/$raw
test -s $d/$raw
test -f $d/TestOva.xml
