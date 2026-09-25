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

vmdk=test-ova.vmdk
ovf=test-i-ova.ovf
mf=test-ova.mf
ova=test-ova.ova
raw=TestOva-sda

# Convert the guest to VMDK once.
d=test-i-ova-chunked.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d
qemu-img convert $f -O vmdk $d/orig.vmdk
size=`stat -c %s $d/orig.vmdk`

# Build an OVA whose disk is split into three chunks named
# <href>.NNNNNNNNN as described in DSP0243, then convert it.
#
# $1 = chunk size in bytes
# $2 = the debug message which shows how virt-v2v presented the chunks
run_case ()
{
    local chunk=$1 expect="$2"
    local c=$d/case-$chunk
    mkdir $c

    split -b $chunk -d -a 9 $d/orig.vmdk $c/$vmdk.
    test `ls $c/$vmdk.* | wc -l` -eq 3

    # Reuse the ordinary test OVF, adding ovf:chunkSize to the <File>.
    sed -e "s,<File ovf:href=\"$vmdk\",& ovf:chunkSize=\"$chunk\"," \
        < "$srcdir/$ovf" > $c/$ovf
    grep -q "ovf:chunkSize=\"$chunk\"" $c/$ovf

    # The manifest lists each chunk by name (as VMware does).
    sha1=`do_sha1 $c/$ovf`
    echo "SHA1($ovf)= $sha1" > $c/$mf
    for f in $c/$vmdk.*; do
        sha256=`do_sha256 $f`
        echo "SHA256(`basename $f`)= $sha256" >> $c/$mf
    done

    ( cd $c && tar -cf $ova $ovf $mf $vmdk.* && rm -f $ovf $mf $vmdk.* )

    $VG virt-v2v --debug-gc -v \
        -i ova $c/$ova \
        -o local -of raw -os $c > $c/log 2>&1 || { cat $c/log; exit 1; }
    cat $c/log

    # If the chunks were not reassembled correctly, virt-v2v fails
    # above ("no root device found") because chunk 0 holds the
    # partition table.
    test -f $c/$raw
    test -s $c/$raw
    test -f $c/TestOva.xml

    # Check the chunks were handled the way we intended.
    grep -q "$expect" $c/log
}

# Chunks which are a whole number of 512 byte sectors can be presented
# to qemu without copying.
aligned=$(( ((size + 2) / 3 + 511) / 512 * 512 ))
run_case $aligned "presenting chunked disk .* without copying"

# DSP0243 does not require chunks to be sector aligned (nor does the
# 8 GiB - 1 USTAR limit), in which case they have to be concatenated.
unaligned=$(( aligned + 1 ))
run_case $unaligned "concatenating .* chunks of disk"
