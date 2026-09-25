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

# Test -i ova option with a DSP0243 chunked disk where one of the
# chunks is missing.  This must be an error, not a silently corrupt
# (truncated) disk.

unset CDPATH
export LANG=C

source ./functions.sh
set -e
set -x

skip_if_skipped

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-i-ova-chunked-gap.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

pushd $d

# Create a phony OVA.  This is only a test of source parsing, not
# conversion, so the contents of the disks doesn't matter.  The OVF
# references disk1.vmdk with ovf:chunkSize, but only the numbered
# chunks disk1.vmdk.NNNNNNNNN exist in the archive.
guestfish disk-create disk1.vmdk.000000000 raw 10k
guestfish disk-create disk1.vmdk.000000001 raw 10k
guestfish disk-create disk1.vmdk.000000002 raw 12k
: > disk1.mf
for c in 000000000 000000001 000000002; do
    sha=`do_sha1 disk1.vmdk.$c`
    echo -e "SHA1(disk1.vmdk.$c)= $sha\r" >> disk1.mf
done
sed -e 's,<File ovf:href="disk1.vmdk" ovf:id="file1" ovf:size="12288"/>,<File ovf:href="disk1.vmdk" ovf:id="file1" ovf:chunkSize="10240" ovf:size="32768"/>,' \
    < "$abs_srcdir/test-i-ova-snapshots.ovf" > test-i-ova-chunked.ovf
grep -q ovf:chunkSize test-i-ova-chunked.ovf
# Leave out chunk 1.
tar -cf test-chunked-gap.ova test-i-ova-chunked.ovf \
    disk1.vmdk.000000000 disk1.vmdk.000000002 disk1.mf

popd

if $VG virt-v2v --debug-gc --quiet \
       -i ova $d/test-chunked-gap.ova \
       --print-source > $d/source 2> $d/err ; then
    echo "$0: virt-v2v should have failed with a missing chunk"
    exit 1
fi

cat $d/err
grep -sq 'missing chunk 1' $d/err
