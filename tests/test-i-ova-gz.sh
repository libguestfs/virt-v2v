#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014 Red Hat Inc.
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

# Test -i ova option with gzip-compressed disks.

unset CDPATH
export LANG=C

source ./functions.sh
set -e
set -x

skip_if_skipped

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-i-ova-gz.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

pushd $d

guestfish disk-create disk1.vmdk raw 10K
gzip disk1.vmdk
sha=`do_sha1 disk1.vmdk.gz`
echo -e "SHA1(disk1.vmdk.gz)= $sha\r" > disk1.mf
cp "$abs_srcdir/test-i-ova-gz.ovf" .

tar -cf test.ova test-i-ova-gz.ovf disk1.vmdk.gz disk1.mf
popd

# Run virt-v2v but only as far as the --print-source stage, and
# normalize the output.
$VG virt-v2v --debug-gc --quiet \
    -i ova $d/test.ova \
    --print-source |
sed 's,[^ \t]*\(\.vmdk\),\1,' > $d/source

# Check the parsed source is what we expect.
diff -u "$srcdir/test-i-ova-gz.expected" $d/source
