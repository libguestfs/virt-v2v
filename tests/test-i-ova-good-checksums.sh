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

# Test -i ova option with good checksums.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/windows.img

if [ ! -f windows.vmdk -o ! -s windows.vmdk ]; then
    echo "$0: test skipped because windows.vmdk was not created"
    exit 77
fi

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-i-ova-good-checksums.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

pushd $d

# Create the test OVA.
cp "$abs_srcdir/test-i-ova-checksums.ovf" test.ovf
cp ../windows.vmdk disk.vmdk

# Test all types of checksum supported by the OVA format.
echo "SHA1(test.ovf)=" `do_sha1 test.ovf` > test.mf
echo "SHA256(disk.vmdk)=" `do_sha256 disk.vmdk` >> test.mf

tar cf test.ova test.ovf disk.vmdk test.mf

# Run virt-v2v but only as far as the --print-source stage.
# It should succeed with no warnings.
if ! $VG virt-v2v --debug-gc --quiet \
       -i ova test.ova \
       --print-source >test.out 2>&1; then
    cat test.out
    exit 1
fi
cat test.out

if grep -sq "warning: " test.out; then
    echo "$0: unexpected warning in virt-v2v output: see messages above"
    exit 1
fi

popd
