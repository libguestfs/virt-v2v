#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2020 Red Hat Inc.
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

# Test -i ova option with ova file compressed in different ways

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires zip --version
requires unzip --help

formats="zip tar-gz tar-xz"

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-i-ova-formats.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

pushd $d

# Create a phony OVA.  This is only a test of source parsing, not
# conversion, so the contents of the disks doesn't matter.
guestfish disk-create disk1.vmdk raw 10K
sha=`do_sha1 disk1.vmdk`
echo -e "SHA1(disk1.vmdk)= $sha\r" > disk1.mf
cp "$abs_srcdir/test-i-ova-formats.ovf" .

for format in $formats; do
    case "$format" in
        zip)
            zip -r test test-i-ova-formats.ovf disk1.vmdk disk1.mf
            mv test.zip test-$format.ova
            ;;
        tar-gz)
            tar -czf test-$format.ova test-i-ova-formats.ovf disk1.vmdk disk1.mf
            ;;
        tar-xz)
            tar -cJf test-$format.ova test-i-ova-formats.ovf disk1.vmdk disk1.mf
            ;;
        *)
            echo "Unhandled format '$format'"
            exit 1
    esac
done

popd

for format in $formats; do
    # Run virt-v2v but only as far as the --print-source stage, and
    # normalize the output.
    $VG virt-v2v --debug-gc --quiet \
        -i ova $d/test-$format.ova \
        --print-source |
    sed 's,[^ \t]*\(disk.*.vmdk\),\1,' > $d/source

    # Check the parsed source is what we expect.
    diff -u "$srcdir/test-i-ova-formats.expected" $d/source
done
