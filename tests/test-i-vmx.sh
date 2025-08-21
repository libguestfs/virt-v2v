#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2017-2025 Red Hat Inc.
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

# Test -i vmx option.
#
# This test is fairly terrible.  It doesn't test SSH at all (which has
# been broken since 1.42).  It doesn't test copying at all.

source ./functions.sh
set -e
set -x

skip_if_skipped

export LANG=C

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

rm -f test-i-vmx-*.actual

# For the tests to succeed we need at least the fileName (VMDK input
# files) to exist.

fns="BZ1308535_21disks.vmdk Fedora-20.vmdk RHEL-7.1-UEFI.vmdk Windows-7-x64.vmdk MSEdge-Win10_preview.vmdk nvme-disk.vmdk"
for fn in BZ1308535_21disks_{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}.vmdk; do
    fns="$fns $fn"
done
fns="$fns win2019.vmdk win2019_1.vmdk"

for fn in $fns; do qemu-img create -f vmdk $fn 512; done

for i in 1 2 3 4 5 6 7; do
    $VG virt-v2v --debug-gc \
        -i vmx test-i-vmx-$i.vmx \
        --print-source > test-i-vmx-$i.actual

    # Normalize the print-source output.
    mv test-i-vmx-$i.actual test-i-vmx-$i.actual.old
    sed \
        -e "s,.*Setting up the source.*,," \
        -e "s,.*Opening the source.*,," \
        -e "s,$(pwd),," \
        < test-i-vmx-$i.actual.old > test-i-vmx-$i.actual
    rm test-i-vmx-$i.actual.old

    # Check the output.
    diff -u "$srcdir/test-i-vmx-$i.expected" test-i-vmx-$i.actual
done

rm test-i-vmx-*.actual
for fn in $fns; do rm $fn; done
