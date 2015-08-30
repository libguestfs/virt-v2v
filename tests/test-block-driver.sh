#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2023 Virtuozzo International GmbH
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

# Test --block-driver option.

source ./functions.sh
set -e
set -x

skip_if_skipped
img_base="$abs_top_builddir/test-data/phony-guests/windows.img"
requires test -s $img_base
requires bash -c 'virt-v2v --machine-readable | grep -sq block-driver-option'

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=$PWD/test-block-driver.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

viostor_img="$d/viostor.qcow2"
vioscsi_img="$d/vioscsi.qcow2"
qemu-img create -f qcow2 -b $img_base -o compat=1.1,backing_fmt=raw $viostor_img
qemu-img create -f qcow2 -b $img_base -o compat=1.1,backing_fmt=raw $vioscsi_img
base_md5="$(do_md5 $img_base)"

viostor_xml="$d/windows-viostor.xml"
viostor_name=windows-viostor
cat > $viostor_xml <<EOF
<node>
  <domain type='test'>
    <name>$viostor_name</name>
    <memory>1048576</memory>
    <os>
      <type>hvm</type>
      <boot dev='hd'/>
    </os>
    <devices>
      <disk type='file' device='disk'>
        <driver name='qemu' type='qcow2'/>
        <source file='$viostor_img'/>
        <target dev='vda' bus='virtio'/>
      </disk>
    </devices>
  </domain>
</node>
EOF

vioscsi_xml="$d/windows-vioscsi.xml"
vioscsi_name=windows-vioscsi
cat > $vioscsi_xml <<EOF
<node>
  <domain type='test'>
    <name>$vioscsi_name</name>
    <memory>1048576</memory>
    <os>
      <type>hvm</type>
      <boot dev='hd'/>
    </os>
    <devices>
      <disk type='file' device='disk'>
        <driver name='qemu' type='qcow2'/>
        <source file='$vioscsi_img'/>
        <target dev='sda' bus='scsi'/>
      </disk>
      <controller type='scsi' index='0' model='virtio-scsi'>
        <address type='pci' domain='0x0000' bus='0x00' slot='0x01' function='0x0'/>
      </controller>
    </devices>
  </domain>
</node>
EOF

response="$d/response"
expected="$d/expected"

echo -e "true\ntrue\ntrue\ntrue" >$expected

check_driver_presence ()
{
    local img="$1" drv="$2"

    local virtio_dir="/Windows/Drivers/VirtIO"

    guestfish >$response <<-EOM
        add-ro $img
        set-program virt-testing
        run
        mount-ro /dev/sda2 /
        is-dir $virtio_dir
        is-file $virtio_dir/$drv.cat
        is-file $virtio_dir/$drv.inf
        is-file $virtio_dir/$drv.sys
EOM

    diff -u $expected $response
}

# Test that --block-driver option doesn't break basic "-i disk" conversion
$VG virt-v2v --debug-gc --block-driver virtio-scsi \
    -i disk $img_base -o local -os $d

# Check that libvirt XML metadata and a disk was created.
test -f $d/windows.xml
test -f $d/windows-sda

# Test in-place conversion with --block-driver specified
#
# Note that v2v actually copies all the drivers present in $VIRTIO_WIN, no
# matter which block driver we specify.  The real difference comes from the
# PCI ID values written into the registry.  Since we can't really test those
# values, we're doing the best we can here, i.e. testing that conversion
# doesn't break and drivers are still present with --block-driver option
# being set explicitly.

# Check that in-place conversion works with --block-driver virtio-blk
$VG virt-v2v-in-place --debug-gc --block-driver virtio-blk \
    -i libvirt -ic "test://$viostor_xml" $viostor_name

check_driver_presence $viostor_img "viostor"

# Check that in-place conversion works with --block-driver virtio-scsi
$VG virt-v2v-in-place --debug-gc --block-driver virtio-scsi \
    -i libvirt -ic "test://$vioscsi_xml" $vioscsi_name

check_driver_presence $vioscsi_img "vioscsi"

# Finally check that the base image remained untouched
test "$base_md5" = "$(do_md5 $img_base)"
