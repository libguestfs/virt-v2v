#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014 Red Hat Inc.
# Copyright (C) 2015 Parallels IP Holdings GmbH.
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

# Test virt-v2v-in-place with the <checksum> section.

unset CDPATH
export LANG=C
set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -s ../test-data/phony-guests/windows.img

img_base="$abs_top_builddir/test-data/phony-guests/windows.img"

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=$PWD/test-checksum-print.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

img="$d/test.img"
rm -f $img
cp $img_base $img
md5="$(do_md5 $img)"

out="$d/out"

libvirt_xml="$d/test.xml"
rm -f $libvirt_xml
cat > $libvirt_xml <<EOF
<domain type='kvm'>
  <name>windows</name>
  <memory>1048576</memory>
  <os>
    <type>hvm</type>
    <boot dev='hd'/>
  </os>
  <devices>
    <disk type='file' device='disk'>
      <driver name='qemu' type='raw'/>
      <source file='$img'/>
      <target dev='vda' bus='virtio'/>
      <checksum method='md5' fail='print'/>
    </disk>
  </devices>
</domain>
EOF

$VG virt-v2v-in-place --debug-gc -i libvirtxml $libvirt_xml > $out
cat $out

# The checksum must appear in the output.
grep $md5 $out
