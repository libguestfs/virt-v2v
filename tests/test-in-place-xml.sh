#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2025 Red Hat Inc.
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

# Test virt-v2v-in-place -O option.

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
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win"

d=$PWD/test-v2v-in-place-xml.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

img="$d/test.qcow2"
qemu-img convert -f raw $img_base -O qcow2 $img

out="$d/out.xml"

libvirt_xml="$d/test.xml"
rm -f $libvirt_xml
n=windows
cat > $libvirt_xml <<EOF
<node>
  <domain type='test'>
    <name>$n</name>
    <memory>1048576</memory>
    <os>
      <type>hvm</type>
      <boot dev='hd'/>
    </os>
    <devices>
      <disk type='file' device='disk'>
        <driver name='qemu' type='qcow2'/>
        <source file='$img'/>
        <target dev='vda' bus='virtio'/>
      </disk>
    </devices>
  </domain>
</node>
EOF

$VG virt-v2v-in-place --debug-gc -i libvirt -ic "test://$libvirt_xml" \
    $n -O $out
cat $out

# Expect certain elements to be present.
grep '^<v2v-inspection' $out
grep '<program>virt-v2v-inspector</program>' $out
grep '<disks>' $out
grep "<disk index='0'>" $out
grep '<distro>windows</distro>' $out
grep '<osinfo>win2k22</osinfo>' $out
