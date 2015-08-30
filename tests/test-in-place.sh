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

# Test virt-v2v-in-place.

unset CDPATH
export LANG=C

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -s ../test-data/phony-guests/windows.img

img_base="$abs_top_builddir/test-data/phony-guests/windows.img"

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=$PWD/test-in-place.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

img="$d/test.qcow2"
rm -f $img
qemu-img create -f qcow2 -b $img_base -o compat=1.1,backing_fmt=raw $img
md5="$(do_md5 $img_base)"

libvirt_xml="$d/test.xml"
rm -f $libvirt_xml
n=windows-overlay
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

$VG virt-v2v-in-place --debug-gc -i libvirt -ic "test://$libvirt_xml" $n

# Test that the drivers have been copied over into the guest
script="$d/test.fish"
expected="$d/expected"
response="$d/response"

mktest ()
{
    local cmd="$1" exp="$2"

    echo "echo '$cmd'" >> "$script"
    echo "$cmd" >> "$expected"

    echo "$cmd" >> "$script"
    echo "$exp" >> "$expected"
}

:> "$script"
:> "$expected"

cat >> "$script" <<EOF
  set-program virt-testing
  run
  mount /dev/sda2 /
EOF

firstboot_dir="/Program Files/Guestfs/Firstboot"
mktest "is-dir \"$firstboot_dir\"" true
mktest "is-file \"$firstboot_dir/firstboot.bat\"" true
mktest "is-dir \"$firstboot_dir/scripts\"" true
virtio_dir="/Windows/Drivers/VirtIO"
mktest "is-dir \"$virtio_dir\"" true
for drv in netkvm vioscsi viostor; do
    for sfx in cat inf sys; do
        mktest "is-file \"$virtio_dir/$drv.$sfx\"" true
    done
done

guestfish --ro -a "$img" < "$script" > "$response"
diff -u "$expected" "$response"

# Test the base image remained untouched
test "$md5" = "$(do_md5 $img_base)"
