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

# Test virt-v2v on real guests.

source ./functions.sh
set -e
set -x

slow_test
skip_if_skipped "$script"
requires_arch x86_64

guestname="$1"
if [ -z "$guestname" ]; then
    echo "$script: guestname parameter not set, don't run this test directly."
    exit 1
fi

disk="real-$guestname.img"
xml="real-$guestname.xml"
os="real-$guestname.d"
rm -f "$disk" "$xml"
rm -rf "$os"
cleanup_fn rm -f "$disk" "$xml"
cleanup_fn rm -rf "$os"
mkdir "$os"

# If the guest doesn't exist in virt-builder, skip.  This is because
# we test some RHEL guests which most users won't have access to.
skip_unless_virt_builder_guest "$guestname"

# Some guests need special virt-builder parameters.
# See virt-builder --notes "$guestname"
declare -a extra

case "$guestname" in
    # Don't try to update Windows versions.
    windows*)
        ;;
    # Or RHEL versions as they require access to private repos.
    rhel*)
        ;;
    *)
        extra[${#extra[*]}]='--update'
        ;;
esac

# Increase the amount of memory in the virt-builder appliance.
extra[${#extra[*]}]='--memsize=8192'

# Build a guest (using virt-builder).
virt-builder "$guestname" --quiet -o "$disk" "${extra[@]}"

# Create some minimal test metadata.
cat > "$xml" <<EOF
<domain type='test'>
  <name>$guestname</name>
  <memory>1048576</memory>
  <os>
    <type>hvm</type>
    <boot dev='hd'/>
  </os>
  <devices>
    <disk type='file' device='disk'>
      <driver name='qemu' type='raw'/>
      <source file='$(pwd)/$disk'/>
      <target dev='vda' bus='virtio'/>
    </disk>
  </devices>
</domain>
EOF

virt-v2v --debug-gc -i libvirtxml "$xml" -o local -os "$os"

# Test the libvirt XML metadata and a disk was created.
test -f "$os/$guestname.xml"
test -f "$os/$guestname-sda"

# Test the libvirt XML is valid.
# XXX This does not check bits depending on the QEMU version.
virt-xml-validate "$os/$guestname.xml"

# Test the disk has a similar size to the original.
size_before="$(du $disk | awk '{print $1}')"
size_after="$(du $os/$guestname-sda | awk '{print $1}')"
diff="$(( 100 * size_after / size_before ))"
if test $diff -lt 50; then
    echo "$script: disk image may have been corrupted or truncated"
    echo "size_before=$size_before size_after=$size_after diff=$diff"
    ls -l "$disk" "$os/$guestname-sda"
    exit 1
fi
