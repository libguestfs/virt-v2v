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

# Test virt-v2v (Phony) Windows conversion.

source ./functions.sh
set -e
set -x

guestname="$1"
if [ -z "$guestname" ]; then
    echo "$script: guestname parameter not set, don't run this test directly."
    exit 1
fi
f=../test-data/phony-guests/$guestname.img

skip_if_skipped $script
requires test -s $f

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win/drivers"

d=test-phony-$guestname.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i disk $f \
    -o local -os $d

# Test the libvirt XML metadata and a disk was created.
test -f $d/$guestname.xml
test -f $d/$guestname-sda

# Test some aspects of the target disk image.
script="$d/test.fish"
expected="$d/expected"
response="$d/response"

mktest ()
{
    local cmd="$1" exp="$2"

    echo "echo '$cmd'" >> "$script"
    echo "$cmd" >> "$expected"

    echo "$cmd" >> "$script"
    if [ -n "$exp" ] ; then
        echo "$exp" >> "$expected"
    fi
}

:> "$script"
:> "$expected"

cat >> "$script" <<EOF
  set-program virt-testing
  run
  inspect-os
  mount /dev/sda2 /
EOF

cat >> "$expected" <<EOF
/dev/sda2
EOF

firstboot_dir="/Program Files/Guestfs/Firstboot"
mktest "is-dir \"$firstboot_dir\"" true
mktest "is-file \"$firstboot_dir/firstboot.bat\"" true
mktest "is-dir \"$firstboot_dir/scripts\"" true
virtio_dir="/Windows/Drivers/VirtIO"
mktest "ls \"$virtio_dir\"" "$(cat test-phony-$guestname-ls.txt)"
osinfo_name="${guestname%-32}"
mktest "inspect-get-osinfo /dev/sda2" "$osinfo_name"

guestfish --ro -a "$d/$guestname-sda" < "$script" > "$response"
diff -u "$expected" "$response"

# host osinfo-db may be too old for win2k25
if [ $osinfo_name != "win2k25" ] ; then
    # Sanity check that _some_ osinfo annotation ends up in XML
    grep -q "libosinfo:os id='http://microsoft.com/win/" $d/$guestname.xml
fi
