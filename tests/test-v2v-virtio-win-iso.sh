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

# Test virt-v2v Windows conversion with VirtIO drivers on an ISO image.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/windows.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
f=../test-data/phony-guests/windows.img

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="../test-data/fake-virtio-win/fake-virtio-win.iso"

if ! test -f "$VIRTIO_WIN"; then
    echo "$0: test skipped because fake virtio-win iso image was not created"
    exit 77
fi

# Return a random element from the array 'choices'.
function random_choice
{
    echo "${choices[$((RANDOM % ${#choices[*]}))]}"
}

# Test the --root option stochastically.
choices=("/dev/sda2" "single" "first")
root=`random_choice`

d=test-v2v-virtio-win-iso.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" windows \
    -o local -os $d \
    --root $root

# Test the libvirt XML metadata and a disk was created.
test -f $d/windows.xml
test -f $d/windows-sda

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

guestfish --ro -a "$d/windows-sda" < "$script" > "$response"
diff -u "$expected" "$response"
