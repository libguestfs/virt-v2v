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

# Test -o libvirt.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/windows.img

# You shouldn't be running the tests as root anyway, but in this case
# it's especially bad because we don't want to start creating guests
# or storage pools in the system namespace.
skip_if_root

# Since libvirt ~ 1.2.19, it started to check that the guest
# architecture was valid at guest creation time, rather than when you
# first run the guest.  Since the guest XML contains arch='x86_64',
# this test will fail on !x86_64.
requires_arch x86_64

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
f=../test-data/phony-guests/windows.img

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

# Generate a random guest name.
guestname=tmp-$(tr -cd 'a-f0-9' < /dev/urandom | head -c 8)

# Generate a random pool name.
poolname=tmp-$(tr -cd 'a-f0-9' < /dev/urandom | head -c 8)

d=test-o-libvirt.d
rm -rf $d

# Clean up.
cleanup_fn rm -r $d
cleanup_fn virsh pool-destroy $poolname
cleanup_fn virsh undefine $guestname

mkdir $d

# Set up the output directory as a libvirt storage pool.
virsh pool-destroy $poolname ||:
virsh pool-create-as $poolname dir - - - - $(pwd)/$d

$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" windows \
    -o libvirt -os $poolname -on $guestname

# Test the disk was created.
test -f $d/$guestname-sda

# Test the guest exists.
virsh dumpxml $guestname
