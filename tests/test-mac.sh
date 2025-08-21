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

# Test --mac parameter.

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -s ../test-data/phony-guests/windows.img

libvirt_uri="test://$abs_builddir/test-mac.xml"
f=../test-data/phony-guests/windows.img

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

d=test-mac.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i libvirt -ic "$libvirt_uri" windows \
    -o local -os $d \
    --mac 52:54:00:01:02:03:network:nancy \
    --mac 52:54:00:01:02:04:bridge:bob \
    --network default_network

# Test the libvirt XML metadata was created.
test -f $d/windows.xml

# Extract just the network interfaces from the XML.
# Delete the network model XML because that can change depending
# on whether virtio-win is installed or not.
sed -n '/interface/,/\/interface/p' $d/windows.xml |
  grep -v 'model type=' > $d/networks

# Test that the output has mapped the networks and bridges correctly.
diff -ur "$srcdir/test-mac-expected.xml" $d/networks
