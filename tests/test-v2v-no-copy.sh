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

# Test --no-copy option.

set -e
set -x

$TEST_FUNCTIONS
skip_if_skipped
skip_if_backend uml
skip_unless_phony_guest windows.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
f=$top_builddir/test-data/phony-guests/windows.img

export VIRT_TOOLS_DATA_DIR="$top_srcdir/test-data/fake-virt-tools"

d=test-v2v-no-copy.d
rm -rf $d
mkdir $d

# No copy with -o local.
$VG virt-v2v --debug-gc --no-copy \
    -i libvirt -ic "$libvirt_uri" windows \
    -o local -os $d

# Test the libvirt XML metadata was created.
test -f $d/windows.xml

# Test the disk was NOT created.
! test -f $d/windows-sda

# --no-copy with -o rhv.
mkdir $d/12345678-1234-1234-1234-123456789abc
mkdir $d/12345678-1234-1234-1234-123456789abc/images
mkdir $d/12345678-1234-1234-1234-123456789abc/master
mkdir $d/12345678-1234-1234-1234-123456789abc/master/vms

# $VG - XXX Disabled because the forking used to write files in -o rhv
# mode confuses valgrind.
virt-v2v --debug-gc --no-copy \
    -i libvirt -ic "$libvirt_uri" windows \
    -o rhv -os $d

# Test the OVF metadata was created.
test -f $d/12345678-1234-1234-1234-123456789abc/master/vms/*/*.ovf

# Test the disk was NOT created.
! test -f $d/12345678-1234-1234-1234-123456789abc/images/*/*.meta

rm -r $d
