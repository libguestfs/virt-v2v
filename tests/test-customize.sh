#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2024 Red Hat Inc.
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

# Test virt-customize options of virt-v2v.

source ./functions.sh
set -e
set -x

skip_if_skipped
fedora=../test-data/phony-guests/fedora.img
requires test -f $fedora

d=test-customize.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i disk $fedora \
    --upload Makefile.am:/file1 \
    --append-line '/etc/fstab:# this is a new comment' \
    -o local -os $d

# Test the file was uploaded into the image.
guestfish --ro -a $d/fedora-sda -i cat /file1
