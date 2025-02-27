#!/bin/bash -
# libguestfs
# Copyright (C) 2015 Red Hat Inc.
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

# Regression test for virt-v2v handling of blank disks:
# https://bugzilla.redhat.com/show_bug.cgi?id=1232192

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires virt-v2v --help
requires test -s ../test-data/phony-guests/windows.img
requires test -f ../test-data/phony-guests/blank-disk.img

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"

virt-v2v -i libvirtxml test-rhbz1232192.xml -o null
