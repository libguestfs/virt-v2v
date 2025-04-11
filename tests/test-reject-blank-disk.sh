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

# Test virt-v2v rejects a blank disk.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/blank-disk.img
requires test -f $f

out=test-reject-blank-disk.out
rm -f $out
cleanup_fn rm -f $out

export LANG=C

if $VG virt-v2v --debug-gc -i disk $f -o null >& $out ; then
    echo "FAIL: virt-v2v didn't reject blank disk"
    exit 1
fi

cat $out

# Check the right error message is printed.
grep "could not detect the source guest" $out
grep "a blank disk" $out
