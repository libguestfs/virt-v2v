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

# Test --print-source option.

set -e

source ./functions.sh
set -e
set -x

skip_if_skipped
requires test -f ../test-data/phony-guests/windows.img

export LANG=C

d=test-print-source.d
rm -rf $d
cleanup_fn rm -r $d
mkdir $d

$VG virt-v2v --debug-gc \
    -i libvirtxml test-print-source.xml \
    -o local -os $d \
    --print-source > $d/output

mv $d/output $d/output.orig
< $d/output.orig \
grep -v 'Setting up the source' |
grep -v 'Opening the source' |
grep -v 'Source guest information' |
sed -e 's,/.*/windows.img,windows.img,' |
grep -v '^$' \
> $d/output

diff -u "$srcdir/test-print-source.expected" $d/output
