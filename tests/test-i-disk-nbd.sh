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

# Test -i disk with nbd URIs.

source ./functions.sh
set -e
set -x

skip_if_skipped

b=fedora.img
f=../test-data/phony-guests/$b
requires test -f $f

requires xz --version

# Check we have nbdkit and the plugins and filters required.
requires nbdkit --version
requires nbdkit file --version
requires nbdkit nbd --version
requires nbdkit null --version
requires nbdkit null --filter=cow --version
requires nbdkit null --filter=xz --version

# Check --exit-with-parent option will work.
requires nbdkit --exit-with-parent --version

d=test-i-disk-nbd.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

cp $f $d/$b
xz $d/$b

# Start nbdkit connected to the compressed file.
nbdkit -r -f --exit-with-parent \
       -U $d/sock -P $d/pid \
       --filter=xz file $d/$b.xz &

# Wait for the pidfile to appear.
for i in {1..60}; do
    if test -s "$d/pid"; then
        break
    fi
    sleep 1
done
if ! test -s "$d/pid"; then
    echo "$0: nbdkit: PID file $d/pid was not created"
    exit 1
fi
cleanup_fn kill "$(cat $d/pid)"

$VG virt-v2v --debug-gc \
    -i disk nbd+unix:///?socket=$d/sock \
    -o local -os $d

# Test the libvirt XML metadata and a disk was created.
test -f $d/unknown.xml
test -f $d/unknown-sda

cat $d/unknown.xml
