#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2018 Red Hat Inc.
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

# Test -o rhv-upload.
#
# These uses a test harness (see
# tests/test-v2v-o-rhv-upload-module/ovirtsdk4) to fake responses from
# oVirt.

set -e
set -x

source ./functions.sh
set -e
set -x

skip_if_skipped
requires python3 --version
requires nbdkit $VIRT_V2V_NBDKIT_PYTHON_PLUGIN --version
requires test -f ../test-data/phony-guests/windows.img

libvirt_uri="test://$abs_top_builddir/test-data/phony-guests/guests.xml"
f=../test-data/phony-guests/windows.img

export VIRT_TOOLS_DATA_DIR="$srcdir/../test-data/fake-virt-tools"
export VIRTIO_WIN="$srcdir/../test-data/fake-virtio-win"
export PYTHONPATH=$srcdir/test-v2v-o-rhv-upload-module:$PYTHONPATH

# Run the imageio process and get the port number.
log=test-v2v-o-rhv-upload.webserver.log
rm -f $log
cleanup_fn rm -f $log
$srcdir/test-v2v-o-rhv-upload-module/imageio.py >$log 2>&1 &
pid=$!
cleanup_fn kill $pid
export IMAGEIO_PORT=
for i in {1..5}; do
    IMAGEIO_PORT=$( grep "^port:" $log | awk '{print $2}' )
    if [ -n "$IMAGEIO_PORT" ]; then break; fi
    sleep 3
done
if [ ! -n "$IMAGEIO_PORT" ]; then
    echo "$0: imageio process did not start up"
    cat $log
    exit 1
fi
echo IMAGEIO_PORT=$IMAGEIO_PORT

# Run virt-v2v -o rhv-upload.
#
# The fake ovirtsdk4 module doesn't care about most of the options
# like -oc, -oo rhv-cafile, -op etc.  Any values may be used.
$VG virt-v2v --debug-gc -v -x \
    -i libvirt -ic "$libvirt_uri" windows \
    -o rhv-upload \
    -oc https://example.com/ovirt-engine/api \
    -oo rhv-cafile=/dev/null \
    -op /dev/null \
    -os Storage
