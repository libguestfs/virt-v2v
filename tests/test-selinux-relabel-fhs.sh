#!/bin/bash -
# libguestfs virt-v2v test script
# Copyright (C) 2014-2026 Red Hat Inc.
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

# Test virt-v2v --selinux-relabel-fhs on modified phony Fedora guest.

source ./functions.sh
set -e
set -x

skip_if_skipped
f=../test-data/phony-guests/fedora.img
requires test -f $f

d=test-selinux-relabel-fhs.d
rm -rf $d
cleanup_fn rm -rf $d
mkdir $d

cp $f $d/fedora.img
truncate -s 100M $d/blank_b

guestfish -a $d/fedora.img -a $d/blank_b -i <<'EOF'

# Make sure this looks sufficiently like an SELinux guest
mkdir /usr/sbin
touch /usr/sbin/load_policy
mkdir /etc/selinux
write /etc/selinux/config "SELINUX=enforcing\nSELINUXTYPE=targeted\n"
mkdir-p /etc/selinux/targeted/contexts/files
touch /etc/selinux/targeted/contexts/files/file_contexts

# Format the blank disk
mkfs ext2 /dev/sdb

# Should not be relabelled
mkdir /database
write-append /etc/fstab "/dev/sdb ///database/ ext2 default 0 0\n"

# Should be relabelled
mkdir /usr/stuff
write-append /etc/fstab "/dev/sdb /usr//stuff ext2 default 0 0\n"

# Should not be relabelled
mkdir /other
write-append /etc/fstab "/dev/sdb //boot/../other/// ext2 default 0 0\n"
mkdir /srvsrv
write-append /etc/fstab "/dev/sdb /srvsrv ext2 default 0 0\n"

EOF

$VG virt-v2v --debug-gc -vx \
    -i disk $d/fedora.img $d/blank_b \
    --selinux-relabel-fhs \
    -o null >& $d/log || { cat $d/log; exit 1; }

cat $d/log

grep 'is_FHS.*"/".*accepted' $d/log
grep 'is_FHS.*"/boot".*accepted' $d/log
grep 'is_FHS.*"/database".*rejected' $d/log
grep 'is_FHS.*"/other".*rejected' $d/log
grep 'is_FHS.*"/srvsrv".*rejected' $d/log
grep 'is_FHS.*"/usr/stuff".*accepted' $d/log
