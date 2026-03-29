#!/bin/bash -
# libguestfs
# Copyright (C) 2010-2025 Red Hat Inc.
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

# Make a multi-disk Windows image for testing boot order.
# Disk 1 (sda) is a blank data disk.
# Disk 2 (sdb) has the Windows OS on it.
# This tests that virt-v2v correctly identifies the boot disk
# when Windows is not on the first disk.

export LANG=C
set -e

SOFTWARE_REG="$SRCDIR/win2k22-software.reg.bin"
SYSTEM_REG="$SRCDIR/windows-system.reg.bin"
CMD_EXE="$SRCDIR/../binaries/bin-win64.exe"

# If libguestfs doesn't support ntfs-3g/ntfsprogs, just touch
# the output files and skip.
if ! guestfish -a /dev/null run : available "ntfs3g ntfsprogs"; then
    touch windows-multi-disk-sda.img windows-multi-disk-sdb.img
    exit 0
fi

# Create disk 1 (sda) - a blank data disk with a single NTFS partition.
guestfish <<EOF
sparse windows-multi-disk-sda.img-t 256M
run

part-init /dev/sda gpt
part-add /dev/sda p 64 -64

mkfs ntfs /dev/sda1

EOF

mv windows-multi-disk-sda.img-t windows-multi-disk-sda.img

# Create disk 2 (sdb) - the Windows OS disk.
guestfish <<EOF
sparse windows-multi-disk-sdb.img-t 512M
run

# Format the disk.
part-init /dev/sda gpt
part-add /dev/sda p 64     524287
part-add /dev/sda p 524288    -64

# Phony boot loader filesystem.
mkfs vfat /dev/sda1

# Mark this as a BIOS boot partition.
part_set_gpt_type /dev/sda 1 21686148-6449-6E6F-744E-656564454649

# Phony root filesystem.
mkfs ntfs /dev/sda2

# Enough to fool inspection API.
mount /dev/sda2 /
mkdir-p /Windows/System32/Config
mkdir-p /Windows/System32/Drivers
mkdir-p /Windows/TEMP

upload $SOFTWARE_REG /Windows/System32/Config/SOFTWARE
upload $SYSTEM_REG /Windows/System32/Config/SYSTEM

upload $CMD_EXE /Windows/System32/cmd.exe

mkdir "/Program Files"
touch /autoexec.bat

EOF

mv windows-multi-disk-sdb.img-t windows-multi-disk-sdb.img
