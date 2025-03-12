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

# Make a Windows image which is enough to fool the inspection heuristics.

export LANG=C
set -e

IMAGENAME="$1"
test -z "$IMAGENAME" && (echo "Must pass image name" ; exit 1)
OSNAME=${IMAGENAME%".img"}
SOFTWARE_REG="$SRCDIR/$OSNAME-software.reg.bin"
SYSTEM_REG="$SRCDIR/windows-system.reg.bin"

if echo "$IMAGENAME" | grep -q "\-32"; then
  CMD_EXE="$SRCDIR/../binaries/bin-win32.exe"
else
  CMD_EXE="$SRCDIR/../binaries/bin-win64.exe"
fi

# If the currently compiled libguestfs doesn't support
# ntfs-3g/ntfsprogs then we cannot create a Windows phony image.
# Nothing actually uses these images in the standard build so we can
# just 'touch' it and emit a warning.
if ! guestfish -a /dev/null run : available "ntfs3g ntfsprogs"; then
  echo "***"
  echo "Warning: cannot create $IMAGENAME because there is no NTFS"
  echo "support in this build of libguestfs.  Just touching the output"
  echo "file instead."
  echo "***"
  touch $IMAGENAME
  exit 0
fi

# Create a disk image.
guestfish <<EOF
sparse $IMAGENAME-t 512M
run

# Format the disk.
part-init /dev/sda mbr
part-add /dev/sda p 64     524287
part-add /dev/sda p 524288    -64

# Disk ID.
pwrite-device /dev/sda "1234" 0x01b8 | cat >/dev/null

# Phony boot loader filesystem.
mkfs ntfs /dev/sda1

# Phony root filesystem.
mkfs ntfs /dev/sda2

# Enough to fool inspection API.
mount /dev/sda2 /
mkdir-p /Windows/System32/Config
mkdir-p /Windows/System32/Drivers

upload $SOFTWARE_REG /Windows/System32/Config/SOFTWARE
upload $SYSTEM_REG /Windows/System32/Config/SYSTEM

upload $CMD_EXE /Windows/System32/cmd.exe

mkdir "/Program Files"
touch /autoexec.bat

EOF

mv $IMAGENAME-t $IMAGENAME
