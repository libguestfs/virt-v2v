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

if [[ "$IMAGENAME" =~ -uefi ]]; then
    EFI=1
    OSNAME=${IMAGENAME%"-uefi.img"}
else
    OSNAME=${IMAGENAME%".img"}
fi

SOFTWARE_REG="$SRCDIR/$OSNAME-software.reg.bin"
SYSTEM_REG="$SRCDIR/windows-system.reg.bin"

if [[ "$IMAGENAME" =~ -32 ]]; then
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

# Extra initialization required for UEFI.
if test "$EFI" = "1"; then
    BCD="$SRCDIR/windows-bcd.reg.bin"
    EFI_COMMANDS="
# Set /dev/sda1 as the EFI system partition (ESP)
part_set_gpt_type /dev/sda 1 C12A7328-F81F-11D2-BA4B-00A0C93EC93B

# Create a boot ESP similar to Windows 11
mount /dev/sda1 /Windows/TEMP

mkdir /Windows/TEMP/EFI
mkdir /Windows/TEMP/EFI/Boot
touch /Windows/TEMP/EFI/Boot/bootx64.efi

mkdir /Windows/TEMP/EFI/Microsoft
mkdir /Windows/TEMP/EFI/Microsoft/Boot
touch /Windows/TEMP/EFI/Microsoft/Boot/bootmgf.efi
touch /Windows/TEMP/EFI/Microsoft/Boot/bootmgfw.efi
upload $BCD /Windows/TEMP/EFI/Microsoft/Boot/BCD
"
fi

# Create a disk image.
guestfish <<EOF
set-program virt-testing
sparse $IMAGENAME-t 512M
run

# Format the disk.
part-init /dev/sda gpt
part-add /dev/sda p 64     524287
part-add /dev/sda p 524288    -64

# Phony boot loader filesystem.
mkfs vfat /dev/sda1

# Mark this as a BIOS boot partition.  UEFI commands below
# may override this.
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

# If UEFI, put a BCD on here.
$EFI_COMMANDS

EOF

mv $IMAGENAME-t $IMAGENAME
