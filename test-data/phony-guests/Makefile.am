# libguestfs test images
# Copyright (C) 2009-2025 Red Hat Inc.
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

include $(top_srcdir)/subdir-rules.mk

EXTRA_DIST = \
	make-debian-img.sh \
	debian-packages \
	debian-syslog \
	make-fedora-img.pl \
	fedora-journal.tar.xz \
	fedora-db.sql.xz \
	fedora.db \
	make-ubuntu-img.sh \
	make-archlinux-img.sh \
	archlinux-package \
	make-coreos-img.sh \
	make-windows-img.sh \
	win10-software.reg \
	win10-software.reg.bin \
	win11-software.reg \
	win11-software.reg.bin \
	win2k22-software.reg \
	win2k22-software.reg.bin \
	win2k25-software.reg \
	win2k25-software.reg.bin \
	win7-32-software.reg \
	win7-32-software.reg.bin \
	winxp-32-software.reg \
	winxp-32-software.reg.bin \
	windows-software-all.reg \
	windows-system.reg \
	windows-system.reg.bin \
	windows-bcd.reg \
	windows-bcd.reg.bin \
	minimal-hive \
	make-guests-all-good.pl \
	guests.xml.in

disk_images = \
	blank-disk.img \
	blank-part.img \
	blank-fs.img \
	blank-bootroot.img \
	blank-bootrootlv.img \
	debian.img \
	fedora.img \
	fedora-md1.img \
	fedora-md2.img \
	fedora-btrfs.img \
	fedora-luks-on-lvm.img \
	fedora-lvm-on-luks.img \
	ubuntu.img \
	archlinux.img \
	coreos.img \
	win10.img \
	win11.img \
	win2k22.img \
	win2k22-uefi.img \
	win2k25.img \
	win7-32.img \
	winxp-32.img \
	windows.img

# This is 'check_DATA' because we don't need it until 'make check'
# time and we need the tools we have built in order to make it.
check_DATA = $(disk_images) guests-all-good.xml

CLEANFILES += \
	$(check_DATA) \
	guests-all-good.xml \
	stamp-fedora-md.img

# Make several different blank images.  These are not guests, but we
# include them in the libvirt fake XML to make sure that virt-df and
# virt-alignment-scan don't break when they encounter them.
blank-%.img:
	rm -f $@ $@-t
	$(top_builddir)/run guestfish \
	    -N $@-t="$$(echo $@ | $(SED) -e 's/blank-//' -e 's/.img//')" exit
	mv $@-t $@

# Make a (dummy) Fedora image.
fedora.img: make-fedora-img.pl \
		fedora-journal.tar.xz \
		fedora.db \
		fedora-static-bin
	SRCDIR=$(srcdir) LAYOUT=partitions $(top_builddir)/run --test ./$<

# Make a (dummy) Fedora image using md devices
fedora-md1.img fedora-md2.img: stamp-fedora-md.img

stamp-fedora-md.img: make-fedora-img.pl \
		fedora-journal.tar.xz \
		fedora.db \
		fedora-static-bin
	rm -f $@
	SRCDIR=$(srcdir) LAYOUT=partitions-md $(top_builddir)/run --test ./$<
	touch $@

# Make a (dummy) Fedora image using a single btrfs with two subvolumes
# for root and home.
fedora-btrfs.img: make-fedora-img.pl \
		fedora-journal.tar.xz \
		fedora.db \
		fedora-static-bin
	SRCDIR=$(srcdir) LAYOUT=btrfs $(top_builddir)/run --test ./$<

# Make a (dummy) Fedora image with LUKS-on-LVM.
fedora-luks-on-lvm.img: make-fedora-img.pl \
		fedora-journal.tar.xz \
		fedora.db \
		fedora-static-bin
	SRCDIR=$(srcdir) LAYOUT=luks-on-lvm $(top_builddir)/run --test ./$<

# Make a (dummy) Fedora image with LVM-on-LUKS.
fedora-lvm-on-luks.img: make-fedora-img.pl \
		fedora-journal.tar.xz \
		fedora.db \
		fedora-static-bin
	SRCDIR=$(srcdir) LAYOUT=lvm-on-luks $(top_builddir)/run --test ./$<

# Make a (dummy) Debian image.
debian.img: make-debian-img.sh
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$<

# Make a (dummy) Ubuntu image.
ubuntu.img: make-ubuntu-img.sh
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$<

# Make a (dummy) Arch Linux image.
archlinux.img: make-archlinux-img.sh
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$<

# Make a (dummy) CoreOS image.
coreos.img: make-coreos-img.sh
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$<

# Make a (dummy) Windows image.
win%.img: make-windows-img.sh \
		win%-software.reg.bin \
		windows-system.reg.bin
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$< $@

# Make a (dummy) Windows UEFI image.
win2k22-uefi.img: make-windows-img.sh \
		win2k22-software.reg.bin \
		windows-system.reg.bin \
		windows-bcd.reg.bin
	SRCDIR=$(srcdir) $(top_builddir)/run --test ./$< $@

# This is the 'latest` windows image used by most test cases
windows.img: win2k22.img
	ln -f $< $@

guests-all-good.xml: make-guests-all-good.pl $(disk_images)
	rm -f $@ $@-t
	./$^ > $@-t
	mv $@-t $@

# Since users might not have the tools needed to create this, we also
# distribute these files and they are only cleaned by 'make distclean'
# not regular 'make clean'.
fedora.db: fedora-db.sql.xz
	rm -f $@ $@-t
	xzcat $< | $(SQLITE3) $@-t
	mv $@-t $@

# This is included in the phony Fedora image to act as a phony "rpm"
# and "dracut" command.  For the use of -all-static here, see
# libguestfs/tests/Makefile.am
check_PROGRAMS = fedora-static-bin
fedora_static_bin_SOURCES = fedora.c
fedora_static_bin_LDFLAGS = -all-static

%-software.reg.bin: %-software.reg windows-software-all.reg
	rm -f $@ $@-t $<-t
	cp $(srcdir)/minimal-hive $@-t
	cat $^ > $<-t
	hivexregedit --merge $@-t --prefix 'HKEY_LOCAL_MACHINE\SOFTWARE' $<-t
	mv $@-t $@
	rm -f $<-t

windows-system.reg.bin: windows-system.reg
	rm -f $@ $@-t
	cp $(srcdir)/minimal-hive $@-t
	hivexregedit --merge $@-t --prefix 'HKEY_LOCAL_MACHINE\SYSTEM' $<
	mv $@-t $@

windows-bcd.reg.bin: windows-bcd.reg
	rm -f $@ $@-t
	cp $(srcdir)/minimal-hive $@-t
	hivexregedit --merge $@-t $<
	mv $@-t $@

DISTCLEANFILES += \
	fedora.db \
	windows-software.reg.bin \
	windows-system.reg.bin \
	windows-bcd.reg.bin

# Don't construct the guests in parallel.  In automake 1.13, check_DATA
# was changed so it can now run in parallel, but this causes everything
# to fall over on machines with limited memory.
.NOTPARALLEL:
