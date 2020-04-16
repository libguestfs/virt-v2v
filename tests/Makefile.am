# libguestfs tests
# Copyright (C) 2009-2020 Red Hat Inc.
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

EXTRA_DIST = test-functions.sh

# Small utility to check for a needed libvirt version;
# to be used in shell/script-based tests.
noinst_PROGRAMS = libvirt-is-version

libvirt_is_version_SOURCES = libvirt-is-version.c

libvirt_is_version_LDADD = \
	$(LIBVIRT_LIBS) \
	$(LTLIBINTL)

libvirt_is_version_CPPFLAGS = \
	-DLOCALEBASEDIR=\""$(datadir)/locale"\"

libvirt_is_version_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBVIRT_CFLAGS)

# Tests.

# The virt-v2v tests here are not meant to be thorough tests of guest
# conversion.  There is a test suite used to test conversion which is
# kept outside libguestfs because it contains lots of proprietary
# unredistributable guests like Windows.
#
# The tests here instead are testing:
#
# - virt-v2v command line arguments
# - virt-v2v reads and writes files to the correct places
# - valgrind & memory leaks
# - any conversion properties that can be tested using just the
#   phony guests (but we don't go out of our way here)
# - that up to date guests don't crash virt-v2v

TESTS_ENVIRONMENT = $(top_builddir)/run --test

TESTS = \
	test-v2v-copy-to-local.sh \
	test-v2v-i-ova-bad-sha1.sh \
	test-v2v-i-ova-bad-sha256.sh \
	test-v2v-i-ova-formats.sh \
	test-v2v-i-ova-good-checksums.sh \
	test-v2v-i-ova-gz.sh \
	test-v2v-i-ova-invalid-manifest1.sh \
	test-v2v-i-ova-invalid-manifest2.sh \
	test-v2v-i-ova-snapshots.sh \
	test-v2v-i-ova-subfolders.sh \
	test-v2v-i-ova-tar.sh \
	test-v2v-i-ova-two-disks.sh \
	test-v2v-i-vmx.sh \
	test-v2v-it-vddk-io-query.sh \
	test-v2v-o-rhv-upload-oo-query.sh \
	test-v2v-o-vdsm-oo-query.sh \
	test-v2v-bad-networks-and-bridges.sh \
	test-v2v-cdrom.sh \
	test-v2v-floppy.sh \
	test-v2v-i-disk.sh \
	test-v2v-i-ova.sh \
	test-v2v-in-place.sh \
	test-v2v-mac.sh \
	test-v2v-machine-readable.sh \
	test-v2v-networks-and-bridges.sh \
	test-v2v-no-copy.sh \
	test-v2v-o-glance.sh \
	test-v2v-o-json.sh \
	test-v2v-o-libvirt.sh \
	test-v2v-o-null.sh \
	test-v2v-o-openstack.sh \
	test-v2v-o-qemu.sh \
	test-v2v-o-rhv.sh \
	test-v2v-o-rhv-upload.sh \
	test-v2v-o-vdsm-options.sh \
	test-v2v-oa-option.sh \
	test-v2v-of-option.sh \
	test-v2v-on-option.sh \
	test-v2v-print-estimate.sh \
	test-v2v-print-source.sh \
	test-v2v-sound.sh \
	test-v2v-virtio-win-iso.sh \
	test-v2v-windows-conversion.sh \
	rhbz1232192.sh \
	$(SLOW_TESTS) \
	$(ROOT_TESTS)

# The VMDK file is used for some -i ova tests.
check_DATA = windows.vmdk
windows.vmdk: ../test-data/phony-guests/windows.img
	rm -f $@ $@-t
	if [ -s $< ]; then \
	    qemu-img convert -f raw $< -O vmdk $@-t; \
	    mv $@-t $@; \
	else \
	    touch $@; \
	fi

DISTCLEANFILES += \
	windows.vmdk

check-valgrind:
	$(MAKE) VG="@VG@" check

SLOW_TESTS = \
	$(real_guests_scripts) \
	test-v2v-trim.sh

check-slow:
	$(MAKE) check TESTS="$(SLOW_TESTS)" SLOW=1

ROOT_TESTS = \
	test-v2v-i-ova-as-root.sh

check-root:
	$(MAKE) check TESTS="$(ROOT_TESTS)"

# A selection of real guests that test-v2v-conversion-of.sh will
# try to convert.  This is only used by 'make check-slow'.

real_guests_scripts = \
	test-v2v-conversion-of-centos-6.sh \
	test-v2v-conversion-of-centos-7.0.sh \
	test-v2v-conversion-of-debian-6.sh \
	test-v2v-conversion-of-debian-7.sh \
	test-v2v-conversion-of-debian-8.sh \
	test-v2v-conversion-of-debian-9.sh \
	test-v2v-conversion-of-fedora-20.sh \
	test-v2v-conversion-of-fedora-23.sh \
	test-v2v-conversion-of-fedora-29.sh \
	test-v2v-conversion-of-opensuse-13.1.sh \
	test-v2v-conversion-of-opensuse-13.2.sh \
	test-v2v-conversion-of-opensuse-42.1.sh \
	test-v2v-conversion-of-rhel-5.10.sh \
	test-v2v-conversion-of-rhel-6.8.sh \
	test-v2v-conversion-of-rhel-7.0.sh \
	test-v2v-conversion-of-rhel-7.2.sh \
	test-v2v-conversion-of-ubuntu-10.04.sh \
	test-v2v-conversion-of-ubuntu-12.04.sh \
	test-v2v-conversion-of-ubuntu-14.04.sh \
	test-v2v-conversion-of-ubuntu-16.04.sh \
	test-v2v-conversion-of-ubuntu-18.04.sh \
	test-v2v-conversion-of-windows-6.2-server.sh \
	test-v2v-conversion-of-windows-6.3-server.sh \
	test-v2v-conversion-of-windows-10.0-server.sh

test-v2v-conversion-of-%.sh:
	rm -f $@ $@-t
	f=`echo "$@" | $(SED) 's/test-v2v-conversion-of-\(.*\).sh/\1/'`; \
	echo 'script=$@ exec $$srcdir/test-v2v-conversion-of.sh' "$$f" > $@-t
	chmod 0755 $@-t
	mv $@-t $@

CLEANFILES += \
	$(real_guests_scripts) \
	real-*.img \
	real-*.xml

EXTRA_DIST += \
	test-v2v-bad-networks-and-bridges.sh \
	test-v2v-cdrom.expected \
	test-v2v-cdrom.sh \
	test-v2v-copy-to-local.sh \
	test-v2v-floppy.expected \
	test-v2v-floppy.sh \
	test-v2v-i-disk.sh \
	test-v2v-i-ova-as-root.ovf \
	test-v2v-i-ova-as-root.sh \
	test-v2v-i-ova-bad-sha1.sh \
	test-v2v-i-ova-bad-sha256.sh \
	test-v2v-i-ova-checksums.ovf \
	test-v2v-i-ova-formats.expected \
	test-v2v-i-ova-formats.ovf \
	test-v2v-i-ova-formats.sh \
	test-v2v-i-ova-good-checksums.sh \
	test-v2v-i-ova-gz.expected \
	test-v2v-i-ova-gz.ovf \
	test-v2v-i-ova-gz.sh \
	test-v2v-i-ova-invalid-manifest1.sh \
	test-v2v-i-ova-invalid-manifest2.sh \
	test-v2v-i-ova-snapshots.expected \
	test-v2v-i-ova-snapshots.expected2 \
	test-v2v-i-ova-snapshots.ovf \
	test-v2v-i-ova-snapshots.sh \
	test-v2v-i-ova-subfolders.expected \
	test-v2v-i-ova-subfolders.expected2 \
	test-v2v-i-ova-subfolders.ovf \
	test-v2v-i-ova-subfolders.sh \
	test-v2v-i-ova-tar.expected \
	test-v2v-i-ova-tar.expected2 \
	test-v2v-i-ova-tar.ovf \
	test-v2v-i-ova-tar.sh \
	test-v2v-i-ova-two-disks.expected \
	test-v2v-i-ova-two-disks.expected2 \
	test-v2v-i-ova-two-disks.ovf \
	test-v2v-i-ova-two-disks.sh \
	test-v2v-i-ova.ovf \
	test-v2v-i-ova.sh \
	test-v2v-i-ova.xml \
	test-v2v-i-vmx.sh \
	test-v2v-i-vmx-1.expected \
	test-v2v-i-vmx-2.expected \
	test-v2v-i-vmx-3.expected \
	test-v2v-i-vmx-4.expected \
	test-v2v-i-vmx-5.expected \
	test-v2v-i-vmx-1.vmx \
	test-v2v-i-vmx-2.vmx \
	test-v2v-i-vmx-3.vmx \
	test-v2v-i-vmx-4.vmx \
	test-v2v-i-vmx-5.vmx \
	test-v2v-in-place.sh \
	test-v2v-it-vddk-io-query.sh \
	test-v2v-machine-readable.sh \
	test-v2v-mac-expected.xml \
	test-v2v-mac.sh \
	test-v2v-networks-and-bridges.sh \
	test-v2v-networks-and-bridges-expected.xml \
	test-v2v-no-copy.sh \
	test-v2v-o-glance.sh \
	test-v2v-o-json.sh \
	test-v2v-o-libvirt.sh \
	test-v2v-o-null.sh \
	test-v2v-o-openstack.sh \
	test-v2v-o-qemu.sh \
	test-v2v-o-rhv.ovf.expected \
	test-v2v-o-rhv.sh \
	test-v2v-o-rhv-upload.sh \
	test-v2v-o-rhv-upload-module/ovirtsdk4/__init__.py \
	test-v2v-o-rhv-upload-module/ovirtsdk4/types.py \
	test-v2v-o-rhv-upload-oo-query.sh \
	test-v2v-o-vdsm-oo-query.sh \
	test-v2v-o-vdsm-options.ovf.expected \
	test-v2v-o-vdsm-options.sh \
	test-v2v-oa-option.sh \
	test-v2v-of-option.sh \
	test-v2v-on-option.sh \
	test-v2v-print-estimate.sh \
	test-v2v-print-source.expected \
	test-v2v-print-source.sh \
	test-v2v-conversion-of.sh \
	test-v2v-sound.sh \
	test-v2v-trim.sh \
	test-v2v-virtio-win-iso.sh \
	test-v2v-windows-conversion.sh \
	rhbz1232192.sh
