# libguestfs tests
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

EXTRA_DIST = functions.sh.in
check_DATA = functions.sh

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
	test-bad-networks-and-bridges.sh \
	test-block-driver.sh \
	test-cdrom.sh \
	test-checksum-bad.sh \
	test-checksum-good-qcow2.sh \
	test-checksum-good.sh \
	test-checksum-print.sh \
	test-customize.sh \
	test-fedora-conversion.sh \
	test-fedora-luks-on-lvm-conversion.sh \
	test-fedora-lvm-on-luks-conversion.sh \
	test-fedora-md-conversion.sh \
	test-floppy.sh \
	test-i-disk-parallel.sh \
	test-i-disk.sh \
	test-i-ova-bad-sha1.sh \
	test-i-ova-bad-sha256.sh \
	test-i-ova-directory.sh \
	test-i-ova-formats.sh \
	test-i-ova-good-checksums.sh \
	test-i-ova-gz.sh \
	test-i-ova-invalid-manifest1.sh \
	test-i-ova-invalid-manifest2.sh \
	test-i-ova-snapshots.sh \
	test-i-ova-subfolders.sh \
	test-i-ova-tar.sh \
	test-i-ova-two-disks.sh \
	test-i-ova.sh \
	test-i-vmx.sh \
	test-in-place.sh \
	test-in-place-xml.sh \
	test-inspector.sh \
	test-it-vddk-io-query.sh \
	test-mac.sh \
	test-machine-readable.sh \
	test-networks-and-bridges.sh \
	test-o-glance.sh \
	test-o-kubevirt-fedora.sh \
	test-o-kubevirt-oo-disk.sh \
	test-o-kubevirt-windows.sh \
	test-o-libvirt.sh \
	test-o-local-qcow2-compressed.sh \
	test-o-null.sh \
	test-o-openstack.sh \
	test-o-ovirt-upload-oo-query.sh \
	test-o-ovirt-upload.sh \
	test-o-ovirt.sh \
	test-o-qemu.sh \
	test-o-vdsm-oo-query.sh \
	test-o-vdsm-options.sh \
	test-oa-option-qcow2.sh \
	test-oa-option-raw.sh \
	test-of-option.sh \
	test-on-option.sh \
	test-open.sh \
	test-open-encrypted.sh \
	test-print-source.sh \
	test-reject-blank-disk.sh \
	test-rhbz1232192.sh \
	test-sound.sh \
	test-virtio-win-iso.sh \
	test-windows-conversion.sh \
	test-windows-uefi-conversion.sh \
	$(phony_windows_scripts) \
	$(SLOW_TESTS) \
	$(ROOT_TESTS)

# The VMDK file is used for some -i ova tests.
check_DATA += windows.vmdk
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
	test-trim.sh

check-slow:
	$(MAKE) check TESTS="$(SLOW_TESTS)" SLOW=1

ROOT_TESTS = \
	test-i-ova-as-root.sh

check-root:
	$(MAKE) check TESTS="$(ROOT_TESTS)"

# A selection of real guests that test-conversion-of.sh will
# try to convert.  This is only used by 'make check-slow'.

real_guests_scripts = \
	test-conversion-of-debian-6.sh \
	test-conversion-of-debian-7.sh \
	test-conversion-of-debian-8.sh \
	test-conversion-of-debian-9.sh \
	test-conversion-of-debian-10.sh \
	test-conversion-of-debian-11.sh \
	test-conversion-of-debian-12.sh \
	test-conversion-of-fedora-42.sh \
	test-conversion-of-opensuse-13.1.sh \
	test-conversion-of-opensuse-13.2.sh \
	test-conversion-of-opensuse-42.1.sh \
	test-conversion-of-rhel-5.11.sh \
	test-conversion-of-rhel-6.10.sh \
	test-conversion-of-rhel-7.9.sh \
	test-conversion-of-rhel-8.7.sh \
	test-conversion-of-rhel-9.4.sh \
	test-conversion-of-rhel-10.0.sh \
	test-conversion-of-ubuntu-16.04.sh \
	test-conversion-of-ubuntu-18.04.sh \
	test-conversion-of-ubuntu-20.04.sh \
	test-conversion-of-windows-6.2-server.sh \
	test-conversion-of-windows-6.3-server.sh \
	test-conversion-of-windows-10.0-server.sh

test-conversion-of-%.sh:
	rm -f $@ $@-t
	f=`echo "$@" | $(SED) 's/test-conversion-of-\(.*\).sh/\1/'`; \
	echo 'script=$@ exec $$srcdir/test-conversion-of.sh' "$$f" > $@-t
	chmod 0755 $@-t
	mv $@-t $@

DISTCLEANFILES += \
	$(real_guests_scripts)

CLEANFILES += \
	real-*.img \
	real-*.xml

phony_windows_scripts = \
	test-phony-win10.sh \
	test-phony-win11.sh \
	test-phony-win2k22.sh \
	test-phony-win2k25.sh \
	test-phony-win7-32.sh \
	test-phony-winxp-32.sh

test-phony-win%.sh: test-windows-phony.sh
	rm -f $@ $@-t
	f=`echo "$@" | $(SED) 's/test-phony-\(.*\).sh/\1/'`; \
	echo 'script=$@ exec $$srcdir/test-windows-phony.sh' "$$f" > $@-t
	chmod 0755 $@-t
	mv $@-t $@

DISTCLEANFILES += \
	$(phony_windows_scripts)

EXTRA_DIST += \
	test-bad-networks-and-bridges.sh \
	test-block-driver.sh \
	test-cdrom.expected \
	test-cdrom.sh \
	test-checksum-bad.sh \
	test-checksum-good-qcow2.sh \
	test-checksum-good.sh \
	test-checksum-print.sh \
	test-conversion-of.sh \
	test-customize.sh \
	test-fedora-btrfs-conversion.sh \
	test-fedora-conversion.sh \
	test-fedora-luks-on-lvm-conversion.sh \
	test-fedora-lvm-on-luks-conversion.sh \
	test-fedora-md-conversion.sh \
	test-floppy.expected \
	test-floppy.sh \
	test-i-disk-parallel.sh \
	test-i-disk.sh \
	test-i-ova-as-root.ovf \
	test-i-ova-as-root.sh \
	test-i-ova-bad-sha1.sh \
	test-i-ova-bad-sha256.sh \
	test-i-ova-checksums.ovf \
	test-i-ova-directory.sh \
	test-i-ova-formats.expected \
	test-i-ova-formats.ovf \
	test-i-ova-formats.sh \
	test-i-ova-good-checksums.sh \
	test-i-ova-gz.expected \
	test-i-ova-gz.ovf \
	test-i-ova-gz.sh \
	test-i-ova-invalid-manifest1.sh \
	test-i-ova-invalid-manifest2.sh \
	test-i-ova-snapshots.expected \
	test-i-ova-snapshots.expected2 \
	test-i-ova-snapshots.ovf \
	test-i-ova-snapshots.sh \
	test-i-ova-subfolders.expected \
	test-i-ova-subfolders.expected2 \
	test-i-ova-subfolders.ovf \
	test-i-ova-subfolders.sh \
	test-i-ova-tar.expected \
	test-i-ova-tar.expected2 \
	test-i-ova-tar.ovf \
	test-i-ova-tar.sh \
	test-i-ova-two-disks.expected \
	test-i-ova-two-disks.expected2 \
	test-i-ova-two-disks.ovf \
	test-i-ova-two-disks.sh \
	test-i-ova.ovf \
	test-i-ova.sh \
	test-i-ova.xml \
	test-i-vmx-1.expected \
	test-i-vmx-1.vmx \
	test-i-vmx-2.expected \
	test-i-vmx-2.vmx \
	test-i-vmx-3.expected \
	test-i-vmx-3.vmx \
	test-i-vmx-4.expected \
	test-i-vmx-4.vmx \
	test-i-vmx-5.expected \
	test-i-vmx-5.vmx \
	test-i-vmx-6.expected \
	test-i-vmx-6.vmx \
	test-i-vmx-7.expected \
	test-i-vmx-7.vmx \
	test-i-vmx.sh \
	test-in-place.sh \
	test-in-place-xml.sh \
	test-inspector.sh \
	test-it-vddk-io-query.sh \
	test-mac-expected.xml \
	test-mac.sh \
	test-machine-readable.sh \
	test-networks-and-bridges-expected.xml \
	test-networks-and-bridges.sh \
	test-o-glance.sh \
	test-o-kubevirt-fedora.sh \
	test-o-kubevirt-fedora.yaml.expected \
	test-o-kubevirt-oo-disk.sh \
	test-o-kubevirt-windows.sh \
	test-o-kubevirt-windows.yaml.expected \
	test-o-libvirt.sh \
	test-o-local-qcow2-compressed.sh \
	test-o-null.sh \
	test-o-openstack.sh \
	test-o-ovirt-upload-module/imageio.py \
	test-o-ovirt-upload-module/ovirtsdk4/__init__.py \
	test-o-ovirt-upload-module/ovirtsdk4/types.py \
	test-o-ovirt-upload-oo-query.sh \
	test-o-ovirt-upload.sh \
	test-o-ovirt.ovf.expected \
	test-o-ovirt.sh \
	test-o-qemu.sh \
	test-o-vdsm-oo-query.sh \
	test-o-vdsm-options.ovf.expected \
	test-o-vdsm-options.sh \
	test-oa-option-qcow2.sh \
	test-oa-option-raw.sh \
	test-of-option.sh \
	test-on-option.sh \
	test-open.sh \
	test-open-encrypted.sh \
	test-print-source.expected \
	test-print-source.sh \
	test-phony-win10-ls.txt \
	test-phony-win11-ls.txt \
	test-phony-win2k22-ls.txt \
	test-phony-win2k25-ls.txt \
	test-phony-win7-32-ls.txt \
	test-phony-winxp-32-ls.txt \
	test-reject-blank-disk.sh \
	test-rhbz1232192.sh \
	test-sound.sh \
	test-trim.sh \
	test-virtio-win-iso.sh \
	test-windows-conversion.sh \
	test-windows-conversion-ls.txt \
	test-windows-uefi-conversion.sh \
	test-windows-phony.sh
