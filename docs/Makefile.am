# libguestfs virt-v2v tool
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
	test-docs.sh \
	virt-v2v.pod \
	virt-v2v-hacking.pod \
	virt-v2v-in-place.pod \
	virt-v2v-input-vmware.pod \
	virt-v2v-input-xen.pod \
	virt-v2v-inspector.pod \
	virt-v2v-open.pod \
	virt-v2v-output-local.pod \
	virt-v2v-output-openstack.pod \
	virt-v2v-output-ovirt.pod \
	virt-v2v-release-notes-1.42.pod \
	virt-v2v-release-notes-2.0.pod \
	virt-v2v-release-notes-2.2.pod \
	virt-v2v-release-notes-2.4.pod \
	virt-v2v-release-notes-2.6.pod \
	virt-v2v-release-notes-2.8.pod \
	virt-v2v-support.pod \
	vm-generation-id-across-hypervisors.txt

# Manual pages and HTML files for the website.

man_MANS = \
	virt-v2v.1 \
	virt-v2v-hacking.1 \
	virt-v2v-in-place.1 \
	virt-v2v-input-vmware.1 \
	virt-v2v-input-xen.1 \
	virt-v2v-inspector.1 \
	virt-v2v-open.1 \
	virt-v2v-output-local.1 \
	virt-v2v-output-openstack.1 \
	virt-v2v-output-ovirt.1 \
	virt-v2v-release-notes-1.42.1 \
	virt-v2v-release-notes-2.0.1 \
	virt-v2v-release-notes-2.2.1 \
	virt-v2v-release-notes-2.4.1 \
	virt-v2v-release-notes-2.6.1 \
	virt-v2v-release-notes-2.8.1 \
	virt-v2v-support.1

noinst_DATA = \
	$(top_builddir)/website/virt-v2v.1.html \
	$(top_builddir)/website/virt-v2v-hacking.1.html \
	$(top_builddir)/website/virt-v2v-in-place.1.html \
	$(top_builddir)/website/virt-v2v-input-vmware.1.html \
	$(top_builddir)/website/virt-v2v-input-xen.1.html \
	$(top_builddir)/website/virt-v2v-inspector.1.html \
	$(top_builddir)/website/virt-v2v-open.1.html \
	$(top_builddir)/website/virt-v2v-output-local.1.html \
	$(top_builddir)/website/virt-v2v-output-openstack.1.html \
	$(top_builddir)/website/virt-v2v-output-ovirt.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-1.42.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-2.0.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-2.2.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-2.4.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-2.6.1.html \
	$(top_builddir)/website/virt-v2v-release-notes-2.8.1.html \
	$(top_builddir)/website/virt-v2v-support.1.html

virt-v2v.1 $(top_builddir)/website/virt-v2v.1.html: stamp-virt-v2v.pod

stamp-virt-v2v.pod: virt-v2v.pod
	$(PODWRAPPER) \
	  --man virt-v2v.1 \
	  --html $(top_builddir)/website/virt-v2v.1.html \
	  --path $(top_srcdir)/common/options \
	  --insert $(top_srcdir)/common/mlcustomize/v2v-customize-options.pod:__CUSTOMIZE_OPTIONS__ \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-hacking.1 $(top_builddir)/website/virt-v2v-hacking.1.html: stamp-virt-v2v-hacking.pod

stamp-virt-v2v-hacking.pod: virt-v2v-hacking.pod
	$(PODWRAPPER) \
	  --man virt-v2v-hacking.1 \
	  --html $(top_builddir)/website/virt-v2v-hacking.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-in-place.1 $(top_builddir)/website/virt-v2v-in-place.1.html: stamp-virt-v2v-in-place.pod

stamp-virt-v2v-in-place.pod: virt-v2v-in-place.pod
	$(PODWRAPPER) \
	  --man virt-v2v-in-place.1 \
	  --html $(top_builddir)/website/virt-v2v-in-place.1.html \
	  --path $(top_srcdir)/common/options \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-input-vmware.1 $(top_builddir)/website/virt-v2v-input-vmware.1.html: stamp-virt-v2v-input-vmware.pod

stamp-virt-v2v-input-vmware.pod: virt-v2v-input-vmware.pod
	$(PODWRAPPER) \
	  --man virt-v2v-input-vmware.1 \
	  --html $(top_builddir)/website/virt-v2v-input-vmware.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-input-xen.1 $(top_builddir)/website/virt-v2v-input-xen.1.html: stamp-virt-v2v-input-xen.pod

stamp-virt-v2v-input-xen.pod: virt-v2v-input-xen.pod
	$(PODWRAPPER) \
	  --man virt-v2v-input-xen.1 \
	  --html $(top_builddir)/website/virt-v2v-input-xen.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-inspector.1 $(top_builddir)/website/virt-v2v-inspector.1.html: stamp-virt-v2v-inspector.pod

stamp-virt-v2v-inspector.pod: virt-v2v-inspector.pod
	$(PODWRAPPER) \
	  --man virt-v2v-inspector.1 \
	  --html $(top_builddir)/website/virt-v2v-inspector.1.html \
	  --path $(top_srcdir)/common/options \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-open.1 $(top_builddir)/website/virt-v2v-open.1.html: stamp-virt-v2v-open.pod

stamp-virt-v2v-open.pod: virt-v2v-open.pod
	$(PODWRAPPER) \
	  --man virt-v2v-open.1 \
	  --html $(top_builddir)/website/virt-v2v-open.1.html \
	  --path $(top_srcdir)/common/options \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-output-local.1 $(top_builddir)/website/virt-v2v-output-local.1.html: stamp-virt-v2v-output-local.pod

stamp-virt-v2v-output-local.pod: virt-v2v-output-local.pod
	$(PODWRAPPER) \
	  --man virt-v2v-output-local.1 \
	  --html $(top_builddir)/website/virt-v2v-output-local.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-output-openstack.1 $(top_builddir)/website/virt-v2v-output-openstack.1.html: stamp-virt-v2v-output-openstack.pod

stamp-virt-v2v-output-openstack.pod: virt-v2v-output-openstack.pod
	$(PODWRAPPER) \
	  --man virt-v2v-output-openstack.1 \
	  --html $(top_builddir)/website/virt-v2v-output-openstack.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-output-ovirt.1 $(top_builddir)/website/virt-v2v-output-ovirt.1.html: stamp-virt-v2v-output-ovirt.pod

stamp-virt-v2v-output-ovirt.pod: virt-v2v-output-ovirt.pod
	$(PODWRAPPER) \
	  --man virt-v2v-output-ovirt.1 \
	  --html $(top_builddir)/website/virt-v2v-output-ovirt.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-1.42.1 $(top_builddir)/website/virt-v2v-release-notes-1.42.1.html: stamp-virt-v2v-release-notes-1.42.pod

stamp-virt-v2v-release-notes-1.42.pod: virt-v2v-release-notes-1.42.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-1.42.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-1.42.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-2.0.1 $(top_builddir)/website/virt-v2v-release-notes-2.0.1.html: stamp-virt-v2v-release-notes-2.0.pod

stamp-virt-v2v-release-notes-2.0.pod: virt-v2v-release-notes-2.0.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-2.0.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-2.0.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-2.2.1 $(top_builddir)/website/virt-v2v-release-notes-2.2.1.html: stamp-virt-v2v-release-notes-2.2.pod

stamp-virt-v2v-release-notes-2.2.pod: virt-v2v-release-notes-2.2.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-2.2.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-2.2.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-2.4.1 $(top_builddir)/website/virt-v2v-release-notes-2.4.1.html: stamp-virt-v2v-release-notes-2.4.pod

stamp-virt-v2v-release-notes-2.4.pod: virt-v2v-release-notes-2.4.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-2.4.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-2.4.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-2.6.1 $(top_builddir)/website/virt-v2v-release-notes-2.6.1.html: stamp-virt-v2v-release-notes-2.6.pod

stamp-virt-v2v-release-notes-2.6.pod: virt-v2v-release-notes-2.6.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-2.6.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-2.6.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-release-notes-2.8.1 $(top_builddir)/website/virt-v2v-release-notes-2.8.1.html: stamp-virt-v2v-release-notes-2.8.pod

stamp-virt-v2v-release-notes-2.8.pod: virt-v2v-release-notes-2.8.pod
	$(PODWRAPPER) \
	  --man virt-v2v-release-notes-2.8.1 \
	  --html $(top_builddir)/website/virt-v2v-release-notes-2.8.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-support.1 $(top_builddir)/website/virt-v2v-support.1.html: stamp-virt-v2v-support.pod

stamp-virt-v2v-support.pod: virt-v2v-support.pod
	$(PODWRAPPER) \
	  --man virt-v2v-support.1 \
	  --html $(top_builddir)/website/virt-v2v-support.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

TESTS_ENVIRONMENT = $(top_builddir)/run --test
TESTS = test-docs.sh
