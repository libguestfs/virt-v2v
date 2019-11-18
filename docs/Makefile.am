# libguestfs virt-v2v tool
# Copyright (C) 2009-2019 Red Hat Inc.
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
	test-v2v-docs.sh \
	virt-v2v.pod \
	virt-v2v-copy-to-local.pod \
	virt-v2v-hacking.pod \
	virt-v2v-input-vmware.pod \
	virt-v2v-input-xen.pod \
	virt-v2v-output-local.pod \
	virt-v2v-output-openstack.pod \
	virt-v2v-output-rhv.pod \
	virt-v2v-support.pod

# Manual pages and HTML files for the website.

man_MANS = \
	virt-v2v.1 \
	virt-v2v-copy-to-local.1 \
	virt-v2v-hacking.1 \
	virt-v2v-input-vmware.1 \
	virt-v2v-input-xen.1 \
	virt-v2v-output-local.1 \
	virt-v2v-output-openstack.1 \
	virt-v2v-output-rhv.1 \
	virt-v2v-support.1

noinst_DATA = \
	$(top_builddir)/website/virt-v2v.1.html \
	$(top_builddir)/website/virt-v2v-copy-to-local.1.html \
	$(top_builddir)/website/virt-v2v-hacking.1.html \
	$(top_builddir)/website/virt-v2v-input-vmware.1.html \
	$(top_builddir)/website/virt-v2v-input-xen.1.html \
	$(top_builddir)/website/virt-v2v-output-local.1.html \
	$(top_builddir)/website/virt-v2v-output-openstack.1.html \
	$(top_builddir)/website/virt-v2v-output-rhv.1.html \
	$(top_builddir)/website/virt-v2v-support.1.html

virt-v2v.1 $(top_builddir)/website/virt-v2v.1.html: stamp-virt-v2v.pod

stamp-virt-v2v.pod: virt-v2v.pod
	$(PODWRAPPER) \
	  --man virt-v2v.1 \
	  --html $(top_builddir)/website/virt-v2v.1.html \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	touch $@

virt-v2v-copy-to-local.1 $(top_builddir)/website/virt-v2v-copy-to-local.1.html: stamp-virt-v2v-copy-to-local.pod

stamp-virt-v2v-copy-to-local.pod: virt-v2v-copy-to-local.pod
	$(PODWRAPPER) \
	  --man virt-v2v-copy-to-local.1 \
	  --html $(top_builddir)/website/virt-v2v-copy-to-local.1.html \
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

virt-v2v-output-rhv.1 $(top_builddir)/website/virt-v2v-output-rhv.1.html: stamp-virt-v2v-output-rhv.pod

stamp-virt-v2v-output-rhv.pod: virt-v2v-output-rhv.pod
	$(PODWRAPPER) \
	  --man virt-v2v-output-rhv.1 \
	  --html $(top_builddir)/website/virt-v2v-output-rhv.1.html \
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
TESTS = test-v2v-docs.sh
