# libguestfs translations of man pages and POD files
# Copyright (C) 2010-2012 Red Hat Inc.
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

# Common logic for generating translated documentation.

include $(top_srcdir)/subdir-rules.mk

LINGUA = $(shell basename -- `pwd`)

# Before 1.23.23, the old Perl tools were called *.pl.
CLEANFILES += *.pl *.pod

MANPAGES = \
	virt-v2v.1 \
	virt-v2v-copy-to-local.1 \
	virt-v2v-input-vmware.1 \
	virt-v2v-input-xen.1 \
	virt-v2v-output-local.1 \
	virt-v2v-output-openstack.1 \
	virt-v2v-output-rhv.1 \
	virt-v2v-support.1 \
	virt-v2v-test-harness.1

podfiles := $(shell for f in `cat $(top_srcdir)/po-docs/podfiles`; do echo `basename $$f .pod`.pod; done)

# Ship the POD files and the translated manpages in the tarball.  This
# just simplifies building from the tarball, at a small cost in extra
# size.
EXTRA_DIST = \
	$(MANPAGES) \
	$(podfiles)

all-local: $(MANPAGES)

virt-v2v.1: key-option.pod keys-from-stdin-option.pod

%.1: %.pod
	$(PODWRAPPER) \
	  --no-strict-checks \
	  --man $@ \
	  $<

# Note: po4a puts the following junk at the top of every POD file it
# generates:
#  - a warning
#  - a probably bogus =encoding line
# Remove both.
# XXX Fix po4a so it doesn't do this.
%.pod: $(srcdir)/../$(LINGUA).po
	$(guestfs_am_v_po4a_translate)$(PO4A_TRANSLATE) \
	  -f pod \
	  -M utf-8 -L utf-8 \
	  -k 0 \
	  -m $(top_srcdir)/$(shell grep '/$(notdir $@)$$' $(top_srcdir)/po-docs/podfiles) \
	  -p $< \
	  | $(SED) '0,/^=encoding/d' > $@

# XXX Can automake do this properly?
install-data-hook:
	$(MKDIR_P) $(DESTDIR)$(mandir)/$(LINGUA)/man1
	$(INSTALL) -m 0644 $(srcdir)/*.1 $(DESTDIR)$(mandir)/$(LINGUA)/man1
