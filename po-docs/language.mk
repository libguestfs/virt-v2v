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
CLEANFILES += *.pl

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

%.1: %.pod
	$(PODWRAPPER) \
	  --no-strict-checks \
	  --man $@ \
	  $<

# If a POD file is missing, the user needs to run make update-po.
# This cannot be done automatically by make because it would be unsafe
# to run po4a or update podfiles potentially in parallel.  Therefore
# tell the user what to do and stop.
$(podfiles):
	@if ! test -f $@; then \
	  echo "***"; \
	  echo "*** You need to run the following commands:"; \
	  echo "***     rm po-docs/podfiles; make -C po-docs update-po"; \
	  echo "*** After that, rerun make."; \
	  echo "***"; \
	  exit 1; \
	fi

# XXX Can automake do this properly?
install-data-hook:
	$(MKDIR_P) $(DESTDIR)$(mandir)/$(LINGUA)/man1
	$(INSTALL) -m 0644 $(srcdir)/*.1 $(DESTDIR)$(mandir)/$(LINGUA)/man1
