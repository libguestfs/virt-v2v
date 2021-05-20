# libguestfs
# Copyright (C) 2013-2020 Red Hat Inc.
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

scripts = \
	virt-v2v

EXTRA_DIST = \
	README \
	$(scripts) \
	$(TESTS)

if HAVE_BASH_COMPLETION

# Bash completion script.

bashcompletiondir = $(BASH_COMPLETIONS_DIR)
#bashcompletion_DATA = $(scripts)

all-local: $(scripts)
	-test $(srcdir) != $(builddir) && cd $(srcdir) && cp $(scripts) $(abs_builddir)/

install-data-local:
	$(mkdir_p) $(DESTDIR)$(bashcompletiondir)
	cp -d $(scripts) $(DESTDIR)$(bashcompletiondir)

clean-local:
	-test $(srcdir) != $(builddir) && rm -f $(scripts)

endif

# Tests.
TESTS_ENVIRONMENT = \
	scripts="$(scripts)" \
	commands="$(scripts)" \
	$(top_builddir)/run --test

TESTS = \
	test-complete-in-script.sh
