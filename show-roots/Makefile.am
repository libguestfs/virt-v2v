# libguestfs virt-v2v-show-roots tool
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
	$(SOURCES_MLI) \
	$(SOURCES_ML) \
	$(SOURCES_C)

SOURCES_MLI = \
	show_roots.mli

SOURCES_ML = \
	show_roots.ml

SOURCES_C = \
	dummy.c

bin_PROGRAMS = virt-v2v-show-roots

virt_v2v_show_roots_SOURCES = $(SOURCES_C)
virt_v2v_show_roots_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/lib
virt_v2v_show_roots_CFLAGS = \
	-pthread \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBGUESTFS_CFLAGS) \
	$(LIBVIRT_CFLAGS)

BOBJECTS = $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix,guestfs,libvirt,nbd \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/common/qemuopts/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/lib \
	-I $(top_builddir)/input \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mlxml \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/common/mlcustomize \
	-I $(top_builddir)/common/mldrivers
if HAVE_OCAML_PKG_GETTEXT
OCAMLPACKAGES += -package gettext-stub
endif

OCAMLCLIBS = \
	-pthread \
	-lqemuopts \
	$(LIBGUESTFS_LIBS) \
	$(LIBVIRT_LIBS) \
	$(LIBCRYPT_LIBS) \
	$(LIBXML2_LIBS) \
	$(JSON_C_LIBS) \
	$(LIBOSINFO_LIBS) \
	$(LIBINTL) \
	$(LIBNBD_LIBS) \
	-lgnu

OCAMLFLAGS = $(OCAML_FLAGS) $(OCAML_WARN_ERROR) -ccopt '$(CFLAGS)'

if !HAVE_OCAMLOPT
OBJECTS = $(BOBJECTS)
else
OBJECTS = $(XOBJECTS)
endif

OCAMLLINKFLAGS = \
	mlstdutils.$(MLARCHIVE) \
	mlgettext.$(MLARCHIVE) \
	mlpcre.$(MLARCHIVE) \
	mlxml.$(MLARCHIVE) \
	mlcutils.$(MLARCHIVE) \
	mltools.$(MLARCHIVE) \
	mllibvirt.$(MLARCHIVE) \
	mlcustomize.$(MLARCHIVE) \
	mldrivers.$(MLARCHIVE) \
	mlv2vlib.$(MLARCHIVE) \
	mlinput.$(MLARCHIVE) \
	$(LINK_CUSTOM_OCAMLC_ONLY)

virt_v2v_show_roots_DEPENDENCIES = \
	$(OBJECTS) \
	$(top_builddir)/input/mlinput.$(MLARCHIVE) \
	$(top_builddir)/lib/mlv2vlib.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
virt_v2v_show_roots_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) $(OCAMLPACKAGES) $(OCAMLLINKFLAGS) \
	  $(OBJECTS) -o $@

# Data directory.

virttoolsdatadir = $(datadir)/virt-tools

# Dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml \
	$(filter %.ml,$(BUILT_SOURCES))
	$(top_builddir)/ocaml-dep.sh $^
-include .depend
