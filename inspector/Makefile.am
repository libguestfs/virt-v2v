# libguestfs virt-v2v-inspector tool
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
	$(SOURCES_MLI) \
	$(SOURCES_ML) \
	$(SOURCES_C) \
	virt-v2v-inspector.pod \
	$(NULL)

SOURCES_MLI = \
	create_inspector_xml.mli \
	inspector.mli \
	$(NULL)

SOURCES_ML = \
	create_inspector_xml.ml \
	inspector.ml \
	$(NULL)

SOURCES_C = dummy.c

bin_PROGRAMS = virt-v2v-inspector

virt_v2v_inspector_SOURCES = $(SOURCES_C)
virt_v2v_inspector_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/lib \
	$(NULL)
virt_v2v_inspector_CFLAGS = \
	-pthread \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBGUESTFS_CFLAGS) \
	$(LIBVIRT_CFLAGS) \
	$(NULL)

BOBJECTS = $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix,guestfs,libvirt,nbd \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/common/qemuopts/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/lib \
	-I $(top_builddir)/input \
	-I $(top_builddir)/convert \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mlxml \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/common/mlcustomize \
	-I $(top_builddir)/common/mldrivers \
	$(NULL)
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
	-lgnu \
	$(NULL)

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
	mlconvert.$(MLARCHIVE) \
	mlinput.$(MLARCHIVE) \
	$(LINK_CUSTOM_OCAMLC_ONLY) \
	$(NULL)

virt_v2v_inspector_DEPENDENCIES = \
	$(OBJECTS) \
	$(top_builddir)/input/mlinput.$(MLARCHIVE) \
	$(top_builddir)/convert/mlconvert.$(MLARCHIVE) \
	$(top_builddir)/lib/mlv2vlib.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh \
	$(NULL)
virt_v2v_inspector_LINK = \
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

# Manual pages.

man_MANS = virt-v2v-inspector.1
noinst_DATA = $(top_builddir)/website/virt-v2v-inspector.1.html

virt-v2v-inspector.1 $(top_builddir)/website/virt-v2v-inspector.1.html: stamp-virt-v2v-inspector.pod

stamp-virt-v2v-inspector.pod: virt-v2v-inspector.pod
	$(PODWRAPPER) \
	  --man virt-v2v-inspector.1 \
	  --html $(top_builddir)/website/virt-v2v-inspector.1.html \
	  --path $(top_srcdir)/common/options \
	  --license GPLv2+ \
	  --warning safe \
	  $<
	$(AM_V_at)touch $@

TESTS_ENVIRONMENT = $(top_builddir)/run --test
TESTS = test-docs.sh
