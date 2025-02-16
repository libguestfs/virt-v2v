# helper-v2v-input tool
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
	$(SOURCES_ML)

SOURCES_MLI = \
	input.mli \
	input_disk.mli \
	input_libvirt.mli \
	input_ova.mli \
	input_vcenter_https.mli \
	input_vddk.mli \
	input_vmx.mli \
	input_xen_ssh.mli \
	name_from_disk.mli \
	nbdkit_curl.mli \
	nbdkit_ssh.mli \
	nbdkit_vddk.mli \
	OVA.mli \
	OVF.mli \
	parse_domain_from_vmx.mli \
	parse_libvirt_xml.mli \
	parse_vmx.mli \
	ssh.mli \
	vCenter.mli

SOURCES_ML = \
	name_from_disk.ml \
	parse_libvirt_xml.ml \
	OVF.ml \
	OVA.ml \
	nbdkit_curl.ml \
	nbdkit_ssh.ml \
	nbdkit_vddk.ml \
	ssh.ml \
	parse_vmx.ml \
	parse_domain_from_vmx.ml \
	vCenter.ml \
	input.ml \
	input_disk.ml \
	input_libvirt.ml \
	input_ova.ml \
	input_vcenter_https.ml \
	input_vddk.ml \
	input_vmx.ml \
	input_xen_ssh.ml

# We pretend that we're building a C library.  automake handles the
# compilation of the C sources for us.  At the end we take the C
# objects and OCaml objects and link them into the OCaml library.
# This C library is never used.

noinst_LIBRARIES = libmlinput.a

if !HAVE_OCAMLOPT
MLINPUT_CMA = mlinput.cma
else
MLINPUT_CMA = mlinput.cmxa
endif

noinst_DATA = $(MLINPUT_CMA)

libmlinput_a_SOURCES = dummy.c
libmlinput_a_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/lib
libmlinput_a_CFLAGS = \
	-pthread \
	-fPIC \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBGUESTFS_CFLAGS)

BOBJECTS = $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix,guestfs,libvirt \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/lib \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/common/mlxml
if HAVE_OCAML_PKG_GETTEXT
OCAMLPACKAGES += -package gettext-stub
endif

OCAMLCLIBS = \
	-pthread \
	$(LIBGUESTFS_LIBS) \
	$(LIBXML2_LIBS) \
	-lgnu

OCAMLFLAGS = $(OCAML_FLAGS) $(OCAML_WARN_ERROR) -ccopt '$(CFLAGS)'

if !HAVE_OCAMLOPT
OBJECTS = $(BOBJECTS)
else
OBJECTS = $(XOBJECTS)
endif

OCAMLLINKFLAGS = \
	mlstdutils.$(MLARCHIVE) \
	mlcutils.$(MLARCHIVE) \
	mlgettext.$(MLARCHIVE) \
	mlpcre.$(MLARCHIVE) \
	mlxml.$(MLARCHIVE) \
	mltools.$(MLARCHIVE) \
	mllibvirt.$(MLARCHIVE) \
	mlv2vlib.$(MLARCHIVE) \
	$(LINK_CUSTOM_OCAMLC_ONLY)

libmlinput_a_DEPENDENCIES = \
	$(OBJECTS)

$(MLINPUT_CMA): $(OBJECTS) libmlinput.a
	$(OCAMLFIND) mklib $(OCAMLPACKAGES) \
	    $(OBJECTS) $(libmlinput_a_OBJECTS) -o mlinput

# Dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml
	$(top_builddir)/ocaml-dep.sh $^
-include .depend
