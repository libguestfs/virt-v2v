# Library code used by virt-v2v and helpers.
# Copyright (C) 2011-2021 Red Hat Inc.
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

CONFIGURE_GENERATED_ML = \
	config.ml

# lib/guestfs-internal-all.h is a hack, remove it eventually XXX
EXTRA_DIST = \
	guestfs-internal-all.h \
	$(SOURCES_MLI) \
	$(SOURCES_ML) \
	$(SOURCES_C)

SOURCES_MLI = \
	config.mli \
	create_ovf.mli \
	DOM.mli \
	libvirt_utils.mli \
	nbdkit.mli \
	networks.mli \
	qemuNBD.mli \
	types.mli \
	utils.mli

SOURCES_ML = \
	types.ml \
	utils.ml \
	nbdkit.ml \
	qemuNBD.ml \
	networks.ml \
	DOM.ml \
	create_ovf.ml \
	libvirt_utils.ml

SOURCES_C = \
	dummy.c

# We pretend that we're building a C library.  automake handles the
# compilation of the C sources for us.  At the end we take the C
# objects and OCaml objects and link them into the OCaml library.
# This C library is never used.

noinst_LIBRARIES = libmlv2vlib.a

if !HAVE_OCAMLOPT
MLV2VLIB_CMA = mlv2vlib.cma
else
MLV2VLIB_CMA = mlv2vlib.cmxa
endif

noinst_DATA = $(MLV2VLIB_CMA)

libmlv2vlib_a_SOURCES = $(SOURCES_C)
libmlv2vlib_a_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where)
libmlv2vlib_a_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	-fPIC

BOBJECTS = config.cmo $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix \
	-I $(builddir) \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/bundled/libvirt-ocaml \
	-I $(top_builddir)/common/mlv2v \
	-I $(top_builddir)/common/mlxml
OCAMLPACKAGES_TESTS = $(MLV2VLIB_CMA)

OCAMLFLAGS = $(OCAML_FLAGS) $(OCAML_WARN_ERROR) -ccopt '$(CFLAGS)'

if !HAVE_OCAMLOPT
OBJECTS = $(BOBJECTS)
else
OBJECTS = $(XOBJECTS)
endif

libmlv2vlib_a_DEPENDENCIES = $(OBJECTS)

$(MLV2VLIB_CMA): $(OBJECTS) libmlv2vlib.a
	$(OCAMLFIND) mklib $(OCAMLPACKAGES) \
	    $(OBJECTS) $(libmlv2vlib_a_OBJECTS) -o mlv2vlib

# OCaml dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml \
	$(CONFIGURE_GENERATED_ML)
	$(top_builddir)/ocaml-dep.sh $^
-include .depend

.PHONY: docs
