# helper-v2v-output
# Copyright (C) 2009-2021 Red Hat Inc.
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

BUILT_SOURCES =

EXTRA_DIST = \
	$(SOURCES_MLI) \
	$(SOURCES_ML) \
	$(SOURCES_C) \
	$(BUILT_SOURCES) \
	embed.sh \
	test-v2v-python-syntax.sh

SOURCES_MLI = \
	changeuid.mli \
	create_libvirt_xml.mli \
	create_kubevirt_yaml.mli \
	openstack_image_properties.mli \
	output.mli \
	output_disk.mli \
	output_glance.mli \
	output_kubevirt.mli \
	output_libvirt.mli \
	output_null.mli \
	output_openstack.mli \
	output_qemu.mli \
	python_script.mli \
	qemuopts.mli

SOURCES_ML = \
	changeuid.ml \
	python_script.ml \
	create_libvirt_xml.ml \
	create_kubevirt_yaml.ml \
	qemuopts.ml \
	openstack_image_properties.ml \
	output.ml \
	output_disk.ml \
	output_glance.ml \
	output_kubevirt.ml \
	output_libvirt.ml \
	output_null.ml \
	output_openstack.ml \
	output_qemu.ml

SOURCES_C = \
	qemuopts-c.c

# We pretend that we're building a C library.  automake handles the
# compilation of the C sources for us.  At the end we take the C
# objects and OCaml objects and link them into the OCaml library.
# This C library is never used.

noinst_LIBRARIES = libmloutput.a

if !HAVE_OCAMLOPT
MLOUTPUT_CMA = mloutput.cma
else
MLOUTPUT_CMA = mloutput.cmxa
endif

noinst_DATA = $(MLOUTPUT_CMA)

libmloutput_a_SOURCES = $(SOURCES_C)
libmloutput_a_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/common/qemuopts \
	-I$(top_srcdir)/lib
libmloutput_a_CFLAGS = \
	-pthread \
	-fPIC \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBGUESTFS_CFLAGS) \
	$(LIBXML2_CFLAGS) \
	$(JSON_C_CFLAGS)

BOBJECTS = $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix,guestfs,libvirt,nbd \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/common/qemuopts/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/lib \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/common/mlv2v \
	-I $(top_builddir)/common/mlxml
if HAVE_OCAML_PKG_GETTEXT
OCAMLPACKAGES += -package gettext-stub
endif

OCAMLCLIBS = \
	-pthread \
	-lqemuopts \
	$(LIBGUESTFS_LIBS) \
	$(LIBXML2_LIBS) \
	$(JSON_C_LIBS) \
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
	mlcutils.$(MLARCHIVE) \
	mlgettext.$(MLARCHIVE) \
	mlpcre.$(MLARCHIVE) \
	mlxml.$(MLARCHIVE) \
	mltools.$(MLARCHIVE) \
	mllibvirt.$(MLARCHIVE) \
	mlv2v.$(MLARCHIVE) \
	mlv2vlib.$(MLARCHIVE) \
	$(LINK_CUSTOM_OCAMLC_ONLY)

libmloutput_a_DEPENDENCIES = \
	$(OBJECTS)

$(MLOUTPUT_CMA): $(OBJECTS) libmloutput.a
	$(OCAMLFIND) mklib $(OCAMLPACKAGES) \
	    $(OBJECTS) $(libmloutput_a_OBJECTS) -o mloutput

# Unit tests.
TESTS_ENVIRONMENT = $(top_builddir)/run --test
TESTS = test-v2v-python-syntax.sh

# Dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml
	$(top_builddir)/ocaml-dep.sh $^
-include .depend
