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

BUILT_SOURCES = \
	output_rhv_upload_cancel_source.ml \
	output_rhv_upload_createvm_source.ml \
	output_rhv_upload_finalize_source.ml \
	output_rhv_upload_plugin_source.ml \
	output_rhv_upload_precheck_source.ml \
	output_rhv_upload_transfer_source.ml \
	output_rhv_upload_vmcheck_source.ml

EXTRA_DIST = \
	$(SOURCES_MLI) \
	$(SOURCES_ML) \
	$(SOURCES_C) \
	$(BUILT_SOURCES) \
	embed.sh \
	rhv-upload-cancel.py \
	rhv-upload-createvm.py \
	rhv-upload-finalize.py \
	rhv-upload-plugin.py \
	rhv-upload-precheck.py \
	rhv-upload-transfer.py \
	rhv-upload-vmcheck.py \
	test-v2v-python-syntax.sh \
	var_expander_tests.ml

SOURCES_MLI = \
	changeuid.mli \
	create_json.mli \
	create_libvirt_xml.mli \
	openstack_image_properties.mli \
	output.mli \
	output_rhv_upload_cancel_source.mli \
	output_rhv_upload_createvm_source.mli \
	output_rhv_upload_finalize_source.mli \
	output_rhv_upload_plugin_source.mli \
	output_rhv_upload_precheck_source.mli \
	output_rhv_upload_transfer_source.mli \
	output_rhv_upload_vmcheck_source.mli \
	python_script.mli \
	qemuopts.mli \
	var_expander.mli

SOURCES_ML = \
	changeuid.ml \
	python_script.ml \
	var_expander.ml \
	create_json.ml \
	create_libvirt_xml.ml \
	qemuopts.ml \
	openstack_image_properties.ml \
	output_rhv_upload_cancel_source.ml \
	output_rhv_upload_createvm_source.ml \
	output_rhv_upload_finalize_source.ml \
	output_rhv_upload_plugin_source.ml \
	output_rhv_upload_precheck_source.ml \
	output_rhv_upload_transfer_source.ml \
	output_rhv_upload_vmcheck_source.ml \
	output.ml

SOURCES_C = \
	qemuopts-c.c

# These files are generated and contain *.py embedded as an OCaml string.
output_rhv_upload_cancel_source.ml: $(srcdir)/rhv-upload-cancel.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_createvm_source.ml: $(srcdir)/rhv-upload-createvm.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_finalize_source.ml: $(srcdir)/rhv-upload-finalize.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_plugin_source.ml: $(srcdir)/rhv-upload-plugin.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_precheck_source.ml: $(srcdir)/rhv-upload-precheck.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_transfer_source.ml: $(srcdir)/rhv-upload-transfer.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_vmcheck_source.ml: $(srcdir)/rhv-upload-vmcheck.py
	$(srcdir)/embed.sh code $^ $@

bin_PROGRAMS = helper-v2v-output

helper_v2v_output_SOURCES = $(SOURCES_C)
helper_v2v_output_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/common/qemuopts \
	-I$(top_srcdir)/lib \
	-I$(top_srcdir)/v2v
helper_v2v_output_CFLAGS = \
	-pthread \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBGUESTFS_CFLAGS) \
	$(LIBXML2_CFLAGS) \
	$(JANSSON_CFLAGS)

BOBJECTS = $(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

OCAMLPACKAGES = \
	-package str,unix,guestfs,nbd \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/common/qemuopts/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/lib \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/bundled/libvirt-ocaml \
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
	$(JANSSON_LIBS) \
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

helper_v2v_output_DEPENDENCIES = \
	$(OBJECTS) \
	$(top_builddir)/lib/mlv2vlib.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
helper_v2v_output_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) $(OCAMLPACKAGES) $(OCAMLLINKFLAGS) \
	  $(OBJECTS) -o $@

# Unit tests.
TESTS =
if HAVE_OCAML_PKG_OUNIT
TESTS += \
	var_expander_tests
check_PROGRAMS = \
	var_expander_tests
endif

var_expander_tests_BOBJECTS = \
	var_expander.cmo \
	var_expander_tests.cmo
var_expander_tests_XOBJECTS = $(var_expander_tests_BOBJECTS:.cmo=.cmx)

var_expander_tests_SOURCES = dummy.c
var_expander_tests_CPPFLAGS = $(virt_v2v_CPPFLAGS)
var_expander_tests_CFLAGS = $(virt_v2v_CFLAGS)

if !HAVE_OCAMLOPT
# Can't call this var_expander_tests_OBJECTS because automake gets confused.
var_expander_tests_THEOBJECTS = $(var_expander_tests_BOBJECTS)
var_expander_tests.cmo: OCAMLPACKAGES += -package ounit2
else
var_expander_tests_THEOBJECTS = $(var_expander_tests_XOBJECTS)
var_expander_tests.cmx: OCAMLPACKAGES += -package ounit2
endif

var_expander_tests_DEPENDENCIES = \
	$(var_expander_tests_THEOBJECTS) \
	$(top_builddir)/common/mlpcre/mlpcre.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
var_expander_tests_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) \
	  $(OCAMLPACKAGES) -package ounit2 \
	  $(OCAMLLINKFLAGS) \
	  $(var_expander_tests_THEOBJECTS) -o $@

# Unit tests.
TESTS_ENVIRONMENT = $(top_builddir)/run --test
TESTS += test-v2v-python-syntax.sh

# Dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml
	$(top_builddir)/ocaml-dep.sh $^
-include .depend
