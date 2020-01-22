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

BUILT_SOURCES = \
	output_rhv_upload_createvm_source.ml \
	output_rhv_upload_deletedisks_source.ml \
	output_rhv_upload_plugin_source.ml \
	output_rhv_upload_precheck_source.ml \
	output_rhv_upload_vmcheck_source.ml

CONFIGURE_GENERATED_ML = \
	config.ml

EXTRA_DIST = \
	$(SOURCES_MLI) \
	$(filter-out $(CONFIGURE_GENERATED_ML) $(BUILT_SOURCES),$(SOURCES_ML)) \
	$(SOURCES_C) \
	copy_to_local.ml \
	copy_to_local.mli \
	embed.sh \
	rhv-upload-createvm.py \
	rhv-upload-deletedisks.py \
	rhv-upload-plugin.py \
	rhv-upload-precheck.py \
	rhv-upload-vmcheck.py \
	test-v2v-python-syntax.sh \
	var_expander_tests.ml \
	v2v_unit_tests.ml

SOURCES_MLI = \
	changeuid.mli \
	cmdline.mli \
	config.mli \
	convert_linux.mli \
	convert_windows.mli \
	create_json.mli \
	create_libvirt_xml.mli \
	create_ovf.mli \
	DOM.mli \
	input_disk.mli \
	input_libvirt.mli \
	input_libvirt_other.mli \
	input_libvirt_vcenter_https.mli \
	input_libvirt_vddk.mli \
	input_libvirt_xen_ssh.mli \
	input_libvirtxml.mli \
	input_ova.mli \
	input_vmx.mli \
	inspect_source.mli \
	libosinfo.mli \
	libosinfo_utils.mli \
	libvirt_utils.mli \
	linux.mli \
	linux_bootloaders.mli \
	linux_kernels.mli \
	measure_disk.mli \
	modules_list.mli \
	name_from_disk.mli \
	nbdkit.mli \
	networks.mli \
	openstack_image_properties.mli \
	output_glance.mli \
	output_json.mli \
	output_libvirt.mli \
	output_local.mli \
	output_null.mli \
	output_openstack.mli \
	output_qemu.mli \
	output_rhv.mli \
	output_rhv_upload.mli \
	output_rhv_upload_createvm_source.mli \
	output_rhv_upload_deletedisks_source.mli \
	output_rhv_upload_plugin_source.mli \
	output_rhv_upload_precheck_source.mli \
	output_rhv_upload_vmcheck_source.mli \
	output_vdsm.mli \
	parse_ova.mli \
	parse_ovf_from_ova.mli \
	parse_libvirt_xml.mli \
	parse_vmx.mli \
	python_script.mli \
	qemuopts.mli \
	target_bus_assignment.mli \
	types.mli \
	utils.mli \
	v2v.mli \
	vCenter.mli \
	var_expander.mli \
	windows.mli \
	windows_virtio.mli

SOURCES_ML = \
	$(CONFIGURE_GENERATED_ML) \
	types.ml \
	utils.ml \
	var_expander.ml \
	python_script.ml \
	name_from_disk.ml \
	nbdkit.ml \
	vCenter.ml \
	libosinfo.ml \
	libosinfo_utils.ml \
	libvirt_utils.ml \
	DOM.ml \
	changeuid.ml \
	parse_ovf_from_ova.ml \
	parse_ova.ml \
	create_ovf.ml \
	create_json.ml \
	linux.ml \
	windows.ml \
	windows_virtio.ml \
	modules_list.ml \
	input_disk.ml \
	parse_vmx.ml \
	parse_libvirt_xml.ml \
	create_libvirt_xml.ml \
	openstack_image_properties.ml \
	qemuopts.ml \
	input_libvirtxml.ml \
	input_libvirt_other.ml \
	input_libvirt_vcenter_https.ml \
	input_libvirt_vddk.ml \
	input_libvirt_xen_ssh.ml \
	input_libvirt.ml \
	input_ova.ml \
	input_vmx.ml \
	linux_bootloaders.ml \
	linux_kernels.ml \
	convert_linux.ml \
	convert_windows.ml \
	output_null.ml \
	output_glance.ml \
	output_json.ml \
	output_libvirt.ml \
	output_local.ml \
	output_qemu.ml \
	output_rhv.ml \
	output_rhv_upload_createvm_source.ml \
	output_rhv_upload_deletedisks_source.ml \
	output_rhv_upload_plugin_source.ml \
	output_rhv_upload_precheck_source.ml \
	output_rhv_upload_vmcheck_source.ml \
	output_rhv_upload.ml \
	output_vdsm.ml \
	output_openstack.ml \
	inspect_source.ml \
	target_bus_assignment.ml \
	measure_disk.ml \
	networks.ml \
	cmdline.ml \
	v2v.ml

SOURCES_C = \
	libosinfo-c.c \
	qemuopts-c.c

# These files are generated and contain *.py embedded as an OCaml string.
output_rhv_upload_createvm_source.ml: $(srcdir)/rhv-upload-createvm.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_deletedisks_source.ml: $(srcdir)/rhv-upload-deletedisks.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_plugin_source.ml: $(srcdir)/rhv-upload-plugin.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_precheck_source.ml: $(srcdir)/rhv-upload-precheck.py
	$(srcdir)/embed.sh code $^ $@
output_rhv_upload_vmcheck_source.ml: $(srcdir)/rhv-upload-vmcheck.py
	$(srcdir)/embed.sh code $^ $@

bin_PROGRAMS = virt-v2v virt-v2v-copy-to-local

virt_v2v_SOURCES = $(SOURCES_C)
virt_v2v_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/common/qemuopts \
	-I$(top_srcdir)/lib
virt_v2v_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBVIRT_CFLAGS) \
	$(LIBOSINFO_CFLAGS)

BOBJECTS = \
	$(top_builddir)/common/mlcustomize/firstboot.cmo \
	$(top_builddir)/common/mlcustomize/SELinux_relabel.cmo \
	$(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

# -I $(top_builddir)/lib/.libs is a hack which forces corresponding -L
# option to be passed to gcc, so we don't try linking against an
# installed copy of libguestfs.
OCAMLPACKAGES = \
	-package str,unix,guestfs \
	-I $(top_builddir)/common/utils/.libs \
	-I $(top_builddir)/common/qemuopts/.libs \
	-I $(top_builddir)/lib/.libs \
	-I $(top_builddir)/gnulib/lib/.libs \
	-I $(top_builddir)/common/mlstdutils \
	-I $(top_builddir)/common/mlutils \
	-I $(top_builddir)/common/mlgettext \
	-I $(top_builddir)/common/mlpcre \
	-I $(top_builddir)/common/mlxml \
	-I $(top_builddir)/common/mltools \
	-I $(top_builddir)/bundled/libvirt-ocaml \
	-I $(top_builddir)/common/mlcustomize \
	-I $(top_builddir)/common/mlv2v
if HAVE_OCAML_PKG_GETTEXT
OCAMLPACKAGES += -package gettext-stub
endif

OCAMLCLIBS = \
	-lqemuopts \
	$(LIBVIRT_LIBS) \
	$(LIBXML2_LIBS) \
	$(JANSSON_LIBS) \
	$(LIBOSINFO_LIBS) \
	$(LIBINTL) \
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
	mlv2v.$(MLARCHIVE) \
	$(LINK_CUSTOM_OCAMLC_ONLY)

virt_v2v_DEPENDENCIES = $(OBJECTS) $(top_srcdir)/ocaml-link.sh
virt_v2v_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) $(OCAMLPACKAGES) $(OCAMLLINKFLAGS) \
	  $(OBJECTS) -o $@

virt_v2v_copy_to_local_SOURCES = \
	dummy.c
virt_v2v_copy_to_local_CPPFLAGS = \
	-DCAML_NAME_SPACE \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where) \
	-I$(top_srcdir)/lib
virt_v2v_copy_to_local_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(LIBVIRT_CFLAGS)

COPY_TO_LOCAL_BOBJECTS = \
	config.cmo \
	types.cmo \
	utils.cmo \
	libvirt_utils.cmo \
	nbdkit.cmo \
	vCenter.cmo \
	copy_to_local.cmo
COPY_TO_LOCAL_XOBJECTS = $(COPY_TO_LOCAL_BOBJECTS:.cmo=.cmx)

if !HAVE_OCAMLOPT
COPY_TO_LOCAL_OBJECTS = $(COPY_TO_LOCAL_BOBJECTS)
else
COPY_TO_LOCAL_OBJECTS = $(COPY_TO_LOCAL_XOBJECTS)
endif

virt_v2v_copy_to_local_DEPENDENCIES = \
	$(COPY_TO_LOCAL_OBJECTS) \
	../common/mlstdutils/mlstdutils.$(MLARCHIVE) \
	../common/mlxml/mlxml.$(MLARCHIVE) \
	../common/mlgettext/mlgettext.$(MLARCHIVE) \
	../common/mlpcre/mlpcre.$(MLARCHIVE) \
	../common/mlutils/mlcutils.$(MLARCHIVE) \
	../common/mltools/mltools.$(MLARCHIVE) \
	../bundled/libvirt-ocaml/mllibvirt.$(MLARCHIVE) \
	../common/mlcustomize/mlcustomize.$(MLARCHIVE) \
	../common/mlv2v/mlv2v.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
virt_v2v_copy_to_local_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) $(OCAMLPACKAGES) $(OCAMLLINKFLAGS) \
	  $(COPY_TO_LOCAL_OBJECTS) -o $@

# Data directory.

virttoolsdatadir = $(datadir)/virt-tools

# Unit tests.
TESTS_ENVIRONMENT = $(top_builddir)/run --test
TESTS = test-v2v-python-syntax.sh

if HAVE_OCAML_PKG_OUNIT
TESTS += \
	v2v_unit_tests \
	var_expander_tests
check_PROGRAMS = \
	v2v_unit_tests \
	var_expander_tests
endif

v2v_unit_tests_BOBJECTS = \
	config.cmo \
	types.cmo \
	utils.cmo \
	libosinfo.cmo \
	libosinfo_utils.cmo \
	DOM.cmo \
	create_ovf.cmo \
	windows.cmo \
	linux.cmo \
	windows_virtio.cmo \
	parse_vmx.cmo \
	v2v_unit_tests.cmo
v2v_unit_tests_XOBJECTS = $(v2v_unit_tests_BOBJECTS:.cmo=.cmx)

v2v_unit_tests_SOURCES = $(virt_v2v_SOURCES)
v2v_unit_tests_CPPFLAGS = $(virt_v2v_CPPFLAGS)
v2v_unit_tests_CFLAGS = $(virt_v2v_CFLAGS)

var_expander_tests_BOBJECTS = \
	var_expander.cmo \
	var_expander_tests.cmo
var_expander_tests_XOBJECTS = $(var_expander_tests_BOBJECTS:.cmo=.cmx)

var_expander_tests_SOURCES = dummy.c
var_expander_tests_CPPFLAGS = $(virt_v2v_CPPFLAGS)
var_expander_tests_CFLAGS = $(virt_v2v_CFLAGS)

if !HAVE_OCAMLOPT
# Can't call this v2v_unit_tests_OBJECTS because automake gets confused.
v2v_unit_tests_THEOBJECTS = $(v2v_unit_tests_BOBJECTS)
v2v_unit_tests.cmo: OCAMLPACKAGES += -package oUnit

var_expander_tests_THEOBJECTS = $(var_expander_tests_BOBJECTS)
var_expander_tests.cmo: OCAMLPACKAGES += -package oUnit
else
v2v_unit_tests_THEOBJECTS = $(v2v_unit_tests_XOBJECTS)
v2v_unit_tests.cmx: OCAMLPACKAGES += -package oUnit

var_expander_tests_THEOBJECTS = $(var_expander_tests_XOBJECTS)
var_expander_tests.cmx: OCAMLPACKAGES += -package oUnit
endif

v2v_unit_tests_DEPENDENCIES = \
	$(v2v_unit_tests_THEOBJECTS) \
	../common/mlstdutils/mlstdutils.$(MLARCHIVE) \
	../common/mlxml/mlxml.$(MLARCHIVE) \
	../common/mlgettext/mlgettext.$(MLARCHIVE) \
	../common/mlpcre/mlpcre.$(MLARCHIVE) \
	../common/mlutils/mlcutils.$(MLARCHIVE) \
	../common/mltools/mltools.$(MLARCHIVE) \
	../common/mlcustomize/mlcustomize.$(MLARCHIVE) \
	../common/mlv2v/mlv2v.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
v2v_unit_tests_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) \
	  $(OCAMLPACKAGES) -package oUnit \
	  $(OCAMLLINKFLAGS) \
	  $(v2v_unit_tests_THEOBJECTS) -o $@

var_expander_tests_DEPENDENCIES = \
	$(var_expander_tests_THEOBJECTS) \
	../common/mlpcre/mlpcre.$(MLARCHIVE) \
	$(top_srcdir)/ocaml-link.sh
var_expander_tests_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) \
	  $(OCAMLPACKAGES) -package oUnit \
	  $(OCAMLLINKFLAGS) \
	  $(var_expander_tests_THEOBJECTS) -o $@

# Dependencies.
.depend: \
	$(srcdir)/*.mli \
	$(srcdir)/*.ml \
	$(CONFIGURE_GENERATED_ML) \
	$(filter %.ml,$(BUILT_SOURCES))
	$(top_builddir)/ocaml-dep.sh $^
-include .depend
