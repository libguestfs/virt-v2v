#!/bin/bash -
# libguestfs
# Copyright (C) 2016 Red Hat Inc.
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

set -e

source ../tests/functions.sh
set -e
set -x

skip_if_skipped

# We don't explicitly document the virt-customize options in the
# synopsis, so don't give an error in the test.
virt_customize_options=\
--append-line,\
--chmod,\
--chown,\
--commands-from-file,\
--copy,\
--copy-in,\
--delete,\
--edit,\
--firstboot,\
--firstboot-command,\
--firstboot-install,\
--hostname,\
--inject-blnsvr,\
--inject-qemu-ga,\
--inject-virtio-win,\
--install,\
--link,\
--mkdir,\
--move,\
--no-logfile,\
--no-selinux-relabel,\
--password,\
--password-crypto,\
--root-password,\
--run,\
--run-command,\
--scrub,\
--selinux-relabel,\
--sm-attach,\
--sm-credentials,\
--sm-register,\
--sm-remove,\
--sm-unregister,\
--ssh-inject,\
--tar-in,\
--timezone,\
--touch,\
--truncate,\
--truncate-recursive,\
--uninstall,\
--update,\
--upload,\
--write

$srcdir/../podcheck.pl virt-v2v.pod virt-v2v \
  --path $srcdir/../common/options \
  --ignore=\
--debug-overlay,\
--ic,\
--if,\
--io,\
--ip,\
--it,\
--in-place,\
--no-trim,\
--password-file,\
--oa,\
--oc,\
--of,\
--on,\
--oo,\
--op,\
--os,\
--vddk-config,\
--vddk-cookie,\
--vddk-libdir,\
--vddk-nfchostport,\
--vddk-port,\
--vddk-snapshot,\
--vddk-thumbprint,\
--vddk-transports,\
--vdsm-compat,\
--vdsm-image-uuid,\
--vdsm-ovf-flavour,\
--vdsm-ovf-output,\
--vdsm-vm-uuid,\
--vdsm-vol-uuid,\
--vmtype,\
$virt_customize_options

$srcdir/../podcheck.pl virt-v2v-in-place.pod virt-v2v-in-place \
  --path $srcdir/../common/options \
  --ignore=\
--ic,\
--if,\
--io,\
--ip,\
--it,\
--password-file,\
--oa,\
--oc,\
--of,\
--on,\
--oo,\
--op,\
--os,\
$virt_customize_options

$srcdir/../podcheck.pl virt-v2v-inspector.pod virt-v2v-inspector \
  --path $srcdir/../common/options \
  --ignore=\
--ic,\
--if,\
--io,\
--ip,\
--it,\
$virt_customize_options
