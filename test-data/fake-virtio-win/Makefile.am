# libguestfs test images
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

# Fake virtio-win (Windows virtio) drivers.
#
# Tests in the v2v subdirectory can set VIRTIO_WIN to point to this
# directory, or to the .iso file in this directory, in order to test
# conversions.
#
# Generate virtio-win-drivers-list.txt with:
#   cd /usr/share/virtio-win && find drivers -type f | sort
#
# Generate virtio-win-iso-list.txt with:
#   mkdir cd
#   sudo mount /usr/share/virtio-win/virtio-win.iso cd
#   find cd -type f | sort

include $(top_srcdir)/subdir-rules.mk

EXTRA_DIST = virtio-win-drivers-list.txt virtio-win-iso-list.txt

check_DATA = drivers cd fake-virtio-win.iso

CLEANFILES += fake-virtio-win.iso

clean-local:
	-rm -rf drivers
	-rm -rf cd

drivers: virtio-win-drivers-list.txt
	while read line; do			\
		mkdir -p `dirname $$line`;	\
		touch "$$line";			\
	done < $<

cd: virtio-win-iso-list.txt
	while read line; do			\
		mkdir -p `dirname $$line`;	\
		touch "$$line";			\
	done < $<

fake-virtio-win.iso: cd
	$(MKISOFS) -J -r -o $@ $(srcdir)/cd
