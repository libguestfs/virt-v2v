# libguestfs test images
# Copyright (C) 2009-2024 Red Hat Inc.
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
# Note that the "drivers" are not real!

include $(top_srcdir)/subdir-rules.mk

drivers = \
	drivers/amd64/Win2003/netkvm.cat \
	drivers/amd64/Win2003/netkvm.inf \
	drivers/amd64/Win2003/netkvm.sys \
	drivers/amd64/Win2003/viostor.cat \
	drivers/amd64/Win2003/viostor.inf \
	drivers/amd64/Win2003/viostor.sys \
	drivers/amd64/Win2008/netkvm.cat \
	drivers/amd64/Win2008/netkvm.inf \
	drivers/amd64/Win2008/netkvm.sys \
	drivers/amd64/Win2008/vioscsi.cat \
	drivers/amd64/Win2008/vioscsi.inf \
	drivers/amd64/Win2008/vioscsi.sys \
	drivers/amd64/Win2008/viostor.cat \
	drivers/amd64/Win2008/viostor.inf \
	drivers/amd64/Win2008/viostor.sys \
	drivers/amd64/Win2008R2/netkvm.cat \
	drivers/amd64/Win2008R2/netkvm.inf \
	drivers/amd64/Win2008R2/netkvm.sys \
	drivers/amd64/Win2008R2/vioscsi.cat \
	drivers/amd64/Win2008R2/vioscsi.inf \
	drivers/amd64/Win2008R2/vioscsi.sys \
	drivers/amd64/Win2008R2/viostor.cat \
	drivers/amd64/Win2008R2/viostor.inf \
	drivers/amd64/Win2008R2/viostor.sys \
	drivers/amd64/Win2012/netkvm.cat \
	drivers/amd64/Win2012/netkvm.inf \
	drivers/amd64/Win2012/netkvm.sys \
	drivers/amd64/Win2012/vioscsi.cat \
	drivers/amd64/Win2012/vioscsi.inf \
	drivers/amd64/Win2012/vioscsi.sys \
	drivers/amd64/Win2012/viostor.cat \
	drivers/amd64/Win2012/viostor.inf \
	drivers/amd64/Win2012/viostor.sys \
	drivers/amd64/Win2012R2/netkvm.cat \
	drivers/amd64/Win2012R2/netkvm.inf \
	drivers/amd64/Win2012R2/netkvm.sys \
	drivers/amd64/Win2012R2/vioscsi.cat \
	drivers/amd64/Win2012R2/vioscsi.inf \
	drivers/amd64/Win2012R2/vioscsi.sys \
	drivers/amd64/Win2012R2/viostor.cat \
	drivers/amd64/Win2012R2/viostor.inf \
	drivers/amd64/Win2012R2/viostor.sys \
	drivers/amd64/Win2022/netkvm.cat \
	drivers/amd64/Win2022/netkvm.inf \
	drivers/amd64/Win2022/netkvm.sys \
	drivers/amd64/Win2022/vioscsi.cat \
	drivers/amd64/Win2022/vioscsi.inf \
	drivers/amd64/Win2022/vioscsi.sys \
	drivers/amd64/Win2022/viostor.cat \
	drivers/amd64/Win2022/viostor.inf \
	drivers/amd64/Win2022/viostor.sys \
	drivers/amd64/Win7/netkvm.cat \
	drivers/amd64/Win7/netkvm.inf \
	drivers/amd64/Win7/netkvm.sys \
	drivers/amd64/Win7/vioscsi.cat \
	drivers/amd64/Win7/vioscsi.inf \
	drivers/amd64/Win7/vioscsi.sys \
	drivers/amd64/Win7/viostor.cat \
	drivers/amd64/Win7/viostor.inf \
	drivers/amd64/Win7/viostor.sys \
	drivers/amd64/Win8.1/netkvm.cat \
	drivers/amd64/Win8.1/netkvm.inf \
	drivers/amd64/Win8.1/netkvm.sys \
	drivers/amd64/Win8.1/vioscsi.cat \
	drivers/amd64/Win8.1/vioscsi.inf \
	drivers/amd64/Win8.1/vioscsi.sys \
	drivers/amd64/Win8.1/viostor.cat \
	drivers/amd64/Win8.1/viostor.inf \
	drivers/amd64/Win8.1/viostor.sys \
	drivers/amd64/Win8/netkvm.cat \
	drivers/amd64/Win8/netkvm.inf \
	drivers/amd64/Win8/netkvm.sys \
	drivers/amd64/Win8/vioscsi.cat \
	drivers/amd64/Win8/vioscsi.inf \
	drivers/amd64/Win8/vioscsi.sys \
	drivers/amd64/Win8/viostor.cat \
	drivers/amd64/Win8/viostor.inf \
	drivers/amd64/Win8/viostor.sys \
	drivers/i386/Win2003/netkvm.cat \
	drivers/i386/Win2003/netkvm.inf \
	drivers/i386/Win2003/netkvm.sys \
	drivers/i386/Win2003/viostor.cat \
	drivers/i386/Win2003/viostor.inf \
	drivers/i386/Win2003/viostor.sys \
	drivers/i386/Win2008/netkvm.cat \
	drivers/i386/Win2008/netkvm.inf \
	drivers/i386/Win2008/netkvm.sys \
	drivers/i386/Win2008/vioscsi.cat \
	drivers/i386/Win2008/vioscsi.inf \
	drivers/i386/Win2008/vioscsi.sys \
	drivers/i386/Win2008/viostor.cat \
	drivers/i386/Win2008/viostor.inf \
	drivers/i386/Win2008/viostor.sys \
	drivers/i386/Win7/netkvm.cat \
	drivers/i386/Win7/netkvm.inf \
	drivers/i386/Win7/netkvm.sys \
	drivers/i386/Win7/vioscsi.cat \
	drivers/i386/Win7/vioscsi.inf \
	drivers/i386/Win7/vioscsi.sys \
	drivers/i386/Win7/viostor.cat \
	drivers/i386/Win7/viostor.inf \
	drivers/i386/Win7/viostor.sys \
	drivers/i386/Win8.1/netkvm.cat \
	drivers/i386/Win8.1/netkvm.inf \
	drivers/i386/Win8.1/netkvm.sys \
	drivers/i386/Win8.1/vioscsi.cat \
	drivers/i386/Win8.1/vioscsi.inf \
	drivers/i386/Win8.1/vioscsi.sys \
	drivers/i386/Win8.1/viostor.cat \
	drivers/i386/Win8.1/viostor.inf \
	drivers/i386/Win8.1/viostor.sys \
	drivers/i386/Win8/netkvm.cat \
	drivers/i386/Win8/netkvm.inf \
	drivers/i386/Win8/netkvm.sys \
	drivers/i386/Win8/vioscsi.cat \
	drivers/i386/Win8/vioscsi.inf \
	drivers/i386/Win8/vioscsi.sys \
	drivers/i386/Win8/viostor.cat \
	drivers/i386/Win8/viostor.inf \
	drivers/i386/Win8/viostor.sys \
	drivers/i386/WinXP/netkvm.cat \
	drivers/i386/WinXP/netkvm.inf \
	drivers/i386/WinXP/netkvm.sys \
	drivers/i386/WinXP/viostor.cat \
	drivers/i386/WinXP/viostor.inf \
	drivers/i386/WinXP/viostor.sys

cd_files = \
	cd/Balloon/2k12/amd64/WdfCoInstaller01011.dll \
	cd/Balloon/2k12/amd64/balloon.cat \
	cd/Balloon/2k12/amd64/balloon.inf \
	cd/Balloon/2k12/amd64/balloon.pdb \
	cd/Balloon/2k12/amd64/balloon.sys \
	cd/Balloon/2k12/amd64/blnsvr.exe \
	cd/Balloon/2k12/amd64/blnsvr.pdb \
	cd/Balloon/2k12R2/amd64/WdfCoInstaller01011.dll \
	cd/Balloon/2k12R2/amd64/balloon.cat \
	cd/Balloon/2k12R2/amd64/balloon.inf \
	cd/Balloon/2k12R2/amd64/balloon.pdb \
	cd/Balloon/2k12R2/amd64/balloon.sys \
	cd/Balloon/2k12R2/amd64/blnsvr.exe \
	cd/Balloon/2k12R2/amd64/blnsvr.pdb \
	cd/Balloon/2k22/amd64/WdfCoInstaller01011.dll \
	cd/Balloon/2k22/amd64/balloon.cat \
	cd/Balloon/2k22/amd64/balloon.inf \
	cd/Balloon/2k22/amd64/balloon.pdb \
	cd/Balloon/2k22/amd64/balloon.sys \
	cd/Balloon/2k22/amd64/blnsvr.exe \
	cd/Balloon/2k22/amd64/blnsvr.pdb \
	cd/Balloon/2k3/amd64/WdfCoInstaller01009.dll \
	cd/Balloon/2k3/amd64/balloon.cat \
	cd/Balloon/2k3/amd64/balloon.inf \
	cd/Balloon/2k3/amd64/balloon.pdb \
	cd/Balloon/2k3/amd64/balloon.sys \
	cd/Balloon/2k3/amd64/blnsvr.exe \
	cd/Balloon/2k3/amd64/blnsvr.pdb \
	cd/Balloon/2k3/x86/WdfCoInstaller01009.dll \
	cd/Balloon/2k3/x86/balloon.cat \
	cd/Balloon/2k3/x86/balloon.inf \
	cd/Balloon/2k3/x86/balloon.pdb \
	cd/Balloon/2k3/x86/balloon.sys \
	cd/Balloon/2k3/x86/blnsvr.exe \
	cd/Balloon/2k3/x86/blnsvr.pdb \
	cd/Balloon/2k8/amd64/WdfCoInstaller01009.dll \
	cd/Balloon/2k8/amd64/balloon.cat \
	cd/Balloon/2k8/amd64/balloon.inf \
	cd/Balloon/2k8/amd64/balloon.pdb \
	cd/Balloon/2k8/amd64/balloon.sys \
	cd/Balloon/2k8/amd64/blnsvr.exe \
	cd/Balloon/2k8/amd64/blnsvr.pdb \
	cd/Balloon/2k8/x86/WdfCoInstaller01009.dll \
	cd/Balloon/2k8/x86/balloon.cat \
	cd/Balloon/2k8/x86/balloon.inf \
	cd/Balloon/2k8/x86/balloon.pdb \
	cd/Balloon/2k8/x86/balloon.sys \
	cd/Balloon/2k8/x86/blnsvr.exe \
	cd/Balloon/2k8/x86/blnsvr.pdb \
	cd/Balloon/2k8R2/amd64/WdfCoInstaller01009.dll \
	cd/Balloon/2k8R2/amd64/balloon.cat \
	cd/Balloon/2k8R2/amd64/balloon.inf \
	cd/Balloon/2k8R2/amd64/balloon.pdb \
	cd/Balloon/2k8R2/amd64/balloon.sys \
	cd/Balloon/2k8R2/amd64/blnsvr.exe \
	cd/Balloon/2k8R2/amd64/blnsvr.pdb \
	cd/Balloon/w7/amd64/WdfCoInstaller01009.dll \
	cd/Balloon/w7/amd64/balloon.cat \
	cd/Balloon/w7/amd64/balloon.inf \
	cd/Balloon/w7/amd64/balloon.pdb \
	cd/Balloon/w7/amd64/balloon.sys \
	cd/Balloon/w7/amd64/blnsvr.exe \
	cd/Balloon/w7/amd64/blnsvr.pdb \
	cd/Balloon/w7/x86/WdfCoInstaller01009.dll \
	cd/Balloon/w7/x86/balloon.cat \
	cd/Balloon/w7/x86/balloon.inf \
	cd/Balloon/w7/x86/balloon.pdb \
	cd/Balloon/w7/x86/balloon.sys \
	cd/Balloon/w7/x86/blnsvr.exe \
	cd/Balloon/w7/x86/blnsvr.pdb \
	cd/Balloon/w8.1/amd64/WdfCoInstaller01011.dll \
	cd/Balloon/w8.1/amd64/balloon.cat \
	cd/Balloon/w8.1/amd64/balloon.inf \
	cd/Balloon/w8.1/amd64/balloon.pdb \
	cd/Balloon/w8.1/amd64/balloon.sys \
	cd/Balloon/w8.1/amd64/blnsvr.exe \
	cd/Balloon/w8.1/amd64/blnsvr.pdb \
	cd/Balloon/w8.1/x86/WdfCoInstaller01011.dll \
	cd/Balloon/w8.1/x86/balloon.cat \
	cd/Balloon/w8.1/x86/balloon.inf \
	cd/Balloon/w8.1/x86/balloon.pdb \
	cd/Balloon/w8.1/x86/balloon.sys \
	cd/Balloon/w8.1/x86/blnsvr.exe \
	cd/Balloon/w8.1/x86/blnsvr.pdb \
	cd/Balloon/w8/amd64/WdfCoInstaller01011.dll \
	cd/Balloon/w8/amd64/balloon.cat \
	cd/Balloon/w8/amd64/balloon.inf \
	cd/Balloon/w8/amd64/balloon.pdb \
	cd/Balloon/w8/amd64/balloon.sys \
	cd/Balloon/w8/amd64/blnsvr.exe \
	cd/Balloon/w8/amd64/blnsvr.pdb \
	cd/Balloon/w8/x86/WdfCoInstaller01011.dll \
	cd/Balloon/w8/x86/balloon.cat \
	cd/Balloon/w8/x86/balloon.inf \
	cd/Balloon/w8/x86/balloon.pdb \
	cd/Balloon/w8/x86/balloon.sys \
	cd/Balloon/w8/x86/blnsvr.exe \
	cd/Balloon/w8/x86/blnsvr.pdb \
	cd/Balloon/xp/x86/WdfCoInstaller01009.dll \
	cd/Balloon/xp/x86/balloon.cat \
	cd/Balloon/xp/x86/balloon.inf \
	cd/Balloon/xp/x86/balloon.pdb \
	cd/Balloon/xp/x86/balloon.sys \
	cd/Balloon/xp/x86/blnsvr.exe \
	cd/Balloon/xp/x86/blnsvr.pdb \
	cd/NetKVM/2k12/amd64/netkvm.cat \
	cd/NetKVM/2k12/amd64/netkvm.inf \
	cd/NetKVM/2k12/amd64/netkvm.pdb \
	cd/NetKVM/2k12/amd64/netkvm.sys \
	cd/NetKVM/2k12/amd64/netkvmco.dll \
	cd/NetKVM/2k12/amd64/readme.doc \
	cd/NetKVM/2k12R2/amd64/netkvm.cat \
	cd/NetKVM/2k12R2/amd64/netkvm.inf \
	cd/NetKVM/2k12R2/amd64/netkvm.pdb \
	cd/NetKVM/2k12R2/amd64/netkvm.sys \
	cd/NetKVM/2k12R2/amd64/netkvmco.dll \
	cd/NetKVM/2k12R2/amd64/readme.doc \
	cd/NetKVM/2k22/amd64/netkvm.cat \
	cd/NetKVM/2k22/amd64/netkvm.inf \
	cd/NetKVM/2k22/amd64/netkvm.pdb \
	cd/NetKVM/2k22/amd64/netkvm.sys \
	cd/NetKVM/2k22/amd64/netkvmco.dll \
	cd/NetKVM/2k22/amd64/readme.doc \
	cd/NetKVM/2k3/amd64/netkvm.cat \
	cd/NetKVM/2k3/amd64/netkvm.inf \
	cd/NetKVM/2k3/amd64/netkvm.pdb \
	cd/NetKVM/2k3/amd64/netkvm.sys \
	cd/NetKVM/2k3/x86/netkvm.cat \
	cd/NetKVM/2k3/x86/netkvm.inf \
	cd/NetKVM/2k3/x86/netkvm.pdb \
	cd/NetKVM/2k3/x86/netkvm.sys \
	cd/NetKVM/2k8/amd64/netkvm.cat \
	cd/NetKVM/2k8/amd64/netkvm.inf \
	cd/NetKVM/2k8/amd64/netkvm.pdb \
	cd/NetKVM/2k8/amd64/netkvm.sys \
	cd/NetKVM/2k8/amd64/netkvmco.dll \
	cd/NetKVM/2k8/amd64/readme.doc \
	cd/NetKVM/2k8/x86/netkvm.cat \
	cd/NetKVM/2k8/x86/netkvm.inf \
	cd/NetKVM/2k8/x86/netkvm.pdb \
	cd/NetKVM/2k8/x86/netkvm.sys \
	cd/NetKVM/2k8/x86/netkvmco.dll \
	cd/NetKVM/2k8/x86/readme.doc \
	cd/NetKVM/2k8R2/amd64/netkvm.cat \
	cd/NetKVM/2k8R2/amd64/netkvm.inf \
	cd/NetKVM/2k8R2/amd64/netkvm.pdb \
	cd/NetKVM/2k8R2/amd64/netkvm.sys \
	cd/NetKVM/2k8R2/amd64/netkvmco.dll \
	cd/NetKVM/2k8R2/amd64/readme.doc \
	cd/NetKVM/w7/amd64/netkvm.cat \
	cd/NetKVM/w7/amd64/netkvm.inf \
	cd/NetKVM/w7/amd64/netkvm.pdb \
	cd/NetKVM/w7/amd64/netkvm.sys \
	cd/NetKVM/w7/amd64/netkvmco.dll \
	cd/NetKVM/w7/amd64/readme.doc \
	cd/NetKVM/w7/x86/netkvm.cat \
	cd/NetKVM/w7/x86/netkvm.inf \
	cd/NetKVM/w7/x86/netkvm.pdb \
	cd/NetKVM/w7/x86/netkvm.sys \
	cd/NetKVM/w7/x86/netkvmco.dll \
	cd/NetKVM/w7/x86/readme.doc \
	cd/NetKVM/w8.1/amd64/netkvm.cat \
	cd/NetKVM/w8.1/amd64/netkvm.inf \
	cd/NetKVM/w8.1/amd64/netkvm.pdb \
	cd/NetKVM/w8.1/amd64/netkvm.sys \
	cd/NetKVM/w8.1/amd64/netkvmco.dll \
	cd/NetKVM/w8.1/amd64/readme.doc \
	cd/NetKVM/w8.1/x86/netkvm.cat \
	cd/NetKVM/w8.1/x86/netkvm.inf \
	cd/NetKVM/w8.1/x86/netkvm.pdb \
	cd/NetKVM/w8.1/x86/netkvm.sys \
	cd/NetKVM/w8.1/x86/netkvmco.dll \
	cd/NetKVM/w8.1/x86/readme.doc \
	cd/NetKVM/w8/amd64/netkvm.cat \
	cd/NetKVM/w8/amd64/netkvm.inf \
	cd/NetKVM/w8/amd64/netkvm.pdb \
	cd/NetKVM/w8/amd64/netkvm.sys \
	cd/NetKVM/w8/amd64/netkvmco.dll \
	cd/NetKVM/w8/amd64/readme.doc \
	cd/NetKVM/w8/x86/netkvm.cat \
	cd/NetKVM/w8/x86/netkvm.inf \
	cd/NetKVM/w8/x86/netkvm.pdb \
	cd/NetKVM/w8/x86/netkvm.sys \
	cd/NetKVM/w8/x86/netkvmco.dll \
	cd/NetKVM/w8/x86/readme.doc \
	cd/NetKVM/xp/x86/netkvm.cat \
	cd/NetKVM/xp/x86/netkvm.inf \
	cd/NetKVM/xp/x86/netkvm.pdb \
	cd/NetKVM/xp/x86/netkvm.sys \
	cd/guest-agent/qemu-ga-x64.msi \
	cd/guest-agent/qemu-ga-x86.msi \
	cd/qemupciserial/qemupciserial.inf \
	cd/viorng/2k12/amd64/WdfCoInstaller01011.dll \
	cd/viorng/2k12/amd64/viorng.cat \
	cd/viorng/2k12/amd64/viorng.inf \
	cd/viorng/2k12/amd64/viorng.pdb \
	cd/viorng/2k12/amd64/viorng.sys \
	cd/viorng/2k12/amd64/viorngci.dll \
	cd/viorng/2k12/amd64/viorngum.dll \
	cd/viorng/2k12R2/amd64/WdfCoInstaller01011.dll \
	cd/viorng/2k12R2/amd64/viorng.cat \
	cd/viorng/2k12R2/amd64/viorng.inf \
	cd/viorng/2k12R2/amd64/viorng.pdb \
	cd/viorng/2k12R2/amd64/viorng.sys \
	cd/viorng/2k12R2/amd64/viorngci.dll \
	cd/viorng/2k12R2/amd64/viorngum.dll \
	cd/viorng/2k22/amd64/WdfCoInstaller01011.dll \
	cd/viorng/2k22/amd64/viorng.cat \
	cd/viorng/2k22/amd64/viorng.inf \
	cd/viorng/2k22/amd64/viorng.pdb \
	cd/viorng/2k22/amd64/viorng.sys \
	cd/viorng/2k22/amd64/viorngci.dll \
	cd/viorng/2k22/amd64/viorngum.dll \
	cd/viorng/2k8/amd64/WdfCoInstaller01009.dll \
	cd/viorng/2k8/amd64/viorng.cat \
	cd/viorng/2k8/amd64/viorng.inf \
	cd/viorng/2k8/amd64/viorng.pdb \
	cd/viorng/2k8/amd64/viorng.sys \
	cd/viorng/2k8/amd64/viorngci.dll \
	cd/viorng/2k8/amd64/viorngum.dll \
	cd/viorng/2k8/x86/WdfCoInstaller01009.dll \
	cd/viorng/2k8/x86/viorng.cat \
	cd/viorng/2k8/x86/viorng.inf \
	cd/viorng/2k8/x86/viorng.pdb \
	cd/viorng/2k8/x86/viorng.sys \
	cd/viorng/2k8/x86/viorngci.dll \
	cd/viorng/2k8/x86/viorngum.dll \
	cd/viorng/2k8R2/amd64/WdfCoInstaller01009.dll \
	cd/viorng/2k8R2/amd64/viorng.cat \
	cd/viorng/2k8R2/amd64/viorng.inf \
	cd/viorng/2k8R2/amd64/viorng.pdb \
	cd/viorng/2k8R2/amd64/viorng.sys \
	cd/viorng/2k8R2/amd64/viorngci.dll \
	cd/viorng/2k8R2/amd64/viorngum.dll \
	cd/viorng/w7/amd64/WdfCoInstaller01009.dll \
	cd/viorng/w7/amd64/viorng.cat \
	cd/viorng/w7/amd64/viorng.inf \
	cd/viorng/w7/amd64/viorng.pdb \
	cd/viorng/w7/amd64/viorng.sys \
	cd/viorng/w7/amd64/viorngci.dll \
	cd/viorng/w7/amd64/viorngum.dll \
	cd/viorng/w7/x86/WdfCoInstaller01009.dll \
	cd/viorng/w7/x86/viorng.cat \
	cd/viorng/w7/x86/viorng.inf \
	cd/viorng/w7/x86/viorng.pdb \
	cd/viorng/w7/x86/viorng.sys \
	cd/viorng/w7/x86/viorngci.dll \
	cd/viorng/w7/x86/viorngum.dll \
	cd/viorng/w8.1/amd64/WdfCoInstaller01011.dll \
	cd/viorng/w8.1/amd64/viorng.cat \
	cd/viorng/w8.1/amd64/viorng.inf \
	cd/viorng/w8.1/amd64/viorng.pdb \
	cd/viorng/w8.1/amd64/viorng.sys \
	cd/viorng/w8.1/amd64/viorngci.dll \
	cd/viorng/w8.1/amd64/viorngum.dll \
	cd/viorng/w8.1/x86/WdfCoInstaller01011.dll \
	cd/viorng/w8.1/x86/viorng.cat \
	cd/viorng/w8.1/x86/viorng.inf \
	cd/viorng/w8.1/x86/viorng.pdb \
	cd/viorng/w8.1/x86/viorng.sys \
	cd/viorng/w8.1/x86/viorngci.dll \
	cd/viorng/w8.1/x86/viorngum.dll \
	cd/viorng/w8/amd64/WdfCoInstaller01011.dll \
	cd/viorng/w8/amd64/viorng.cat \
	cd/viorng/w8/amd64/viorng.inf \
	cd/viorng/w8/amd64/viorng.pdb \
	cd/viorng/w8/amd64/viorng.sys \
	cd/viorng/w8/amd64/viorngci.dll \
	cd/viorng/w8/amd64/viorngum.dll \
	cd/viorng/w8/x86/WdfCoInstaller01011.dll \
	cd/viorng/w8/x86/viorng.cat \
	cd/viorng/w8/x86/viorng.inf \
	cd/viorng/w8/x86/viorng.pdb \
	cd/viorng/w8/x86/viorng.sys \
	cd/viorng/w8/x86/viorngci.dll \
	cd/viorng/w8/x86/viorngum.dll \
	cd/vioscsi/2k12/amd64/vioscsi.cat \
	cd/vioscsi/2k12/amd64/vioscsi.inf \
	cd/vioscsi/2k12/amd64/vioscsi.pdb \
	cd/vioscsi/2k12/amd64/vioscsi.sys \
	cd/vioscsi/2k12R2/amd64/vioscsi.cat \
	cd/vioscsi/2k12R2/amd64/vioscsi.inf \
	cd/vioscsi/2k12R2/amd64/vioscsi.pdb \
	cd/vioscsi/2k12R2/amd64/vioscsi.sys \
	cd/vioscsi/2k22/amd64/vioscsi.cat \
	cd/vioscsi/2k22/amd64/vioscsi.inf \
	cd/vioscsi/2k22/amd64/vioscsi.pdb \
	cd/vioscsi/2k22/amd64/vioscsi.sys \
	cd/vioscsi/2k8/amd64/vioscsi.cat \
	cd/vioscsi/2k8/amd64/vioscsi.inf \
	cd/vioscsi/2k8/amd64/vioscsi.pdb \
	cd/vioscsi/2k8/amd64/vioscsi.sys \
	cd/vioscsi/2k8/x86/vioscsi.cat \
	cd/vioscsi/2k8/x86/vioscsi.inf \
	cd/vioscsi/2k8/x86/vioscsi.pdb \
	cd/vioscsi/2k8/x86/vioscsi.sys \
	cd/vioscsi/2k8R2/amd64/vioscsi.cat \
	cd/vioscsi/2k8R2/amd64/vioscsi.inf \
	cd/vioscsi/2k8R2/amd64/vioscsi.pdb \
	cd/vioscsi/2k8R2/amd64/vioscsi.sys \
	cd/vioscsi/w7/amd64/vioscsi.cat \
	cd/vioscsi/w7/amd64/vioscsi.inf \
	cd/vioscsi/w7/amd64/vioscsi.pdb \
	cd/vioscsi/w7/amd64/vioscsi.sys \
	cd/vioscsi/w7/x86/vioscsi.cat \
	cd/vioscsi/w7/x86/vioscsi.inf \
	cd/vioscsi/w7/x86/vioscsi.pdb \
	cd/vioscsi/w7/x86/vioscsi.sys \
	cd/vioscsi/w8.1/amd64/vioscsi.cat \
	cd/vioscsi/w8.1/amd64/vioscsi.inf \
	cd/vioscsi/w8.1/amd64/vioscsi.pdb \
	cd/vioscsi/w8.1/amd64/vioscsi.sys \
	cd/vioscsi/w8.1/x86/vioscsi.cat \
	cd/vioscsi/w8.1/x86/vioscsi.inf \
	cd/vioscsi/w8.1/x86/vioscsi.pdb \
	cd/vioscsi/w8.1/x86/vioscsi.sys \
	cd/vioscsi/w8/amd64/vioscsi.cat \
	cd/vioscsi/w8/amd64/vioscsi.inf \
	cd/vioscsi/w8/amd64/vioscsi.pdb \
	cd/vioscsi/w8/amd64/vioscsi.sys \
	cd/vioscsi/w8/x86/vioscsi.cat \
	cd/vioscsi/w8/x86/vioscsi.inf \
	cd/vioscsi/w8/x86/vioscsi.pdb \
	cd/vioscsi/w8/x86/vioscsi.sys \
	cd/vioserial/2k12/amd64/WdfCoInstaller01011.dll \
	cd/vioserial/2k12/amd64/vioser.cat \
	cd/vioserial/2k12/amd64/vioser.inf \
	cd/vioserial/2k12/amd64/vioser.pdb \
	cd/vioserial/2k12/amd64/vioser.sys \
	cd/vioserial/2k12R2/amd64/WdfCoInstaller01011.dll \
	cd/vioserial/2k12R2/amd64/vioser.cat \
	cd/vioserial/2k12R2/amd64/vioser.inf \
	cd/vioserial/2k12R2/amd64/vioser.pdb \
	cd/vioserial/2k12R2/amd64/vioser.sys \
	cd/vioserial/2k22/amd64/WdfCoInstaller01011.dll \
	cd/vioserial/2k22/amd64/vioser.cat \
	cd/vioserial/2k22/amd64/vioser.inf \
	cd/vioserial/2k22/amd64/vioser.pdb \
	cd/vioserial/2k22/amd64/vioser.sys \
	cd/vioserial/2k3/amd64/WdfCoInstaller01009.dll \
	cd/vioserial/2k3/amd64/vioser.cat \
	cd/vioserial/2k3/amd64/vioser.inf \
	cd/vioserial/2k3/amd64/vioser.pdb \
	cd/vioserial/2k3/amd64/vioser.sys \
	cd/vioserial/2k3/x86/WdfCoInstaller01009.dll \
	cd/vioserial/2k3/x86/vioser.cat \
	cd/vioserial/2k3/x86/vioser.inf \
	cd/vioserial/2k3/x86/vioser.pdb \
	cd/vioserial/2k3/x86/vioser.sys \
	cd/vioserial/2k8/amd64/WdfCoInstaller01009.dll \
	cd/vioserial/2k8/amd64/vioser.cat \
	cd/vioserial/2k8/amd64/vioser.inf \
	cd/vioserial/2k8/amd64/vioser.pdb \
	cd/vioserial/2k8/amd64/vioser.sys \
	cd/vioserial/2k8/x86/WdfCoInstaller01009.dll \
	cd/vioserial/2k8/x86/vioser.cat \
	cd/vioserial/2k8/x86/vioser.inf \
	cd/vioserial/2k8/x86/vioser.pdb \
	cd/vioserial/2k8/x86/vioser.sys \
	cd/vioserial/2k8R2/amd64/WdfCoInstaller01009.dll \
	cd/vioserial/2k8R2/amd64/vioser.cat \
	cd/vioserial/2k8R2/amd64/vioser.inf \
	cd/vioserial/2k8R2/amd64/vioser.pdb \
	cd/vioserial/2k8R2/amd64/vioser.sys \
	cd/vioserial/w7/amd64/WdfCoInstaller01009.dll \
	cd/vioserial/w7/amd64/vioser.cat \
	cd/vioserial/w7/amd64/vioser.inf \
	cd/vioserial/w7/amd64/vioser.pdb \
	cd/vioserial/w7/amd64/vioser.sys \
	cd/vioserial/w7/x86/WdfCoInstaller01009.dll \
	cd/vioserial/w7/x86/vioser.cat \
	cd/vioserial/w7/x86/vioser.inf \
	cd/vioserial/w7/x86/vioser.pdb \
	cd/vioserial/w7/x86/vioser.sys \
	cd/vioserial/w8.1/amd64/WdfCoInstaller01011.dll \
	cd/vioserial/w8.1/amd64/vioser.cat \
	cd/vioserial/w8.1/amd64/vioser.inf \
	cd/vioserial/w8.1/amd64/vioser.pdb \
	cd/vioserial/w8.1/amd64/vioser.sys \
	cd/vioserial/w8.1/x86/WdfCoInstaller01011.dll \
	cd/vioserial/w8.1/x86/vioser.cat \
	cd/vioserial/w8.1/x86/vioser.inf \
	cd/vioserial/w8.1/x86/vioser.pdb \
	cd/vioserial/w8.1/x86/vioser.sys \
	cd/vioserial/w8/amd64/WdfCoInstaller01011.dll \
	cd/vioserial/w8/amd64/vioser.cat \
	cd/vioserial/w8/amd64/vioser.inf \
	cd/vioserial/w8/amd64/vioser.pdb \
	cd/vioserial/w8/amd64/vioser.sys \
	cd/vioserial/w8/x86/WdfCoInstaller01011.dll \
	cd/vioserial/w8/x86/vioser.cat \
	cd/vioserial/w8/x86/vioser.inf \
	cd/vioserial/w8/x86/vioser.pdb \
	cd/vioserial/w8/x86/vioser.sys \
	cd/vioserial/xp/x86/WdfCoInstaller01009.dll \
	cd/vioserial/xp/x86/vioser.cat \
	cd/vioserial/xp/x86/vioser.inf \
	cd/vioserial/xp/x86/vioser.pdb \
	cd/vioserial/xp/x86/vioser.sys \
	cd/viostor/2k12/amd64/viostor.cat \
	cd/viostor/2k12/amd64/viostor.inf \
	cd/viostor/2k12/amd64/viostor.pdb \
	cd/viostor/2k12/amd64/viostor.sys \
	cd/viostor/2k12R2/amd64/viostor.cat \
	cd/viostor/2k12R2/amd64/viostor.inf \
	cd/viostor/2k12R2/amd64/viostor.pdb \
	cd/viostor/2k12R2/amd64/viostor.sys \
	cd/viostor/2k22/amd64/viostor.cat \
	cd/viostor/2k22/amd64/viostor.inf \
	cd/viostor/2k22/amd64/viostor.pdb \
	cd/viostor/2k22/amd64/viostor.sys \
	cd/viostor/2k3/amd64/viostor.cat \
	cd/viostor/2k3/amd64/viostor.inf \
	cd/viostor/2k3/amd64/viostor.pdb \
	cd/viostor/2k3/amd64/viostor.sys \
	cd/viostor/2k3/x86/viostor.cat \
	cd/viostor/2k3/x86/viostor.inf \
	cd/viostor/2k3/x86/viostor.pdb \
	cd/viostor/2k3/x86/viostor.sys \
	cd/viostor/2k8/amd64/viostor.cat \
	cd/viostor/2k8/amd64/viostor.inf \
	cd/viostor/2k8/amd64/viostor.pdb \
	cd/viostor/2k8/amd64/viostor.sys \
	cd/viostor/2k8/x86/viostor.cat \
	cd/viostor/2k8/x86/viostor.inf \
	cd/viostor/2k8/x86/viostor.pdb \
	cd/viostor/2k8/x86/viostor.sys \
	cd/viostor/2k8R2/amd64/viostor.cat \
	cd/viostor/2k8R2/amd64/viostor.inf \
	cd/viostor/2k8R2/amd64/viostor.pdb \
	cd/viostor/2k8R2/amd64/viostor.sys \
	cd/viostor/w7/amd64/viostor.cat \
	cd/viostor/w7/amd64/viostor.inf \
	cd/viostor/w7/amd64/viostor.pdb \
	cd/viostor/w7/amd64/viostor.sys \
	cd/viostor/w7/x86/viostor.cat \
	cd/viostor/w7/x86/viostor.inf \
	cd/viostor/w7/x86/viostor.pdb \
	cd/viostor/w7/x86/viostor.sys \
	cd/viostor/w8.1/amd64/viostor.cat \
	cd/viostor/w8.1/amd64/viostor.inf \
	cd/viostor/w8.1/amd64/viostor.pdb \
	cd/viostor/w8.1/amd64/viostor.sys \
	cd/viostor/w8.1/x86/viostor.cat \
	cd/viostor/w8.1/x86/viostor.inf \
	cd/viostor/w8.1/x86/viostor.pdb \
	cd/viostor/w8.1/x86/viostor.sys \
	cd/viostor/w8/amd64/viostor.cat \
	cd/viostor/w8/amd64/viostor.inf \
	cd/viostor/w8/amd64/viostor.pdb \
	cd/viostor/w8/amd64/viostor.sys \
	cd/viostor/w8/x86/viostor.cat \
	cd/viostor/w8/x86/viostor.inf \
	cd/viostor/w8/x86/viostor.pdb \
	cd/viostor/w8/x86/viostor.sys \
	cd/viostor/xp/x86/viostor.cat \
	cd/viostor/xp/x86/viostor.inf \
	cd/viostor/xp/x86/viostor.pdb \
	cd/viostor/xp/x86/viostor.sys \
	cd/virtio-win-1.7.4_amd64.vfd \
	cd/virtio-win-1.7.4_x86.vfd \
	cd/virtio-win_license.txt

EXTRA_DIST = $(drivers) $(cd_files)

check_DATA = fake-virtio-win.iso

CLEANFILES += fake-virtio-win.iso

fake-virtio-win.iso: $(cd_files)
	$(MKISOFS) -J -r -o $@ $(srcdir)/cd
