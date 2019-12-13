# libguestfs
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

dnl Any C libraries required by virt-v2v.

dnl Of course we need libguestfs.
PKG_CHECK_MODULES([LIBGUESTFS], [libguestfs >= 1.40])

dnl Define a C symbol for the host CPU architecture.
AC_DEFINE_UNQUOTED([host_cpu],["$host_cpu"],[Host architecture.])

dnl Headers.
AC_CHECK_HEADERS([\
    byteswap.h \
    errno.h \
    linux/magic.h \
    sys/mount.h \
    sys/socket.h \
    sys/statfs.h \
    sys/statvfs.h \
    sys/time.h \
    sys/types.h \
    sys/un.h \
    sys/vfs.h \
    sys/wait.h \
    windows.h])

dnl Functions.
AC_CHECK_FUNCS([\
    fsync \
    posix_fadvise \
    statfs \
    statvfs \
    sync])

dnl Which header file defines major, minor, makedev.
AC_HEADER_MAJOR

dnl GNU gettext tools (optional).
AC_CHECK_PROG([XGETTEXT],[xgettext],[xgettext],[no])
AC_CHECK_PROG([MSGCAT],[msgcat],[msgcat],[no])
AC_CHECK_PROG([MSGFMT],[msgfmt],[msgfmt],[no])
AC_CHECK_PROG([MSGMERGE],[msgmerge],[msgmerge],[no])

dnl Check they are the GNU gettext tools.
AC_MSG_CHECKING([msgfmt is GNU tool])
if $MSGFMT --version >/dev/null 2>&1 && $MSGFMT --version | grep -q 'GNU gettext'; then
    msgfmt_is_gnu=yes
else
    msgfmt_is_gnu=no
fi
AC_MSG_RESULT([$msgfmt_is_gnu])
AM_CONDITIONAL([HAVE_GNU_GETTEXT],
    [test "x$XGETTEXT" != "xno" && test "x$MSGCAT" != "xno" && test "x$MSGFMT" != "xno" && test "x$MSGMERGE" != "xno" && test "x$msgfmt_is_gnu" != "xno"])

dnl Check for gettext.
AM_GNU_GETTEXT([external])

dnl Check for PCRE (required)
PKG_CHECK_MODULES([PCRE], [libpcre], [], [
    AC_CHECK_PROGS([PCRE_CONFIG], [pcre-config pcre2-config], [no])
    AS_IF([test "x$PCRE_CONFIG" = "xno"], [
        AC_MSG_ERROR([Please install the pcre devel package])
    ])
    PCRE_CFLAGS=`$PCRE_CONFIG --cflags`
    PCRE_LIBS=`$PCRE_CONFIG --libs`
])

dnl libvirt (required)
PKG_CHECK_MODULES([LIBVIRT], [libvirt >= 0.10.2])

libvirt_ro_uri='qemu+unix:///system?socket=/var/run/libvirt/libvirt-sock-ro'
AC_SUBST([libvirt_ro_uri])

dnl libxml2 (required)
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0])
old_LIBS="$LIBS"
LIBS="$LIBS $LIBXML2_LIBS"
AC_CHECK_FUNCS([xmlBufferDetach])
LIBS="$old_LIBS"

dnl Check for Jansson JSON library (required).
PKG_CHECK_MODULES([JANSSON], [jansson >= 2.7])
