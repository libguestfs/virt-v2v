# virt-v2v
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

dnl Any C libraries required by virt-v2v.

dnl Of course we need libguestfs.
dnl
dnl We need libguestfs 1.59.2 for guestfs_xfs_info2.
PKG_CHECK_MODULES([LIBGUESTFS], [libguestfs >= 1.59.2])
printf "libguestfs version is "; $PKG_CONFIG --modversion libguestfs

dnl And libnbd.
PKG_CHECK_MODULES([LIBNBD], [libnbd >= 1.14])
printf "libnbd version is "; $PKG_CONFIG --modversion libnbd

dnl Test if it's GNU or XSI strerror_r.
AC_FUNC_STRERROR_R

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

dnl Check for PCRE2 (required)
PKG_CHECK_MODULES([PCRE2], [libpcre2-8], [], [
    AC_CHECK_PROGS([PCRE2_CONFIG], [pcre2-config], [no])
    AS_IF([test "x$PCRE2_CONFIG" = "xno"], [
        AC_MSG_ERROR([Please install the pcre2 devel package])
    ])
    PCRE_CFLAGS=`$PCRE2_CONFIG --cflags`
    PCRE_LIBS=`$PCRE2_CONFIG --libs8`
])

dnl libvirt (required)
PKG_CHECK_MODULES([LIBVIRT], [libvirt >= 0.10.2])
printf "libvirt version is "; $PKG_CONFIG --modversion libvirt

libvirt_ro_uri='qemu+unix:///system?socket=/var/run/libvirt/libvirt-sock-ro'
AC_SUBST([libvirt_ro_uri])

dnl libxml2 (required)
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0])
printf "libxml2 version is "; $PKG_CONFIG --modversion libxml-2.0
old_LIBS="$LIBS"
LIBS="$LIBS $LIBXML2_LIBS"
AC_CHECK_FUNCS([xmlBufferDetach])
LIBS="$old_LIBS"

dnl Check for JSON-C library (required).
PKG_CHECK_MODULES([JSON_C], [json-c >= 0.14])
printf "json-c version is "; $PKG_CONFIG --modversion json-c

dnl Check for libosinfo (mandatory)
PKG_CHECK_MODULES([LIBOSINFO], [libosinfo-1.0])
printf "libosinfo version is "; $PKG_CONFIG --modversion libosinfo-1.0

dnl glibc 2.27 removes crypt(3) and suggests using libxcrypt.
PKG_CHECK_MODULES([LIBCRYPT], [libxcrypt], [
    AC_SUBST([LIBCRYPT_CFLAGS])
    AC_SUBST([LIBCRYPT_LIBS])
],[
    dnl Check if crypt() is provided by another library.
    old_LIBS="$LIBS"
    AC_SEARCH_LIBS([crypt],[crypt])
    LIBS="$old_LIBS"
    if test "$ac_cv_search_crypt" = "-lcrypt" ; then
        LIBCRYPT_LIBS="-lcrypt"
    fi
    AC_SUBST([LIBCRYPT_LIBS])
])

dnl Do we need to include <crypt.h>?
old_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $LIBCRYPT_CFLAGS"
AC_CHECK_HEADERS([crypt.h])
CFLAGS="$old_CFLAGS"
