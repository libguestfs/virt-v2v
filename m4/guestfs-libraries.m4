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
    endian.h \
    sys/endian.h \
    errno.h \
    linux/fs.h \
    linux/magic.h \
    linux/raid/md_u.h \
    linux/rtc.h \
    printf.h \
    sys/inotify.h \
    sys/mount.h \
    sys/resource.h \
    sys/socket.h \
    sys/statfs.h \
    sys/statvfs.h \
    sys/time.h \
    sys/types.h \
    sys/un.h \
    sys/vfs.h \
    sys/wait.h \
    windows.h \
    sys/xattr.h])

dnl Functions.
AC_CHECK_FUNCS([\
    be32toh \
    fsync \
    futimens \
    getxattr \
    htonl \
    htons \
    inotify_init1 \
    lgetxattr \
    listxattr \
    llistxattr \
    lsetxattr \
    lremovexattr \
    mknod \
    ntohl \
    ntohs \
    posix_fallocate \
    posix_fadvise \
    removexattr \
    setitimer \
    setrlimit \
    setxattr \
    sigaction \
    statfs \
    statvfs \
    sync])

dnl Which header file defines major, minor, makedev.
AC_HEADER_MAJOR

dnl Check for UNIX_PATH_MAX, creating a custom one if not available.
AC_MSG_CHECKING([for UNIX_PATH_MAX])
AC_COMPILE_IFELSE([
  AC_LANG_PROGRAM([[
#include <sys/un.h>
  ]], [[
#ifndef UNIX_PATH_MAX
#error UNIX_PATH_MAX not defined
#endif
  ]])], [
    AC_MSG_RESULT([yes])
  ], [
    AC_MSG_RESULT([no])
    AC_MSG_CHECKING([for size of sockaddr_un.sun_path])
    AC_COMPUTE_INT(unix_path_max, [sizeof (myaddr.sun_path)], [
#include <sys/un.h>
struct sockaddr_un myaddr;
      ], [
        AC_MSG_ERROR([cannot get it])
      ])
    AC_MSG_RESULT([$unix_path_max])
    AC_DEFINE_UNQUOTED([UNIX_PATH_MAX], $unix_path_max, [Custom value for UNIX_PATH_MAX])
  ])

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

dnl Check for libselinux (optional).
AC_CHECK_HEADERS([selinux/selinux.h])
AC_CHECK_LIB([selinux],[setexeccon],[
    have_libselinux="$ac_cv_header_selinux_selinux_h"
    SELINUX_LIBS="-lselinux"

    old_LIBS="$LIBS"
    LIBS="$LIBS $SELINUX_LIBS"
    AC_CHECK_FUNCS([setcon getcon])
    LIBS="$old_LIBS"
],[have_libselinux=no])
if test "x$have_libselinux" = "xyes"; then
    AC_DEFINE([HAVE_LIBSELINUX],[1],[Define to 1 if you have libselinux.])
fi
AC_SUBST([SELINUX_LIBS])

dnl Check for PCRE (required)
PKG_CHECK_MODULES([PCRE], [libpcre], [], [
    AC_CHECK_PROGS([PCRE_CONFIG], [pcre-config pcre2-config], [no])
    AS_IF([test "x$PCRE_CONFIG" = "xno"], [
        AC_MSG_ERROR([Please install the pcre devel package])
    ])
    PCRE_CFLAGS=`$PCRE_CONFIG --cflags`
    PCRE_LIBS=`$PCRE_CONFIG --libs`
])

dnl Check for Augeas >= 1.2.0 (required).
PKG_CHECK_MODULES([AUGEAS],[augeas >= 1.2.0])

dnl Check for aug_source function, added in Augeas 1.8.0.
old_LIBS="$LIBS"
LIBS="$AUGEAS_LIBS"
AC_CHECK_FUNCS([aug_source])
LIBS="$old_LIBS"

dnl libmagic (required)
AC_CHECK_LIB([magic],[magic_file],[
    AC_CHECK_HEADER([magic.h],[
        AC_SUBST([MAGIC_LIBS], ["-lmagic"])
    ], [])
],[])
AS_IF([test -z "$MAGIC_LIBS"],
    [AC_MSG_ERROR([libmagic (part of the "file" command) is required.
                   Please install the file devel package])])

dnl libvirt (highly recommended)
AC_ARG_WITH([libvirt],[
    AS_HELP_STRING([--without-libvirt],
                   [disable libvirt support @<:@default=check@:>@])],
    [],
    [with_libvirt=check])
AS_IF([test "$with_libvirt" != "no"],[
    PKG_CHECK_MODULES([LIBVIRT], [libvirt >= 0.10.2],[
        AC_SUBST([LIBVIRT_CFLAGS])
        AC_SUBST([LIBVIRT_LIBS])
        AC_DEFINE([HAVE_LIBVIRT],[1],[libvirt found at compile time.])
    ],[
        if test "$DEFAULT_BACKEND" = "libvirt"; then
            AC_MSG_ERROR([Please install the libvirt devel package])
        else
            AC_MSG_WARN([libvirt not found, some core features will be disabled])
        fi
    ])
])
AM_CONDITIONAL([HAVE_LIBVIRT],[test "x$LIBVIRT_LIBS" != "x"])

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
