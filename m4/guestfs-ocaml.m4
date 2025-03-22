# libguestfs
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

dnl Check for OCaml (required, for OCaml bindings and OCaml tools).

dnl OCAMLC and OCAMLFIND have to be unset first, otherwise
dnl AC_CHECK_TOOL (inside AC_PROG_OCAML) will not look.
OCAMLC=
OCAMLFIND=
AC_PROG_OCAML
AC_PROG_FINDLIB

AS_IF([test "x$OCAMLC" = "xno"],[
    AC_MSG_ERROR([OCaml compiler is required])
])

AS_IF([test "x$OCAMLFIND" = "xno"],[
    AC_MSG_ERROR([OCaml findlib is required])
])

dnl --disable-ocaml only disables OCaml bindings and OCaml virt tools.
AC_ARG_ENABLE([ocaml],
    AS_HELP_STRING([--disable-ocaml], [disable OCaml language bindings and tools]),
    [],
    [enable_ocaml=yes])

dnl OCaml >= 4.08 is required.
ocaml_ver_str=4.08
ocaml_min_major=4
ocaml_min_minor=8
AC_MSG_CHECKING([if OCaml version >= $ocaml_ver_str])
ocaml_major="`echo $OCAMLVERSION | $AWK -F. '{print $1}'`"
ocaml_minor="`echo $OCAMLVERSION | $AWK -F. '{print $2}' | sed 's/^0//'`"
AS_IF([test "$ocaml_major" -ge $((ocaml_min_major+1)) || ( test "$ocaml_major" -eq $ocaml_min_major && test "$ocaml_minor" -ge $ocaml_min_minor )],[
    AC_MSG_RESULT([yes ($ocaml_major, $ocaml_minor)])
],[
    AC_MSG_RESULT([no])
    AC_MSG_FAILURE([OCaml compiler is not new enough.  At least OCaml $ocaml_ver_str is required])
])

AM_CONDITIONAL([HAVE_OCAML],
               [test "x$enable_ocaml" != "xno"])
AM_CONDITIONAL([HAVE_OCAMLOPT],
               [test "x$OCAMLOPT" != "xno"])
AM_CONDITIONAL([HAVE_OCAMLDOC],
               [test "x$OCAMLDOC" != "xno"])

dnl Check if ocamlc/ocamlopt -runtime-variant _pic works.  It was
dnl added in OCaml >= 4.03, but in theory might be disabled by
dnl downstream distros.
OCAML_RUNTIME_VARIANT_PIC_OPTION=""
if test "x$OCAMLC" != "xno"; then
    AC_MSG_CHECKING([if OCaml ‘-runtime-variant _pic’ works])
    rm -f conftest.ml contest
    echo 'print_endline "hello world"' > conftest.ml
    if $OCAMLOPT conftest.ml -runtime-variant _pic -o conftest >&5 2>&5 ; then
        AC_MSG_RESULT([yes])
        OCAML_RUNTIME_VARIANT_PIC_OPTION="-runtime-variant _pic"
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.ml contest
fi
AC_SUBST([OCAML_RUNTIME_VARIANT_PIC_OPTION])

dnl ocaml guestfs module is required.
OCAML_PKG_guestfs=no
AC_CHECK_OCAML_PKG(guestfs)
if test "x$OCAML_PKG_guestfs" = "xno"; then
    AC_MSG_ERROR([the OCaml module 'guestfs' is required])
fi

dnl ocaml libvirt module is required.
OCAML_PKG_libvirt=no
AC_CHECK_OCAML_PKG(libvirt)
if test "x$OCAML_PKG_libvirt" = "xno"; then
    AC_MSG_ERROR([the OCaml module 'libvirt' is required])
fi

dnl ocaml nbd (libnbd) module is required.
OCAML_PKG_nbd=no
AC_CHECK_OCAML_PKG(nbd)
if test "x$OCAML_PKG_nbd" = "xno"; then
    AC_MSG_ERROR([the OCaml module 'nbd' (from libnbd) is required])
fi

OCAML_PKG_gettext=no
AS_IF([test "x$OCAMLC" != "xno"],[
    # Create common/mlgettext/common_gettext.ml gettext functions or stubs.

    # If we're building in a different directory, then common/mlgettext
    # might not exist yet, so create it:
    mkdir -p common/mlgettext

    GUESTFS_CREATE_COMMON_GETTEXT_ML([common/mlgettext/common_gettext.ml])
])
AM_CONDITIONAL([HAVE_OCAML_PKG_GETTEXT],
               [test "x$OCAML_PKG_gettext" != "xno"])

AC_CHECK_PROG([OCAML_GETTEXT],[ocaml-gettext],[ocaml-gettext],[no])
AM_CONDITIONAL([HAVE_OCAML_GETTEXT],
               [test "x$OCAML_PKG_gettext" != "xno" && test "x$OCAML_GETTEXT" != "xno"])

dnl Flags we want to pass to every OCaml compiler call.
OCAML_WARN_ERROR="-warn-error +C+D+E+F+L+M+P+S+U+V+Y+Z+X+52-3-6 -w -6"
AC_SUBST([OCAML_WARN_ERROR])
OCAML_FLAGS="-g -annot $safe_string_option"
AC_SUBST([OCAML_FLAGS])
