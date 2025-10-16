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

# The major, minor, and release fields MUST be numbers.  Packagers can
# add extra information using --with-extra="..." which may be any
# freeform string.
m4_define([v2v_major],   [2])
m4_define([v2v_minor],   [9])
m4_define([v2v_release], [9])

AC_INIT([virt-v2v],v2v_major.v2v_minor.v2v_release)

# Headings within the configure script output.
term_bold=""
term_red=""
term_green=""
term_restore=""
AS_IF([test -t 1], [
    AS_CASE(["$TERM"],
            [xterm*|vt220*], [
              term_bold="$(printf "\e@<:@1m")"
              term_red="$(printf "\e@<:@22;31m")"
              term_green="$(printf "\e@<:@22;32m")"
              term_restore="$(printf "\e@<:@0m")"
    ])
])
m4_define([HEADING],
          [AS_ECHO
           AS_ECHO(["${term_bold}$1${term_restore}"])])

HEADING([Checking for C compiler and basic build environment])
m4_include([m4/guestfs-c.m4])

AC_CONFIG_AUX_DIR([build-aux])

AC_USE_SYSTEM_EXTENSIONS

dnl Initialize automake.
AM_INIT_AUTOMAKE(foreign subdir-objects tar-pax) dnl NB: Do not [quote] this parameter.

m4_ifndef([AM_SILENT_RULES], [m4_define([AM_SILENT_RULES],[])])
AM_SILENT_RULES([yes]) # make --enable-silent-rules the default.

AC_CONFIG_MACRO_DIR([m4])

dnl Initialize libtool.
LT_INIT

HEADING([Checking for version and optional features])

dnl Stable or development version?
BRANCH_NUMBER=v2v_major.v2v_minor
AC_SUBST([BRANCH_NUMBER])
AC_MSG_CHECKING([if $BRANCH_NUMBER is a stable or development branch of virt-v2v])
AS_IF([test "$((v2v_minor % 2))" -eq 0 ],[
    BRANCH_TYPE=stable
    AC_MSG_RESULT([$BRANCH_TYPE])
],[
    BRANCH_TYPE=development
    AC_MSG_RESULT([$BRANCH_TYPE])
])
AC_SUBST([BRANCH_TYPE])

dnl Extra string, a freeform string defined by packagers.
AC_ARG_WITH([extra],
    [AS_HELP_STRING([--with-extra],
                    [extra version string (for use by packagers)])],
    [v2v_extra="$withval"],
    [v2v_extra=]
)

AC_MSG_NOTICE([virt-v2v version v2v_major.v2v_minor.v2v_release$v2v_extra])

dnl Split up the version string.
AC_DEFINE([PACKAGE_VERSION_MAJOR],[v2v_major],[Major version number.])
AC_DEFINE([PACKAGE_VERSION_MINOR],[v2v_minor],[Minor version number.])
AC_DEFINE([PACKAGE_VERSION_RELEASE],[v2v_release],[Release number.])
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_EXTRA],["$v2v_extra"],[Extra version string.])
PACKAGE_VERSION_FULL="v2v_major.v2v_minor.v2v_release${v2v_extra}"
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_FULL],["$PACKAGE_VERSION_FULL"],[Full version string.])
AC_SUBST([PACKAGE_VERSION_FULL])

AC_ARG_ENABLE([block-driver],
    [AS_HELP_STRING([--disable-block-driver],
                    [disable --block-driver feature])],
    [case $enableval in
    yes|no) ;;
    *) AC_MSG_ERROR([bad value $enableval for --enable-block-driver option]) ;;
    esac
    ENABLE_BLOCK_DRIVER=$enableval],
    [ENABLE_BLOCK_DRIVER=yes]
)
AC_SUBST([ENABLE_BLOCK_DRIVER])
AM_CONDITIONAL([ENABLE_BLOCK_DRIVER], [test "x$ENABLE_BLOCK_DRIVER" = "xyes"])

dnl Allow some input modes to be disabled.
AC_ARG_ENABLE([xen],
    [AS_HELP_STRING([--disable-xen],
                    [disable Xen input mode])],
    [case $enableval in
    yes|no) ;;
    *) AC_MSG_ERROR([bad value $enableval for --enable-xen option]) ;;
    esac
    ENABLE_XEN=$enableval],
    [ENABLE_XEN=yes]
)
AC_SUBST([ENABLE_XEN])
AM_CONDITIONAL([ENABLE_XEN], [test "x$ENABLE_XEN" = "xyes"])

dnl Allow some output modes to be disabled.
AC_ARG_ENABLE([glance],
    [AS_HELP_STRING([--disable-glance],
                    [disable -o glance output mode])],
    [case $enableval in
    yes|no) ;;
    *) AC_MSG_ERROR([bad value $enableval for --enable-glance option]) ;;
    esac
    ENABLE_GLANCE=$enableval],
    [ENABLE_GLANCE=yes]
)
AC_SUBST([ENABLE_GLANCE])
AM_CONDITIONAL([ENABLE_GLANCE], [test "x$ENABLE_GLANCE" = "xyes"])

AC_ARG_ENABLE([ovirt],
    [AS_HELP_STRING([--disable-ovirt],
                    [disable all oVirt output modes])],
    [case $enableval in
    yes|no) ;;
    *) AC_MSG_ERROR([bad value $enableval for --enable-ovirt option]) ;;
    esac
    ENABLE_OVIRT=$enableval],
    [ENABLE_OVIRT=yes]
)
AC_SUBST([ENABLE_OVIRT])
AM_CONDITIONAL([ENABLE_OVIRT], [test "x$ENABLE_OVIRT" = "xyes"])

dnl Check for external programs required to either build or run
dnl virt-v2v.
HEADING([Checking for external programs])
m4_include([m4/guestfs-progs.m4])

dnl Any C libraries required by virt-v2v.
HEADING([Checking for libraries used by virt-v2v])
m4_include([m4/guestfs-libraries.m4])

dnl OCaml.
HEADING([Checking for OCaml])
m4_include([m4/guestfs-ocaml.m4])

dnl Perl, used for running mllibvirt generator, and man pages.
HEADING([Checking for Perl])
m4_include([m4/guestfs-perl.m4])

dnl Bash completion.
HEADING([Checking for bash completion])
m4_include([m4/guestfs-bash-completion.m4])

dnl This are required to get common/*/Makefile.am files to work.  We
dnl should further decouple these in future XXX
AM_CONDITIONAL([HAVE_PYTHON],[false])  dnl Disables a test

dnl Produce output files.
HEADING([Generating output files])

AC_CONFIG_HEADERS([config.h])

AC_CONFIG_FILES([installcheck.sh],
                [chmod +x,-w installcheck.sh])
AC_CONFIG_FILES([ocaml-dep.sh],
                [chmod +x,-w ocaml-dep.sh])
AC_CONFIG_FILES([ocaml-link.sh],
                [chmod +x,-w ocaml-link.sh])
AC_CONFIG_FILES([podwrapper.pl],
                [chmod +x,-w podwrapper.pl])
AC_CONFIG_FILES([run],
                [chmod +x,-w run])

dnl NB: Remove common/mlstdutils/guestfs_config.ml in future XXX
AC_CONFIG_FILES([Makefile
                 bash/Makefile
                 common/options/Makefile
                 common/mlcustomize/Makefile
                 common/mldrivers/Makefile
                 common/mlgettext/Makefile
                 common/mlpcre/Makefile
                 common/mlstdutils/Makefile
                 common/mlstdutils/guestfs_config.ml
                 common/mltools/Makefile
                 common/mlutils/Makefile
                 common/mlxml/Makefile
                 common/qemuopts/Makefile
                 common/utils/Makefile
                 config.sh
                 convert/Makefile
                 docs/Makefile
                 gnulib/lib/Makefile
                 in-place/Makefile
                 input/Makefile
                 inspector/Makefile
                 lib/Makefile
                 lib/config.ml
                 open/Makefile
                 output/Makefile
                 po-docs/Makefile
                 po-docs/ja/Makefile
                 po-docs/uk/Makefile
                 po/Makefile
                 test-data/Makefile
                 test-data/binaries/Makefile
                 test-data/blank-disks/Makefile
                 test-data/fake-virtio-win/Makefile
                 test-data/fake-virt-tools/Makefile
                 test-data/phony-guests/Makefile
                 test-data/phony-guests/guests.xml
                 tests/functions.sh
                 tests/Makefile
                 tests/test-cdrom.xml
                 tests/test-floppy.xml
                 tests/test-mac.xml
                 tests/test-networks-and-bridges.xml
                 tests/test-print-source.xml
                 tests/test-rhbz1232192.xml
                 tests/test-sound.xml
                 v2v/Makefile])

AC_OUTPUT

dnl Produce summary.
echo
echo
echo "------------------------------------------------------------"
HEADING([Thank you for downloading $PACKAGE_STRING])
echo
echo "This is how we have configured the optional components for you today:"
echo

print ()
{
    printf '    %.40s %s\n' \
        "$1 ........................................" "$2"
}

feature ()
{
    feat="$1"
    shift
    if "$@"; then
        printf "$term_green"
        print "$feat" "yes"
    else
        printf "$term_red"
        print "$feat" "no"
    fi
    printf "$term_restore"
}

feature "OCaml gettext (for translations)" \
        test "x$HAVE_OCAML_PKG_GETTEXT_TRUE" = "x"

feature "--block-driver feature"  test "x$ENABLE_BLOCK_DRIVER" = "xyes"
feature "Xen input mode"          test "x$ENABLE_XEN" = "xyes"
feature "-o glance output mode"   test "x$ENABLE_GLANCE" = "xyes"
feature "oVirt output modes"      test "x$ENABLE_OVIRT" = "xyes"

echo
echo "Please report bugs back to the mailing list:"
echo "https://lists.libguestfs.org"
echo
echo "Next you should type 'make' to build the package,"
echo "then 'make check' to run the tests."
echo "------------------------------------------------------------"
