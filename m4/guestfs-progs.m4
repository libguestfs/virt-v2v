# libguestfs
# Copyright (C) 2009-2020 Red Hat Inc.
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

# Check for external programs required to either build or run
# libguestfs.
#
# AC_CHECK_PROG(S) or AC_PATH_PROG(S)?
#
# Use AC_CHECK_PROG(S) for programs which are only used during build.
#
# Use AC_PATH_PROG(S) for program names which are compiled into the
# binary and used at run time.  The reason is so that we know which
# programs the binary actually uses.

# Define $(SED).
m4_ifdef([AC_PROG_SED],[
    AC_PROG_SED
],[
    dnl ... else hope for the best
    AC_SUBST([SED], "sed")
])

# Define $(AWK).
AC_PROG_AWK

AC_PROG_LN_S

dnl Check for xorriso/genisoimage/mkisofs.
AC_PATH_PROGS([MKISOFS],[xorrisofs genisoimage mkisofs],[no],
    [$PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin])
test "x$MKISOFS" = "xno" && AC_MSG_ERROR([xorriso or genisoimage or mkisofs must be installed])

dnl po4a for translating man pages and POD files (optional).
AC_CHECK_PROG([PO4A_GETTEXTIZE],[po4a-gettextize],[po4a-gettextize],[no])
AC_CHECK_PROG([PO4A_TRANSLATE],[po4a-translate],[po4a-translate],[no])
AM_CONDITIONAL([HAVE_PO4A], [test "x$PO4A_GETTEXTIZE" != "xno" && test "x$PO4A_TRANSLATE" != "xno"])

dnl Check for sqlite3 (optional).
AC_CHECK_PROG([SQLITE3],[sqlite3],[sqlite3],[no])

dnl zip/unzip, used by virt-v2v
AC_PATH_PROGS([ZIP],[zip],[no])
AC_DEFINE_UNQUOTED([ZIP],["$ZIP"],[Name of zip program.])
AM_CONDITIONAL([HAVE_ZIP],[test "x$ZIP" != "xno"])
AC_PATH_PROGS([UNZIP],[unzip],[no])
AC_DEFINE_UNQUOTED([UNZIP],["$UNZIP"],[Name of unzip program.])

dnl nbdinfo, nbdcopy, required by virt-v2v
AC_CHECK_PROG([NBDINFO], [nbdinfo], [nbdinfo], [no])
AC_CHECK_PROG([NBDCOPY], [nbdcopy], [nbdcopy], [no])
AS_IF([test "x$NBDINFO" = "xno" || test "x$NBDCOPY" = "xno"],
      [AC_MSG_ERROR([nbdinfo and nbdcopy (from libnbd) must be installed])])

dnl Check for valgrind
AC_CHECK_PROG([VALGRIND],[valgrind],[valgrind],[no])
AS_IF([test "x$VALGRIND" != "xno"],[
    # Substitute the whole valgrind command.
    # --read-inline-info=no is a temporary workaround for RHBZ#1662656.
    VG='libtool --mode=execute $(VALGRIND) --vgdb=no --leak-check=full --error-exitcode=119 --suppressions=$(abs_top_srcdir)/valgrind-suppressions --trace-children=no --child-silent-after-fork=yes --run-libc-freeres=no --num-callers=100 --read-inline-info=no'
    ],[
    # No valgrind, so substitute VG with something that will break.
    VG=VALGRIND_IS_NOT_INSTALLED
])
AC_SUBST([VG])
AM_SUBST_NOTMAKE([VG])

dnl pycodestyle, used to check the Python scripts.
AC_CHECK_PROGS([PYCODESTYLE],[pycodestyle],[no])
