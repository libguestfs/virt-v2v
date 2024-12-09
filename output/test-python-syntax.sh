#!/bin/bash -
# libguestfs
# Copyright (C) 2018 Red Hat Inc.
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

# Files to check.
files="$(find "$srcdir" -name '*.py')"

# Checks the files are syntactically correct, but not very much else.
for f in $files; do
    python3 -m py_compile "$f"
done

# Checks the files correspond to PEP8 coding style.
# https://www.python.org/dev/peps/pep-0008/
if test "x$PYCODESTYLE" != xno; then
    # Ignore:
    # E501 line too long
    # E722 do not use bare 'except'
    $PYCODESTYLE --ignore=E501,E722 $files
fi
