#
# Makefile.am
#
# Copyright (C) 2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-jwt.
#
# guile-jwt is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guile-jwt is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guile-jwt. If not, see https://www.gnu.org/licenses/.
#

TESTS = test-hmac.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)

CLEANFILES = $(TESTS:%.scm=%.log)

EXTRA_DIST = $(TESTS) runner.scm
