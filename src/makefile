# Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
# Project   : Atlantis
# Author    : Fran Martinez Fadrique
# Language  : make
# Synopsis  : Makefile for XFunit library build

# License   : This file is part of XFunit.
#
#             XFunit is free software: you can redistribute it and/or modify
#             it under the terms of the GNU Lesser General Public License as
#             published by the Free Software Foundation, either version 3 of
#             the License, or (at your option) any later version.
#
#             XFunit is distributed in the hope that it will be useful,
#             but WITHOUT ANY WARRANTY; without even the implied warranty of
#             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#             See the GNU Lesser General Public License for more details.
#
#             You should have received a copy of the GNU Lesser General Public
#             License along with XFunit.  
#             If not, see <http://www.gnu.org/licenses/>.

# Main make target (recursive in subdirectories)
all:
	@cd utilities; $(MAKE)
	@cd xfunit; $(MAKE)

# Target to build the libraries only
libs:
	@cd utilities; $(MAKE) libs
	@cd xfunit; $(MAKE) libs

# Target to run the unit tests
units:
	@cd xfunit/utest; $(MAKE) units

# Target for directory clean up (recursive in subdirectories)
clean:
	@cd utilities; $(MAKE) clean
	@cd xfunit; $(MAKE) clean

# Test target
test:
	echo $(OBJECTS)

