# Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
# Project   : Atlantis
# Author    : Fran Martinez Fadrique
# Language  : make
# Synopsis  : Makefile for unit test build and execution

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

# Master configuration key through the compiler command
F90COMP = gfortran

# Reference directories
OBJDIR := Debug
MODDIR := Debug
LIBDIR := Debug
BINDIR := Debug

IMODDIR := ../Debug
ILIBDIR := ../Debug

# XFunit library
LIBS = $(ILIBDIR)/libxfunit.a

# Option selection for Intel Fortran (ifort)
ifeq ($(findstring ifort, $(F90COMP)), ifort)
OPTIONS = -fpp -O0 -stand f18 -traceback -check bounds -assume byterecl -diag-disable=5462 -I $(IMODDIR) -I $(MODDIR) -module $(MODDIR) -o $@ -free -Tf $< -Tf $*.f03
AR = ar
endif

# Option selection for GNU Fortran (gfortran)
ifeq ($(findstring gfortran, $(F90COMP)), gfortran)
OPTIONS = -cpp -O0 -std=f2008 -fbacktrace -fbounds-check -I $(IMODDIR) -I $(MODDIR) -J $(MODDIR) -o $@ $^
AR = ar
endif

# Automatic generation of list of source files and targets
SRCS_MAIN_F03 := $(shell ls unit_*.f03 | grep -v _tests)
BINS_MAIN := $(SRCS_MAIN_F03:%.f03=$(BINDIR)/%.bin)

# List of unit test output files (JUnit-like files)
JUNITS := $(SRCS_MAIN_F03:unit_%.f03=%.jxml)

# Main make target 
all: $(BINS_MAIN)

# Build executables from related sources
$(OBJDIR)/%.bin: %_tests.f03 %.f03 
	@if [ ! -d "$(BINDIR)" ]; then mkdir -p $(OBJDIR); fi
	@echo "(FC)" $@
	@$(F90COMP) $(OPTIONS) $(LIBS)

# Target for unit test execution
units: $(JUNITS)

# Execute unit test
%.jxml: $(BINDIR)/unit_%.bin
	@echo "(UT)" $@
	@export XFUNIT_ROOT_DIR=$(PWD);  \
    $< > /dev/null

# Target for directory clean up
clean:
	@echo "(RM)" $(OBJDIR)"/*.bin"
	@rm -f $(OBJDIR)/*.bin
	@echo "(RM)" $(MODDIR)"/*.mod"
	@rm -f $(MODDIR)/*.mod
	@echo "(RM) *.jxml"
	@rm -f *.jxml
	@echo "(RM) *.out"
	@rm -f *.out

# Test target
test:
	echo $(JUNITS)
