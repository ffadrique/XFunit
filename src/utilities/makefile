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

# Master configuration key through the compiler command
F90COMP = gfortran
F90COMP = /home/fran/gcc/gcc-10/bin/gfortran

# Reference directories
OBJDIR := Debug
MODDIR := Debug
LIBDIR := Debug

# XFunit library
LIBS = $(LIBDIR)/libxfunit_utilities.a

# Option selection for Intel Fortran (ifort)
ifeq ($(findstring ifort, $(F90COMP)), ifort)
OPTIONS = -c -fpp -g -O0 -stand f18 -traceback -check bounds -assume byterecl -diag-disable=5462 -I $(MODDIR) -module $(MODDIR) -o $@ $<
AR = ar
endif

# Option selection for GNU Fortran (gfortran)
ifeq ($(findstring gfortran, $(F90COMP)), gfortran)
OPTIONS = -c -cpp -g -O0 -std=f2008 -fbacktrace -fbounds-check -I $(MODDIR) -J $(MODDIR) -o $@ $<
AR = ar
endif

# Automatic generation of list of source files and targets
SRCS_F90 := $(wildcard *.f90)
OBJECTS_F90 := $(SRCS_F90:%.f90=$(OBJDIR)/%.o)
OBJECTS := $(OBJECTS_F90)

# Main make target (recursive in subdirectories)
all: $(LIBS)

# Target to build the library only
libs: $(LIBS)

# Build library from objects
$(LIBS): $(OBJECTS)
	@echo "(AR)" $(LIBS)
	@$(AR) -r $@ $(OBJECTS)
    
# Build objects from sources
$(OBJDIR)/%.o: %.f90
	@if [ ! -d "$(OBJDIR)" ]; then mkdir -p $(OBJDIR); fi
	@echo "(FC)" $@
	@$(F90COMP) $(OPTIONS)
	
# Target for directory clean up (recursive in subdirectories)
clean:
	@echo "(RM)" $(OBJDIR)"/*.o"
	@rm -f $(OBJDIR)/*.o
	@echo "(RM)" $(MODDIR)"/*.mod"
	@rm -f $(MODDIR)/*.mod

# Test target
test:
	echo $(OBJECTS)

# Include dependencies for individual sources/objects for compilation order
include makefile.deps
