program unit_m_xfunit_assertion_integer_between

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_xfunit_assertion_integer_between
!
! License   : This file is part of XFunit.
!
!             XFunit is free software: you can redistribute it and/or modify
!             it under the terms of the GNU Lesser General Public License as
!             published by the Free Software Foundation, either version 3 of
!             the License, or (at your option) any later version.
!
!             XFunit is distributed in the hope that it will be useful,
!             but WITHOUT ANY WARRANTY; without even the implied warranty of
!             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!             See the GNU Lesser General Public License for more details.
!
!             You should have received a copy of the GNU Lesser General Public
!             License along with XFunit.  
!             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit

  use unit_m_xfunit_assertion_integer_between_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_integer_between'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! Test strucutures. Allocatable to deallocate before exit and facilitate dynamic analysis, e.g. valgrind
  type(t_xfunit_manager), allocatable :: manager
  type(t_xfunit_suite), allocatable :: suite

! Test configuration variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Initialise report generation flag
  junit_strict = .false.

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  allocate( manager, source=xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict ) )

! Initialise test suite
  allocate( suite )
  suite = xfunit_suite( package=package, &
                        source='m_xfunit_assertion_integer_between.f90', &
                        annotation='Assertion interface for integer between values' )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_integer_between_test_001', &
                                         classname='t_xfunit_assertion_integer_between', &
                                         executer=unit_m_xfunit_assertion_integer_between_test_001, &
                                         annotation='Integer kind=1' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_integer_between_test_002', &
                                         classname='t_xfunit_assertion_integer_between', &
                                         executer=unit_m_xfunit_assertion_integer_between_test_002, &
                                         annotation='Integer kind=2' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_integer_between_test_003', &
                                         classname='t_xfunit_assertion_integer_between', &
                                         executer=unit_m_xfunit_assertion_integer_between_test_003, &
                                         annotation='Integer kind=4' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_integer_between_test_004', &
                                         classname='t_xfunit_assertion_integer_between', &
                                         executer=unit_m_xfunit_assertion_integer_between_test_004, &
                                         annotation='Integer kind=8' ) )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit testing (to clear all memory realted to testing infrastructure)
  deallocate( suite )
  deallocate( manager )

end program unit_m_xfunit_assertion_integer_between
