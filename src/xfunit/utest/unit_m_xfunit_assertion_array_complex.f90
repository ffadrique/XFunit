program unit_m_xfunit_assertion_array_complex

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_xfunit_assertion_array_complex
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

  use unit_m_xfunit_assertion_array_complex_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_array_complex'
  
  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_unit), allocatable :: ut

! Initialise report generation flag
  junit_strict = .false.

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  allocate( manager, source=xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict ) )

! Initialise test suite
  allocate( suite )
  allocate( ut )
  suite = xfunit_suite( package=package, &
                        source='m_xfunit_assertion_array_complex.f90', &
                        annotation='Assertion interface for complex array' )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_001', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_001, &
                    annotation='Array complex(kind=4) general' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_002', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_002, &
                    annotation='Array complex(kind=4) with common threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_003', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_003, &
                    annotation='Array complex(kind=4) with common expected value and threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_004', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_004, &
                    annotation='Array complex(kind=8) general' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_005', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_005, &
                    annotation='Array complex(kind=8) with common threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_006', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_006, &
                    annotation='Array complex(kind=8) with common expected value and threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_007', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_007, &
                    annotation='Array complex(kind=4) general with default threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_008', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_008, &
                    annotation='Array complex(kind=4) with common expected value and default threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_009', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_009, &
                    annotation='Array complex(kind=8) general with default threshold' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_array_complex_test_010', &
                    classname='t_xfunit_assertion_array_complex', &
                    executer=unit_m_xfunit_assertion_array_complex_test_010, &
                    annotation='Array complex(kind=8) with common expected value and default threshold' )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit testing
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_m_xfunit_assertion_array_complex
