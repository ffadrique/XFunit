program unit_m_xfunit_assertion_files

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_xfunit_assertion_files
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

  use unit_m_xfunit_assertion_files_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_files'
  
  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

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
                        source='m_xfunit_assertion_files.f90', &
                        annotation='Assertion interface for file comparison' )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_001', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_001, &
                                         annotation='Character interface (text)' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_002', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_002, &
                                         annotation='Character interface (binary)' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_003', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_003, &
                                         annotation='String interface (text)' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_004', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_004, &
                                         annotation='String interface (binary)' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_005', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_005, &
                                         annotation='Performance (text)' ) )

! Create test
  call suite%add_unit_test( xfunit_unit( name='unit_m_xfunit_assertion_files_test_006', &
                                         classname='t_xfunit_assertion_files', &
                                         executer=unit_m_xfunit_assertion_files_test_006, &
                                         annotation='Performance (binary)' ) )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit testing (to clear all memory related to testing infrastructure)
  deallocate( suite )
  deallocate( manager )

end program unit_m_xfunit_assertion_files
