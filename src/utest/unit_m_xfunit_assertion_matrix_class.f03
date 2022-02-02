program unit_m_xfunit_assertion_matrix_class

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_xfunit_assertion_matrix_class
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

  use unit_m_xfunit_assertion_matrix_class_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_matrix_class'

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
                        source='m_xfunit_assertion_matrix_class.f03', &
                        before=unit_m_xfunit_assertion_matrix_class_suite_before, &
                        annotation='Assertion interface for class(*) matrix' )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_matrix_class_test_001', &
                    classname='t_xfunit_assertion_matrix_class', &
                    executer=unit_m_xfunit_assertion_matrix_class_test_001, &
                    annotation='Matrix class(*) general' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_matrix_class_test_002', &
                    classname='t_xfunit_assertion_matrix_class', &
                    executer=unit_m_xfunit_assertion_matrix_class_test_002, &
                    annotation='Matrix class(*) general without serializer' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_matrix_class_test_003', &
                    classname='t_xfunit_assertion_matrix_class', &
                    executer=unit_m_xfunit_assertion_matrix_class_test_003, &
                    annotation='Matrix class(*) common expected value' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_xfunit_assertion_matrix_class_test_004', &
                    classname='t_xfunit_assertion_matrix_class', &
                    executer=unit_m_xfunit_assertion_matrix_class_test_004, &
                    annotation='Matrix class(*) common expected value without serializer' )
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

end program unit_m_xfunit_assertion_matrix_class
