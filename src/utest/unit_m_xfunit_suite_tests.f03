module unit_m_xfunit_suite_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_suite
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

  use m_string
  use m_xfunit
  use m_messages
  use m_string

  use m_xfunit_suite

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_xfunit_suite_test_001

  public unit_m_xfunit_suite_test_002

  public unit_m_xfunit_suite_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_suite'
  
  character(len=130), parameter :: sccs_info = &
  '$Id: $'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite
  type(t_xfunit_unit), save :: ut

! The error handling structure
  type(t_messages), save :: msg


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_suite_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_xfunit_suite) :: opt

! Reset error handling structures
  call msg%reset_error()

! Initialise option
  opt =  xfunit_suite( package='xfunit', &
                       source='m_xfunit_suite.f90', &
                       annotation='Autotest: m_xfunit_suite constructor' )

! Verify construction
  call ut%assert_equal( 'Name', opt%get_name(), string('m_xfunit_suite') )
  call ut%assert_equal( 'Package', opt%get_package(), string('xfunit') )
  call ut%assert_equal( 'Source', opt%get_source(), string('m_xfunit_suite.f90') )
  call ut%assert_equal( 'Annotation', opt%get_annotation(), string('Autotest: m_xfunit_suite constructor') )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_suite_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_suite_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_suite_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_suite_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_suite_test_003

end module unit_m_xfunit_suite_tests

