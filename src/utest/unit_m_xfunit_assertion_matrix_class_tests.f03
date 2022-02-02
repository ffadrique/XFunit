module unit_m_xfunit_assertion_matrix_class_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_matrix_class
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

  use m_util_convert
  use m_xfunit
  use m_messages

  use m_xfunit_assertion
  use m_xfunit_assertion_matrix_class

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_matrix_class_suite_before

  public unit_m_xfunit_assertion_matrix_class_test_001

  public unit_m_xfunit_assertion_matrix_class_test_002

  public unit_m_xfunit_assertion_matrix_class_test_003

  public unit_m_xfunit_assertion_matrix_class_test_004

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_matrix_class'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! Type to be used as input for the assertions
  type t_test
    integer :: i = 0
    real :: r = 0.0
    character(len=10) :: c
  end type t_test

! Test variables
  type(t_test), parameter :: expected0 = t_test( 1, 1.0, 'One' )
  type(t_test), parameter, dimension(3,2) :: expected = &
    reshape( [ t_test( 1, 1.0, 'One'), &
               t_test( 2, 2.0, 'Two'), &
               t_test( 3, 3.0, 'Three'), &
               t_test( 4, 4.0, 'Four'), &
               t_test( 5, 5.0, 'Five'), &
               t_test( 6, 6.0, 'Six') ], [ 3, 2 ] )
  type(t_test), dimension(3,2), save :: actual0
  type(t_test), dimension(3,2), save :: actual

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! # Auxiliary functions for object comparison in assertion ###################
! # Follows interface in m_xfunit_assertion_class ############################
! ############################################################################

pure function test_equal( left, right ) result(res)
  class(*), intent(in) :: left
  class(*), intent(in) :: right
  logical :: res
  select type( left )
    type is( t_test )
      select type( right )
        type is( t_test )
          res = left%i == right%i
      end select
  end select
end function test_equal

pure function test_serialize( this ) result(res)
  class(*), intent(in) :: this
  character(len=:), allocatable :: res
  select type( this )
    type is( t_test )
      res = trim(character(this%i)) // ': ' // trim(character(this%r)) // ' (' // trim(this%c) // ')'
  end select
end function test_serialize

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_class_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_class) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_equal( 'General interface', actual, expected, test_equal, test_serialize )

! General interface with status
  call ut%assert_equal( 'General interface', actual, expected, test_equal, test_serialize, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_class_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_class_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_class) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface without serializer
  call ut%assert_equal( 'General interface without serializer', actual, expected, test_equal )

! General interface without serializer with status
  call ut%assert_equal( 'General interface without serializer with status', actual, expected, test_equal, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_class_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_class_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_class) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected value interface
  call ut%assert_equal( 'Common expected value', actual0, expected0, test_equal, test_serialize )

! Common expected value interface with status
  call ut%assert_equal( 'Common expected value with status', actual0, expected0, test_equal, test_serialize, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_class_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_class_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_class) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected value interface without serializer
  call ut%assert_equal( 'Common expected value without serializer', actual0, expected0, test_equal )

! Common expected value interface without serializer with status
  call ut%assert_equal( 'Common expected value without serializer with status', actual0, expected0, test_equal, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_class_test_004

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_class_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite

! Initialise comparison variables
  actual0 = expected0
  actual = expected

end subroutine unit_m_xfunit_assertion_matrix_class_suite_before

end module unit_m_xfunit_assertion_matrix_class_tests
