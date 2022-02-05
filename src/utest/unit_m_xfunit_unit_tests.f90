module unit_m_xfunit_unit_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_unit
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
  use m_messages

  use m_xfunit_unit

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_xfunit_unit_test_001

  public unit_m_xfunit_unit_test_002

  public unit_m_xfunit_unit_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_unit'
  
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

subroutine unit_m_xfunit_unit_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Populate the test case
  call generate_simple_equality_assertions( ut )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_unit_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_unit_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Populate the test case
  call generate_simple_equality_assertions( ut )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_unit_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_unit_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Populate the test case
  call generate_array_equality_assertions( ut )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_unit_test_003

! ############################################################################
! ### DATA GENERATION ROUTINE ################################################
! ############################################################################

subroutine generate_simple_equality_assertions( ut )

! The test case
  type(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1)  :: test_integer_1 = 4_1
  integer(kind=2)  :: test_integer_2 = 4_2
  integer(kind=4)  :: test_integer_4 = 4_4
  real             :: test_real    = 4.0
  double precision :: test_double  = 4.0_8
  logical(kind=1)  :: test_logical_1 = .true._1
  logical(kind=2)  :: test_logical_2 = .true._2
  logical(kind=4)  :: test_logical_4 = .true._4
  character(len=4) :: test_character = '44  '
  integer :: status

!! Assert integer value
!  call ut%assert_equal( 'test_integer_1', test_integer_1, 4_1, status=status )
!  call ut%assert_equal( 'test_integer_2', test_integer_2, 4_2, status=status )
!  call ut%assert_equal( 'test_integer_4', test_integer_4, 4_4, status=status )
!
!! Assert real value
!  call ut%assert_equal( 'test_real', test_real, 4.0, status=status )
!  call ut%assert_equal( 'test_real', test_real, 4.0, threshold=0.1, status=status )
!  call ut%assert_equal( 'test_real', test_real, 4.095, 0.1, status )
!  
!! Assert double value
!  call ut%assert_equal( 'test_double', test_double, 4.0_8, status=status )
!  call ut%assert_equal( 'test_double', test_double, 4.0_8, 0.1_8, status )
!  call ut%assert_equal( 'test_double', test_double, 4.095_8, 0.1_8, status )
!
!! Assert logical value
!  call ut%assert_equal( 'test_logical_1', test_logical_1, .true._1, status=status )
!  call ut%assert_equal( 'test_logical_2', test_logical_2, .true._2, status=status )
!  call ut%assert_equal( 'test_logical_4', test_logical_4, .true._4, status=status )
!
!! Assert character value
!  call ut%assert_equal( 'test_character', test_character, '44', status=status )
!  call ut%assert_equal( 'test_character', test_character, '44  ', status=status )

end subroutine generate_simple_equality_assertions


subroutine generate_array_equality_assertions( ut )

! The test case
  type(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1),  dimension(4) :: test_integer_1 = 4_1
  integer(kind=2),  dimension(4) :: test_integer_2 = 4_2
  integer(kind=4),  dimension(4) :: test_integer_4 = 4_4
  real,             dimension(4) :: test_real    = 4.0
  double precision, dimension(4) :: test_double  = 4.0_8
  logical(kind=1),  dimension(4) :: test_logical_1 = .true._1
  logical(kind=2),  dimension(4) :: test_logical_2 = .true._2
  logical(kind=4),  dimension(4) :: test_logical_4 = .true._4
  character(len=4), dimension(4) :: test_character = '44'
  integer :: status

  integer(kind=1),  dimension(4) :: expect_integer_1 = (/ 4_1, 4_1, 4_1, 4_1 /)
  integer(kind=2),  dimension(4) :: expect_integer_2 = (/ 4_2, 4_2, 4_2, 4_2 /)
  integer(kind=4),  dimension(4) :: expect_integer_4 = (/ 4_4, 4_4, 4_4, 4_4 /)
  real,             dimension(4) :: expect_real      = (/ 4.0, 4.0, 4.0, 4.0 /)
  double precision, dimension(4) :: expect_double    = (/ 4.0_8, 4.0_8, 4.0_8, 4.0_8 /)
  logical(kind=1),  dimension(4) :: expect_logical_1 = (/ .true._1, .true._1, .true._1, .true._1 /)
  logical(kind=2),  dimension(4) :: expect_logical_2 = (/ .true._2, .true._2, .true._2, .true._2 /)
  logical(kind=4),  dimension(4) :: expect_logical_4 = (/ .true._4, .true._4, .true._4, .true._4 /)
  character(len=4), dimension(4) :: expect_character = (/ '44', '44', '44', '44' /)

!! Assert integer value
!  call ut%assert_equal( 'test_integer_1', test_integer_1, expect_integer_1, status )
!  call ut%assert_equal( 'test_integer_2', test_integer_2, expect_integer_2, status )
!  call ut%assert_equal( 'test_integer_4', test_integer_4, expect_integer_4, status )
!
!! Assert real value
!  call ut%assert_equal( 'test_real', test_real, expect_real, status=status )
!  call ut%assert_equal( 'test_real', test_real, expect_real, 0.2, status )
!  
!! Assert double value
!  call ut%assert_equal( 'test_double', test_double, expect_double, status=status )
!  call ut%assert_equal( 'test_double', test_double, expect_double, 0.2_8, status )
!  
!! Assert logical value
!  call ut%assert_equal( 'test_logical_1', test_logical_1, expect_logical_1, status )
!  call ut%assert_equal( 'test_logical_2', test_logical_2, expect_logical_2, status )
!  call ut%assert_equal( 'test_logical_4', test_logical_4, expect_logical_4, status )
!  
!! Assert character value
!  call ut%assert_equal( 'test_character', test_character, expect_character, status=status )

end subroutine generate_array_equality_assertions

end module unit_m_xfunit_unit_tests

