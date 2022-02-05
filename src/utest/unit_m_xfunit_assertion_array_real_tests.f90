module unit_m_xfunit_assertion_array_real_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_array_real
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

  use m_xfunit_assertion
  use m_xfunit_assertion_array_real

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_array_real_test_001

  public unit_m_xfunit_assertion_array_real_test_002

  public unit_m_xfunit_assertion_array_real_test_003

  public unit_m_xfunit_assertion_array_real_test_004

  public unit_m_xfunit_assertion_array_real_test_005

  public unit_m_xfunit_assertion_array_real_test_006

  public unit_m_xfunit_assertion_array_real_test_007

  public unit_m_xfunit_assertion_array_real_test_008

  public unit_m_xfunit_assertion_array_real_test_009

  public unit_m_xfunit_assertion_array_real_test_010

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_array_real'
  
  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The error handling structure
  type(t_messages), save :: msg

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  real, dimension(3,3) :: actual, expected, threshold

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=4 general interface
  call ut%assert_equal( 'General array', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 0.1_4, 0.1_4, 0.1_4, 0.1_4, 0.1_4, 0.1_4 ] )

! Test interface for real kind=4 general interface with status
  call ut%assert_equal( 'General array with status', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 0.1_4, 0.1_4, 0.1_4, 0.1_4, 0.1_4, 0.1_4 ], &
                        status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=4 with common threshold
  call ut%assert_equal( 'Common threshold', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        0.1_4 )

! Test interface for real kind=4 with common threshold with statuts
  call ut%assert_equal( 'Common threshold with status', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        0.1_4, &
                        status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=4 with common expected value and threshold
  call ut%assert_equal( 'Common expected value and threshold', &
                        [ 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4 ], &
                        1.0_4, &
                        0.1_4 )

! Test interface for real kind=4 with common expected value and threshold with status
  call ut%assert_equal( 'Common expected value and threshold with status', &
                        [ 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4 ], &
                        1.0_4, &
                        0.1_4, &
                        status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=8 general interface
  call ut%assert_equal( 'General array', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 0.1_8, 0.1_8, 0.1_8, 0.1_8, 0.1_8, 0.1_8 ] )
  
! Test interface for real kind=8 general interface with status
  call ut%assert_equal( 'General array with status', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 0.1_8, 0.1_8, 0.1_8, 0.1_8, 0.1_8, 0.1_8 ], &
                        status )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=8 common threshold
  call ut%assert_equal( 'Common threshold', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        0.1_8 )

! Test interface for real kind=8 common threshold with status
  call ut%assert_equal( 'Common threshold with status', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        0.1_8, &
                        status )

      ! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=8 with common expected value and threshold
  call ut%assert_equal( 'Common expected value and threshold', &
                        [ 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8 ], &
                        1.0_8, &
                        0.1_8 )

! Test interface for real kind=8 with common expected value and threshold with status
  call ut%assert_equal( 'Common expected value and threshold with status', &
                        [ 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8 ], &
                        1.0_8, &
                        0.1_8, &
                        status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  real, dimension(3,3) :: actual, expected, threshold

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=4 general interface
  call ut%assert_equal( 'General array with default threshold', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ] )

! Test interface for real kind=4 general interface with status
  call ut%assert_equal( 'General array with default threshold with status', &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        [ 1.0_4, 2.0_4, 3.0_4, 4.0_4, 5.0_4, 6.0_4 ], &
                        status=status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=4 with common expected value and threshold
  call ut%assert_equal( 'Common expected value and default threshold', &
                        [ 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4 ], &
                        1.0_4 )

! Test interface for real kind=4 with common expected value and threshold with status
  call ut%assert_equal( 'Common expected value and default threshold with status', &
                        [ 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4, 1.0_4 ], &
                        1.0_4, &
                        status=status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_008

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_009( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  real, dimension(3,3) :: actual, expected, threshold

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=8 general interface
  call ut%assert_equal( 'General array with default threshold', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ] )

! Test interface for real kind=8 general interface with status
  call ut%assert_equal( 'General array with default threshold with status', &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8 ], &
                        status=status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_009

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_real_test_010( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  integer :: status

! Local variables
  character(len=:), allocatable :: clocal

! Reset error handling structures
  call msg%reset_error()

! Test interface for real kind=8 with common expected value and threshold
  call ut%assert_equal( 'Common expected value and default threshold', &
                        [ 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8 ], &
                        1.0_8 )

! Test interface for real kind=8 with common expected value and threshold with status
  call ut%assert_equal( 'Common expected value and default threshold with status', &
                        [ 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8 ], &
                        1.0_8, &
                        status=status )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_real_test_010

end module unit_m_xfunit_assertion_array_real_tests
