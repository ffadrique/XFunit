module unit_m_xfunit_assertion_array_integer_greater_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object oriented Fortran 2006/2008
! Synopsis  : Unit tests for m_xfunit_assertion_array_integer_greater
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
  use m_xfunit_assertion_array_integer_greater

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_array_integer_greater_test_001

  public unit_m_xfunit_assertion_array_integer_greater_test_002

  public unit_m_xfunit_assertion_array_integer_greater_test_003

  public unit_m_xfunit_assertion_array_integer_greater_test_004

  public unit_m_xfunit_assertion_array_integer_greater_test_005

  public unit_m_xfunit_assertion_array_integer_greater_test_006

  public unit_m_xfunit_assertion_array_integer_greater_test_007

  public unit_m_xfunit_assertion_array_integer_greater_test_008

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_array_integer_greater'

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

subroutine unit_m_xfunit_assertion_array_integer_greater_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_greater( 'General interface', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          [ 1_1, 1_1, 1_1, 1_1 ] )
  call ut%assert_greater( 'General interface (low bound)', &
                          [ 1_1, 1_1, 1_1, 1_1 ], &
                          [ 1_1, 1_1, 1_1, 1_1 ] )

! General interface with status
  call ut%assert_greater( 'General interface with status', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          [ 1_1, 1_1, 1_1, 1_1 ], &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_greater( 'General interface (open_low)', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          [ 1_1, 1_1, 1_1, 1_1 ], &
                          open_low=.true. )

! General interface (open_low) with status
  call ut%assert_greater( 'General interface (open_low) with status', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          [ 1_1, 1_1, 1_1, 1_1 ], &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected low interface
  call ut%assert_greater( 'Common expected low interface', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          1_1 )
  call ut%assert_greater( 'Common expected low interface (low bound)', &
                          [ 1_1, 1_1, 1_1, 1_1 ], &
                          1_1 )

! Common expected low interface with status
  call ut%assert_greater( 'Common expected low interface with status', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          1_1, &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Common expected low interface (open_low)
  call ut%assert_greater( 'Common expected low interface (open_low)', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          1_1, &
                          open_low=.true. )

! Common expected low interface (open_low) with status
  call ut%assert_greater( 'Common expected low interface (open_low) with status', &
                          [ 2_1, 2_1, 2_1, 2_1 ], &
                          1_1, &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_greater( 'General interface', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          [ 1_2, 1_2, 1_2, 1_2 ] )
  call ut%assert_greater( 'General interface (low bound)', &
                          [ 1_2, 1_2, 1_2, 1_2 ], &
                          [ 1_2, 1_2, 1_2, 1_2 ] )

! General interface with status
  call ut%assert_greater( 'General interface with status', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          [ 1_2, 1_2, 1_2, 1_2 ], &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_greater( 'General interface (open_low)', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          [ 1_2, 1_2, 1_2, 1_2 ], &
                          open_low=.true. )

! General interface (open_low) with status
  call ut%assert_greater( 'General interface (open_low) with status', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          [ 1_2, 1_2, 1_2, 1_2 ], &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected low interface
  call ut%assert_greater( 'Common expected low interface', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          1_2 )
  call ut%assert_greater( 'Common expected low interface (low bound)', &
                          [ 1_2, 1_2, 1_2, 1_2 ], &
                          1_2 )

! Common expected low interface with status
  call ut%assert_greater( 'Common expected low interface with status', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          1_2, &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Common expected low interface (open_low)
  call ut%assert_greater( 'Common expected low interface (open_low)', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          1_2, &
                          open_low=.true. )

! Common expected low interface (open_low) with status
  call ut%assert_greater( 'Common expected low interface (open_low) with status', &
                          [ 2_2, 2_2, 2_2, 2_2 ], &
                          1_2, &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_greater( 'General interface', &
                          [ 2, 2, 2, 2 ], &
                          [ 1, 1, 1, 1 ] )
  call ut%assert_greater( 'General interface (low bound)', &
                          [ 1, 1, 1, 1 ], &
                          [ 1, 1, 1, 1 ] )

! General interface with status
  call ut%assert_greater( 'General interface with status', &
                          [ 2, 2, 2, 2 ], &
                          [ 1, 1, 1, 1 ], &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_greater( 'General interface (open_low)', &
                          [ 2, 2, 2, 2 ], &
                          [ 1, 1, 1, 1 ], &
                          open_low=.true. )

! General interface (open_low) with status
  call ut%assert_greater( 'General interface (open_low) with status', &
                          [ 2, 2, 2, 2 ], &
                          [ 1, 1, 1, 1 ], &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected low interface
  call ut%assert_greater( 'Common expected low interface', &
                          [ 2, 2, 2, 2 ], &
                          1 )
  call ut%assert_greater( 'Common expected low interface (low bound)', &
                          [ 1, 1, 1, 1 ], &
                          1 )

! Common expected low interface with status
  call ut%assert_greater( 'Common expected low interface with status', &
                          [ 2, 2, 2, 2 ], &
                          1, &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Common expected low interface (open_low)
  call ut%assert_greater( 'Common expected low interface (open_low)', &
                          [ 2, 2, 2, 2 ], &
                          1, &
                          open_low=.true. )

! Common expected low interface (open_low) with status
  call ut%assert_greater( 'Common expected low interface (open_low) with status', &
                          [ 2, 2, 2, 2 ], &
                          1, &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_greater( 'General interface', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          [ 1_8, 1_8, 1_8, 1_8 ] )
  call ut%assert_greater( 'General interface (low bound)', &
                          [ 1_8, 1_8, 1_8, 1_8 ], &
                          [ 1_8, 1_8, 1_8, 1_8 ] )

! General interface with status
  call ut%assert_greater( 'General interface with status', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          [ 1_8, 1_8, 1_8, 1_8 ], &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_greater( 'General interface (open_low)', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          [ 1_8, 1_8, 1_8, 1_8 ], &
                          open_low=.true. )

! General interface (open_low) with status
  call ut%assert_greater( 'General interface (open_low) with status', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          [ 1_8, 1_8, 1_8, 1_8 ], &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_integer_greater_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_integer_greater) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Common expected low interface
  call ut%assert_greater( 'Common expected low interface', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          1_8 )
  call ut%assert_greater( 'Common expected low interface (low bound)', &
                          [ 1_8, 1_8, 1_8, 1_8 ], &
                          1_8 )

! Common expected low interface with status
  call ut%assert_greater( 'Common expected low interface with status', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          1_8, &
                          status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Common expected low interface (open_low)
  call ut%assert_greater( 'Common expected low interface (open_low)', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          1_8, &
                          open_low=.true. )

! Common expected low interface (open_low) with status
  call ut%assert_greater( 'Common expected low interface (open_low) with status', &
                          [ 2_8, 2_8, 2_8, 2_8 ], &
                          1_8, &
                          open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_integer_greater_test_008

end module unit_m_xfunit_assertion_array_integer_greater_tests
