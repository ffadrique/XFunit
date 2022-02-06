module unit_m_xfunit_assertion_matrix_integer_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_matrix_integer
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
  use m_xfunit_assertion_matrix_integer

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_matrix_integer_test_001

  public unit_m_xfunit_assertion_matrix_integer_test_002

  public unit_m_xfunit_assertion_matrix_integer_test_003

  public unit_m_xfunit_assertion_matrix_integer_test_004

  public unit_m_xfunit_assertion_matrix_integer_test_005

  public unit_m_xfunit_assertion_matrix_integer_test_006

  public unit_m_xfunit_assertion_matrix_integer_test_007

  public unit_m_xfunit_assertion_matrix_integer_test_008

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_matrix_integer'
  
  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=1 general interface
  call ut%assert_equal( 'General matrix', &
                        reshape( [ 1_1, 2_1, 3_1, 4_1, 5_1, 6_1 ], [ 2, 3 ] ), &
                        reshape( [ 1_1, 2_1, 3_1, 4_1, 5_1, 6_1 ], [ 2, 3 ] ) )
  
! Test interface for integer kind=1 general interface with status
  call ut%assert_equal( 'General matrix with status', &
                        reshape( [ 1_1, 2_1, 3_1, 4_1, 5_1, 6_1 ], [ 2, 3 ] ), &
                        reshape( [ 1_1, 2_1, 3_1, 4_1, 5_1, 6_1 ], [ 2, 3 ] ), &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=1 common expected value interface
  call ut%assert_equal( 'General matrix', reshape( [ 1_1, 1_1, 1_1, 1_1, 1_1, 1_1 ], [ 2, 3 ] ), 1_1 )
  
! Test interface for integer kind=1 common expected value interface with status
  call ut%assert_equal( 'General matrix with status', reshape( [ 1_1, 1_1, 1_1, 1_1, 1_1, 1_1 ], [ 2, 3 ] ), 1_1, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=2 general interface
  call ut%assert_equal( 'General matrix', &
                        reshape( [ 1_2, 2_2, 3_2, 4_2, 5_2, 6_2 ], [ 2, 3 ] ), &
                        reshape( [ 1_2, 2_2, 3_2, 4_2, 5_2, 6_2 ], [ 2, 3 ] ) )

! Test interface for integer kind=2 general interface with statuts
  call ut%assert_equal( 'General matrix with status', &
                        reshape( [ 1_2, 2_2, 3_2, 4_2, 5_2, 6_2 ], [ 2, 3 ] ), &
                        reshape( [ 1_2, 2_2, 3_2, 4_2, 5_2, 6_2 ], [ 2, 3 ] ), &
                        status)
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=2 common expected value interface
  call ut%assert_equal( 'Common expected value', reshape( [ 1_2, 1_2, 1_2, 1_2, 1_2, 1_2 ], [ 2, 3 ] ), 1_2 )
  
! Test interface for integer kind=2 common expected value interface with status
  call ut%assert_equal( 'Common expected value with status', reshape( [ 1_2, 1_2, 1_2, 1_2, 1_2, 1_2 ], [ 2, 3 ] ), 1_2, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=4 general interface
  call ut%assert_equal( 'General matrix', &
                        reshape( [ 1_4, 2_4, 3_4, 4_4, 5_4, 6_4 ], [ 2, 3 ] ), &
                        reshape( [ 1_4, 2_4, 3_4, 4_4, 5_4, 6_4 ], [ 2, 3 ] ) )
                        
! Test interface for integer kind=4 general interface with status
  call ut%assert_equal( 'General matrix with status', &
                        reshape( [ 1_4, 2_4, 3_4, 4_4, 5_4, 6_4 ], [ 2, 3 ] ), &
                        reshape( [ 1_4, 2_4, 3_4, 4_4, 5_4, 6_4 ], [ 2, 3 ] ), &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  
                        
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=4 common expected value interface
  call ut%assert_equal( 'Common expected value', reshape( [ 1_4, 1_4, 1_4, 1_4, 1_4, 1_4 ], [ 2, 3 ] ), 1_4 )
  
! Test interface for integer kind=4 common expected value interface with status
  call ut%assert_equal( 'Common expected value with status', reshape( [ 1_4, 1_4, 1_4, 1_4, 1_4, 1_4 ], [ 2, 3 ] ), 1_4, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=8 general interface
  call ut%assert_equal( 'General matrix', &
                        reshape( [ 1_8, 2_8, 3_8, 4_8, 5_8, 6_8 ], [ 2, 3 ] ), &
                        reshape( [ 1_8, 2_8, 3_8, 4_8, 5_8, 6_8 ], [ 2, 3 ] ) )
                        
! Test interface for integer kind=8 general interface with status
  call ut%assert_equal( 'General matrix with status', &
                        reshape( [ 1_8, 2_8, 3_8, 4_8, 5_8, 6_8 ], [ 2, 3 ] ), &
                        reshape( [ 1_8, 2_8, 3_8, 4_8, 5_8, 6_8 ], [ 2, 3 ] ), &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  
                        
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_matrix_integer_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_matrix_integer) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Test interface for integer kind=8 common expected value interface
  call ut%assert_equal( 'Common expected value', reshape( [ 1_8, 1_8, 1_8, 1_8, 1_8, 1_8 ], [ 2, 3 ] ), 1_8 )

! Test interface for integer kind=8 common expected value interface with status
  call ut%assert_equal( 'Common expected value with status', reshape( [ 1_8, 1_8, 1_8, 1_8, 1_8, 1_8 ], [ 2, 3 ] ), 1_8, status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_matrix_integer_test_008

end module unit_m_xfunit_assertion_matrix_integer_tests
