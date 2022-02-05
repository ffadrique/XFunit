module unit_m_xfunit_assertion_array_logical_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_array_logical
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
  use m_xfunit_assertion_array_logical

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_array_logical_test_001

  public unit_m_xfunit_assertion_array_logical_test_002

  public unit_m_xfunit_assertion_array_logical_test_003

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_array_logical'

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

subroutine unit_m_xfunit_assertion_array_logical_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_logical) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_equal( 'General interface', &
                        [ .true._1, .false._1, .true._1, .false._1 ], &
                        [ .true._1, .false._1, .true._1, .false._1 ] )

! General interface with status
  call ut%assert_equal( 'General interface with status', &
                        [ .true._1, .false._1, .true._1, .false._1 ], &
                        [ .true._1, .false._1, .true._1, .false._1 ], &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! True direct interface
  call ut%assert_true( 'True direct interface', [ .true._1, .true._1, .true._1, .true._1 ] )

! True direct direct interface with status
  call ut%assert_true( 'True direct interface with status',  [ .true._1, .true._1, .true._1, .true._1 ], status )
  call ut%assert_equal( 'Status (true)', status, xfunit_assertion_is_pass )

! False direct interface
  call ut%assert_false( 'True direct interface', .not. [ .true._1, .true._1, .true._1, .true._1 ] )

! False direct direct interface with status
  call ut%assert_false( 'False direct interface with status', .not. [ .true._1, .true._1, .true._1, .true._1 ], status )
  call ut%assert_equal( 'Status (false)', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_logical_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_logical_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_logical) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_equal( 'General interface', &
                        [ .true._2, .false._2, .true._2, .false._2 ], &
                        [ .true._2, .false._2, .true._2, .false._2 ] )

! General interface with status
  call ut%assert_equal( 'General interface with status', &
                        [ .true._2, .false._2, .true._2, .false._2 ], &
                        [ .true._2, .false._2, .true._2, .false._2 ], &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! True direct interface
  call ut%assert_true( 'True direct interface', [ .true._2, .true._2, .true._2, .true._2 ] )

! True direct direct interface with status
  call ut%assert_true( 'True direct interface with status',  [ .true._2, .true._2, .true._2, .true._2 ], status )
  call ut%assert_equal( 'Status (true)', status, xfunit_assertion_is_pass )

! False direct interface
  call ut%assert_false( 'True direct interface', .not. [ .true._2, .true._2, .true._2, .true._2 ] )

! False direct direct interface with status
  call ut%assert_false( 'False direct interface with status', .not. [ .true._2, .true._2, .true._2, .true._2 ], status )
  call ut%assert_equal( 'Status (false)', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_logical_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_array_logical_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_array_logical) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_equal( 'General interface', &
                        [ .true., .false., .true., .false. ], &
                        [ .true., .false., .true., .false. ] )

! General interface with status
  call ut%assert_equal( 'General interface with status', &
                        [ .true., .false., .true., .false. ], &
                        [ .true., .false., .true., .false. ], &
                        status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! True direct interface
  call ut%assert_true( 'True direct interface', [ .true., .true., .true., .true. ] )

! True direct direct interface with status
  call ut%assert_true( 'True direct interface with status',  [ .true., .true., .true., .true. ], status )
  call ut%assert_equal( 'Status (true)', status, xfunit_assertion_is_pass )

! False direct interface
  call ut%assert_false( 'True direct interface', .not. [ .true., .true., .true., .true. ] )

! False direct direct interface with status
  call ut%assert_false( 'False direct interface with status', .not. [ .true., .true., .true., .true. ], status )
  call ut%assert_equal( 'Status (false)', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_array_logical_test_003

end module unit_m_xfunit_assertion_array_logical_tests
