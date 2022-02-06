module unit_m_xfunit_assertion_real_between_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_real_between
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
  use m_xfunit_assertion_real_between

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_real_between_test_001

  public unit_m_xfunit_assertion_real_between_test_002

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_real_between'

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

subroutine unit_m_xfunit_assertion_real_between_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_real_between) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_between( 'General interface', 2.0, 1.0, 3.0 )
  call ut%assert_between( 'General interface (low bound)', 1.0, 1.0, 3.0 )
  call ut%assert_between( 'General interface (high bound)', 3.0, 1.0, 3.0 )

! General interface with status
  call ut%assert_between( 'General interface with status', 2.0, 1.0, 3.0, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_between( 'General interface (open_low)', 2.0, 1.0, 3.0, open_low=.true. )

! General interface (open_low) with status
  call ut%assert_between( 'General interface (open_low) with status', 2.0, 1.0, 3.0, open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_high)
  call ut%assert_between( 'General interface (open_high)', 2.0, 1.0, 3.0, open_high=.true. )

! General interface (open_high) with status
  call ut%assert_between( 'General interface (open_high) with status', 2.0, 1.0, 3.0, open_high=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_real_between_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_real_between_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_real_between) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! General interface
  call ut%assert_between( 'General interface', 2.0_8, 1.0_8, 3.0_8 )
  call ut%assert_between( 'General interface (low bound)', 1.0_8, 1.0_8, 3.0_8 )
  call ut%assert_between( 'General interface (high bound)', 3.0_8, 1.0_8, 3.0_8 )

! General interface with status
  call ut%assert_between( 'General interface with status', 2.0_8, 1.0_8, 3.0_8, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_low)
  call ut%assert_between( 'General interface (open_low)', 2.0_8, 1.0_8, 3.0_8, open_low=.true. )

! General interface (open_low) with status
  call ut%assert_between( 'General interface (open_low) with status', 2.0_8, 1.0_8, 3.0_8, open_low=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface (open_high)
  call ut%assert_between( 'General interface (open_high)', 2.0_8, 1.0_8, 3.0_8, open_high=.true. )

! General interface (open_high) with status
  call ut%assert_between( 'General interface (open_high) with status', 2.0_8, 1.0_8, 3.0_8, open_high=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_real_between_test_002

end module unit_m_xfunit_assertion_real_between_tests
