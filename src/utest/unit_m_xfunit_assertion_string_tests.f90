module unit_m_xfunit_assertion_string_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_string
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
  use m_string

  use m_xfunit
  use m_messages

  use m_xfunit_assertion
  use m_xfunit_assertion_string

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_string_test_001

  public unit_m_xfunit_assertion_string_test_002

  public unit_m_xfunit_assertion_string_test_003

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_string'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The error handling structure
  type(t_messages), save :: msg

! Reference string variables
  character(len=*), parameter :: cactual = 'Lorem ipsum, dolor sit amet'
  character(len=*), parameter :: cexpected = cactual

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_string_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_string) :: object

! Local variables
  integer :: status
  type(t_string) :: actual, expected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  actual = string(cactual)
  expected = string(cexpected)

! General interface
  call ut%assert_equal( 'General interface', actual, expected )

! General interface with status
  call ut%assert_equal( 'General interface with status', actual, expected, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )

! General interface
  call ut%assert_equal( 'General interface (empty strings)', '', '' )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_string_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_string_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_string) :: object

! Local variables
  integer :: status
  type(t_string) :: actual, expected
  type(t_string) :: e

! Reset error handling structures
  call msg%reset_error()

! Initialise
  actual = string(cactual)
  expected = string(cexpected)
  e = string( cexpected(:8) // '*' )

! General interface (exact)
  call ut%assert_equal( 'General interface (exact)', actual, expected, &
                        matching=xfunit_assertion_string_match_exact )

! General interface (exact) with status
  call ut%assert_equal( 'General interface (exact) with status', actual, expected, &
                        matching=xfunit_assertion_string_match_exact, status=status )
  call ut%assert_equal( 'Status (exact)', status, xfunit_assertion_is_pass )

! General interface (global)
  call ut%assert_equal( 'General interface (global)', actual, e, &
                        matching=xfunit_assertion_string_match_global )

! General interface (global) with status
  call ut%assert_equal( 'General interface (global) with status', actual, e, &
                        matching=xfunit_assertion_string_match_global, status=status )
  call ut%assert_equal( 'Status (global)', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_string_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_string_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_string) :: object

! Local variables
  integer :: status
  type(t_string) :: actual, expected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  actual = string(cactual)
  expected = string(cexpected)

! General interface (exact)
  call ut%assert_equal( 'General interface (ignorecase)', actual, expected%uppercase(), ignorecase=.true. )

! General interface (exact) with status
  call ut%assert_equal( 'General interface (exact) with status', actual, expected%lowercase(), &
                        ignorecase=.true., status=status )
  call ut%assert_equal( 'Status (exact)', status, xfunit_assertion_is_pass )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_string_test_003

end module unit_m_xfunit_assertion_string_tests
