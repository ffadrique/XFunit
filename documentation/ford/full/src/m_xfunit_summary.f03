module m_xfunit_summary

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Reference :
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Container of statistics information for suite and unit tests
!>
!> License   : This file is part of XFunit.
!>
!>             XFunit is free software: you can redistribute it and/or modify
!>             it under the terms of the GNU Lesser General Public License as
!>             published by the Free Software Foundation, either version 3 of
!>             the License, or (at your option) any later version.
!>
!>             XFunit is distributed in the hope that it will be useful,
!>             but WITHOUT ANY WARRANTY; without even the implied warranty of
!>             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!>             See the GNU Lesser General Public License for more details.
!>
!>             You should have received a copy of the GNU Lesser General Public
!>             License along with Zofu.
!>             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert
  use m_object
  use m_messages
  use m_xml

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_summary
  public xfunit_summary

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The suite or unit test summary type
  type, extends(t_object) :: t_xfunit_summary
    private

!>     Total number of declared tests
      integer :: tests = 0

!>     Count of executed tests
      integer :: executed = 0

!>     Count of passed tests
      integer :: passed = 0

!>     Count of failed tests
      integer :: failed = 0

!>     Count of tests in error
      integer :: errors = 0

!>     Tests execution time
      real(kind=8) :: time = 0.0_8

    contains

!>     Getters/setters
      procedure :: get_tests => xfunit_summary_get_tests
      procedure :: get_executed => xfunit_summary_get_executed
      procedure :: get_passed => xfunit_summary_get_passed
      procedure :: get_failed => xfunit_summary_get_failed
      procedure :: get_errors => xfunit_summary_get_errors
      procedure :: get_time => xfunit_summary_get_time

  end type t_xfunit_summary

!---End of declaration of module variables--------------------------------------

contains

!> Generic constructor
pure function xfunit_summary( tests, executed, passed, failed, errors, time ) result(res)

!> The tests
  integer, intent(in) :: tests

!> The executed
  integer, intent(in) :: executed

!> The passed
  integer, intent(in) :: passed

!> The failed
  integer, intent(in) :: failed

!> The errors
  integer, intent(in) :: errors

!> The time
  real(kind=8), intent(in) :: time

!> The constructed structure
  type(t_xfunit_summary) :: res

! Initialise the tests
  res%tests = tests

! Initialise the executed
  res%executed = executed

! Initialise the passed
  res%passed = passed

! Initialise the failed
  res%failed = failed

! Initialise the errors
  res%errors = errors

! Initialise the time
  res%time = time

end function xfunit_summary


! Access functions

!> Get attribute tests
elemental function xfunit_summary_get_tests( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  integer :: res

! Return the value
  res = xfunit_summary%tests

end function xfunit_summary_get_tests


!> Get attribute executed
elemental function xfunit_summary_get_executed( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  integer :: res

! Return the value
  res = xfunit_summary%executed

end function xfunit_summary_get_executed


!> Get attribute passed
elemental function xfunit_summary_get_passed( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  integer :: res

! Return the value
  res = xfunit_summary%passed

end function xfunit_summary_get_passed


!> Get attribute failed
elemental function xfunit_summary_get_failed( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  integer :: res

! Return the value
  res = xfunit_summary%failed

end function xfunit_summary_get_failed


!> Get attribute errors
elemental function xfunit_summary_get_errors( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  integer :: res

! Return the value
  res = xfunit_summary%errors

end function xfunit_summary_get_errors


!> Get attribute time
elemental function xfunit_summary_get_time( xfunit_summary ) result(res)

!> The data structure
  class(t_xfunit_summary), intent(in) :: xfunit_summary

!> The parameter value to be returned
  real(kind=8) :: res

! Return the value
  res = xfunit_summary%time

end function xfunit_summary_get_time

end module m_xfunit_summary
