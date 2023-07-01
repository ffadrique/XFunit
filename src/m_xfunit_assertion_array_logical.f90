module m_xfunit_assertion_array_logical

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests logical array assertion
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

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_logical

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_logical
  public xfunit_assertion_array_logical

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The logical array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_logical
  end type t_xfunit_assertion_array_logical

! Constructor interface
  interface xfunit_assertion_array_logical
    module procedure xfunit_assertion_array_logical_common
    module procedure xfunit_assertion_array_logical_array
  end interface xfunit_assertion_array_logical

!---End of declaration of module variables--------------------------------------

contains

! Constructor for logical array assertion (common expected)
function xfunit_assertion_array_logical_common( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  logical(kind=4), dimension(:), intent(in) :: actual

! The assertion expected value
  logical(kind=4), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_array_logical) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_logical), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_logical( name, actual, expected ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_logical_index, rast )

end function xfunit_assertion_array_logical_common


! Constructor for logical array assertion
function xfunit_assertion_array_logical_array( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  logical(kind=4), dimension(:), intent(in) :: actual

! The assertion expected value
  logical(kind=4), dimension(:), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_array_logical) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_logical), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_logical( name, actual, expected ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_logical_index, rast )

end function xfunit_assertion_array_logical_array

end module m_xfunit_assertion_array_logical

