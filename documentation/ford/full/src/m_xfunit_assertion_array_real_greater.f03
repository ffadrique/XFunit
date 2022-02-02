module m_xfunit_assertion_array_real_greater

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests real range (greater than) array assertion
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

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_real_greater

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_real_greater

  public xfunit_assertion_array_real_greater

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The real range (greater than) array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_real_greater
  end type t_xfunit_assertion_array_real_greater

!> Constructor interface
  interface xfunit_assertion_array_real_greater
    module procedure xfunit_assertion_array_real_greater_common
    module procedure xfunit_assertion_array_real_greater_array
  end interface xfunit_assertion_array_real_greater

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real range (greater than) array assertion (common boundary)
pure function xfunit_assertion_array_real_greater_common( name, actual, low, open_low ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  real(kind=8), intent(in)  :: low

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The returned assertion
  type(t_xfunit_assertion_array_real_greater) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_greater), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_greater( name, actual, low, open_low ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_greater_index, rast )

end function xfunit_assertion_array_real_greater_common


!> Constructor for real range (greater than) array assertion
pure function xfunit_assertion_array_real_greater_array( name, actual, low, open_low ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  real(kind=8), dimension(:), intent(in)  :: low

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The returned assertion
  type(t_xfunit_assertion_array_real_greater) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_greater), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_greater( name, actual, low, open_low ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_greater_index, rast )

end function xfunit_assertion_array_real_greater_array

end module m_xfunit_assertion_array_real_greater

