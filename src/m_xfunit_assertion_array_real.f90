module m_xfunit_assertion_array_real

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests real array assertion
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
  use m_xfunit_assertion_real

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_real
  public xfunit_assertion_array_real

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The real array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_real
  end type t_xfunit_assertion_array_real

! Constructor interface
  interface xfunit_assertion_array_real
    module procedure xfunit_assertion_array_real_common
    module procedure xfunit_assertion_array_real_array_common
    module procedure xfunit_assertion_array_real_array
  end interface xfunit_assertion_array_real

!---End of declaration of module variables--------------------------------------

contains

! Constructor for real array assertion (common expected and threshold)
function xfunit_assertion_array_real_common( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

! The assertion expected value
  real(kind=8), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), optional, intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_array_real) :: res

! Local variables
  integer :: n
  real(kind=8) :: localt
  type(t_xfunit_assertion_real), allocatable, dimension(:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(expected)
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real( name, actual, expected, localt ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_index, rast )

end function xfunit_assertion_array_real_common


! Constructor for real array assertion (array expected and common threshold)
function xfunit_assertion_array_real_array_common( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

! The assertion expected value
  real(kind=8), dimension(:), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_array_real) :: res

! Local variables
  integer :: n
  real(kind=8), dimension(size(actual)) :: localt
  type(t_xfunit_assertion_real), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real( name, actual, expected, threshold ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_index, rast )

end function xfunit_assertion_array_real_array_common


! Constructor for real assertion
function xfunit_assertion_array_real_array( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

! The assertion expected value
  real(kind=8), dimension(:), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), optional, dimension(:), intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_array_real) :: res

! Local variables
  integer :: n
  real(kind=8), dimension(size(actual)) :: localt
  type(t_xfunit_assertion_real), allocatable, dimension(:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(expected)
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real( name, actual, expected, localt ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_index, rast )

end function xfunit_assertion_array_real_array

end module m_xfunit_assertion_array_real

