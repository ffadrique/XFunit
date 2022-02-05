module m_xfunit_assertion_matrix_real

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests real matrix assertion
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
  use m_xfunit_assertion_matrix
  use m_xfunit_assertion_real

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_matrix_real
  public xfunit_assertion_matrix_real

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The real matrix assertion type
  type, extends(t_xfunit_assertion_matrix) :: t_xfunit_assertion_matrix_real
  end type t_xfunit_assertion_matrix_real

! Constructor interface
  interface xfunit_assertion_matrix_real
    module procedure xfunit_assertion_matrix_real_common
    module procedure xfunit_assertion_matrix_real_array_common
    module procedure xfunit_assertion_matrix_real_array
  end interface xfunit_assertion_matrix_real

!---End of declaration of module variables--------------------------------------

contains

! Constructor for real matrix assertion
pure function xfunit_assertion_matrix_real_common( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

! The assertion expected value
  real(kind=8), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), optional, intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_matrix_real) :: res

! Local variables
  integer :: n1, n2
  real(kind=8), dimension(size(actual,1),size(actual,2)) :: localt
  type(t_xfunit_assertion_real), allocatable, dimension(:,:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(expected)
  end if

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2), source=xfunit_assertion_real( name, actual, expected, localt ) )
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_real_index, rast )

end function xfunit_assertion_matrix_real_common


! Constructor for real matrix assertion
pure function xfunit_assertion_matrix_real_array_common( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

! The assertion expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_matrix_real) :: res

! Local variables
  integer :: n1, n2
  type(t_xfunit_assertion_real), allocatable, dimension(:,:) :: rast

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2), source=xfunit_assertion_real( name, actual, expected, threshold ) )
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_real_index, rast )

end function xfunit_assertion_matrix_real_array_common


! Constructor for real assertion
pure function xfunit_assertion_matrix_real_array( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

! The assertion expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

! The assertion evaluation threshold
  real(kind=8), optional, dimension(:,:), intent(in) :: threshold

! The returned assertion
  type(t_xfunit_assertion_matrix_real) :: res

! Local variables
  integer :: n1, n2
  real(kind=8), dimension(size(actual,1),size(actual,2)) :: localt
  type(t_xfunit_assertion_real), allocatable, dimension(:,:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(expected)
  end if

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2), source=xfunit_assertion_real( name, actual, expected, localt ) )
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_real_index, rast )

end function xfunit_assertion_matrix_real_array

end module m_xfunit_assertion_matrix_real
