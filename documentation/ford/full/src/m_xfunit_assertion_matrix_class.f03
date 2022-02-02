module m_xfunit_assertion_matrix_class

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests infinite polymorphic matrix assertion
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
  use m_xfunit_assertion_matrix
  use m_xfunit_assertion_class

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_matrix_class
  public xfunit_assertion_matrix_class

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The infinite polymorphic matrix assertion type
  type, extends(t_xfunit_assertion_matrix) :: t_xfunit_assertion_matrix_class
  end type t_xfunit_assertion_matrix_class

!> Constructor interface
  interface xfunit_assertion_matrix_class
    module procedure xfunit_assertion_matrix_class_common
    module procedure xfunit_assertion_matrix_class_array
  end interface xfunit_assertion_matrix_class

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for infinite polymorphic matrix assertion (common expected)
function xfunit_assertion_matrix_class_common( name, actual, expected, equal, serialize ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  class(*), dimension(:,:), intent(in) :: actual

!> The assertion expected value
  class(*), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The returned assertion
  type(t_xfunit_assertion_matrix_class) :: res

! Local variables
  integer :: i1, i2, n1, n2
  type(t_xfunit_assertion_class), allocatable, dimension(:,:) :: rast
  type(t_xfunit_assertion_class) :: mold

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2) )
  do i1 = 1, n1
    do i2 = 1, n2
      rast(i1,i2) = xfunit_assertion_class( name, actual(i1,i2), expected, equal, serialize )
    end do
  end do
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_class_index, rast, mold )

end function xfunit_assertion_matrix_class_common


!> Constructor for infinite polymorphic matrix assertion
function xfunit_assertion_matrix_class_array( name, actual, expected, equal, serialize ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  class(*), dimension(:,:), intent(in) :: actual

!> The assertion expected value
  class(*), dimension(:,:), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The returned assertion
  type(t_xfunit_assertion_matrix_class) :: res

! Local variables
  integer :: i1, i2, n1, n2
  type(t_xfunit_assertion_class), allocatable, dimension(:,:) :: rast
  type(t_xfunit_assertion_class) :: mold

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2) )
  do i1 = 1, n1
    do i2 = 1, n2
      rast(i1,i2) = xfunit_assertion_class( name, actual(i1,i2), expected(i1,i2), equal, serialize )
    end do
  end do
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_class_index, rast, mold )

end function xfunit_assertion_matrix_class_array

end module m_xfunit_assertion_matrix_class

