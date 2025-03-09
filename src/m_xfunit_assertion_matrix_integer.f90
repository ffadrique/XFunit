module m_xfunit_assertion_matrix_integer

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests integer matrix assertion
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
  use m_xfunit_assertion_integer

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_matrix_integer
  public xfunit_assertion_matrix_integer

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The integer matrix assertion type
  type, extends(t_xfunit_assertion_matrix) :: t_xfunit_assertion_matrix_integer
  end type t_xfunit_assertion_matrix_integer

! Constructor interface
  interface xfunit_assertion_matrix_integer
    module procedure xfunit_assertion_matrix_integer_common
    module procedure xfunit_assertion_matrix_integer_array
  end interface xfunit_assertion_matrix_integer

!---End of declaration of module variables--------------------------------------

contains

! Constructor for integer matrix assertion (common expected)
function xfunit_assertion_matrix_integer_common( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  integer(kind=8), dimension(:,:), intent(in) :: actual

! The assertion expected value
  integer(kind=8), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_matrix_integer) :: res

! Local variables
  integer :: n1, n2
  type(t_xfunit_assertion_integer), allocatable, dimension(:,:) :: rast

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2), source=xfunit_assertion_integer( name, actual, expected ) )
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_integer_index, rast )

end function xfunit_assertion_matrix_integer_common


! Constructor for integer matrix assertion
function xfunit_assertion_matrix_integer_array( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  integer(kind=8), dimension(:,:), intent(in) :: actual

! The assertion expected value
  integer(kind=8), dimension(:,:), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_matrix_integer) :: res

! Local variables
  integer :: n1, n2
  type(t_xfunit_assertion_integer), allocatable, dimension(:,:) :: rast

! Store the matrix information (assume all matrixs conform to actual)
  n1 = size(actual,1)
  n2 = size(actual,2)
  allocate( rast(n1,n2), source=xfunit_assertion_integer( name, actual, expected ) )
  res%t_xfunit_assertion_matrix = xfunit_assertion_matrix( name, xfunit_assertion_matrix_integer_index, rast )

end function xfunit_assertion_matrix_integer_array

end module m_xfunit_assertion_matrix_integer

