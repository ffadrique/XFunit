module m_xfunit_assertion_array_class

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests infinite polymorphic array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_class

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_class
  public xfunit_assertion_array_class

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> Unit tests infinite polymorphic array assertion
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_class
  end type t_xfunit_assertion_array_class

!> Constructor interface
  interface xfunit_assertion_array_class
    module procedure xfunit_assertion_array_class_common
    module procedure xfunit_assertion_array_class_array
  end interface xfunit_assertion_array_class

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for infinite polymorphic array assertion (common expected)
function xfunit_assertion_array_class_common( name, actual, expected, equal, serialize ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  class(*), dimension(:), intent(in) :: actual

!> The assertion expected value
  class(*), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The returned assertion
  type(t_xfunit_assertion_array_class) :: res

! Local variables
  integer :: i, n
  type(t_xfunit_assertion_class), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_class) :: mold

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n) )
  do i = 1, n
    rast(i) = xfunit_assertion_class( name, actual(i), expected, equal, serialize )
  end do
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_class_index, rast, mold )

end function xfunit_assertion_array_class_common


!> Constructor for infinite polymorphic array assertion
function xfunit_assertion_array_class_array( name, actual, expected, equal, serialize ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  class(*), dimension(:), intent(in) :: actual

!> The assertion expected value
  class(*), dimension(:), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The returned assertion
  type(t_xfunit_assertion_array_class) :: res

! Local variables
  integer :: i, n
  type(t_xfunit_assertion_class), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_class) :: mold

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n) )
  do i = 1, n
    rast(i) = xfunit_assertion_class( name, actual(i), expected(i), equal, serialize )
  end do
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_class_index, rast, mold )

end function xfunit_assertion_array_class_array

end module m_xfunit_assertion_array_class

