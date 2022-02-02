module m_xfunit_assertion_array_integer

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests integer array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_integer

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_integer
  public xfunit_assertion_array_integer

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The integer array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_integer
  end type t_xfunit_assertion_array_integer

!> Constructor interface
  interface xfunit_assertion_array_integer
    module procedure xfunit_assertion_array_integer_common
    module procedure xfunit_assertion_array_integer_array
  end interface xfunit_assertion_array_integer

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for integer array assertion (common exepcted)
pure function xfunit_assertion_array_integer_common( name, actual, expected ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value (common)
  integer(kind=8), intent(in) :: expected

!> The returned assertion
  type(t_xfunit_assertion_array_integer) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_integer), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_integer( name, actual, expected ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_integer_index, rast )

end function xfunit_assertion_array_integer_common


!> Constructor for integer array assertion
pure function xfunit_assertion_array_integer_array( name, actual, expected ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  integer(kind=8), dimension(:), intent(in) :: expected

!> The returned assertion
  type(t_xfunit_assertion_array_integer) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_integer), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_integer( name, actual, expected ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_integer_index, rast )

end function xfunit_assertion_array_integer_array

end module m_xfunit_assertion_array_integer

