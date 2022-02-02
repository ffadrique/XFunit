module m_xfunit_assertion_array_integer_greater

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests integer range (greater than) array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_integer_greater

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_integer_greater
  public xfunit_assertion_array_integer_greater

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The integer range (greater than) array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_integer_greater
  end type t_xfunit_assertion_array_integer_greater

!> Constructor interface
  interface xfunit_assertion_array_integer_greater
    module procedure xfunit_assertion_array_integer_greater_common
    module procedure xfunit_assertion_array_integer_greater_array
  end interface xfunit_assertion_array_integer_greater

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for integer range (greater than) array assertion (common boundary)
pure function xfunit_assertion_array_integer_greater_common( name, actual, low, open_low ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  integer(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  integer(kind=8), intent(in)  :: low

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The returned assertion
  type(t_xfunit_assertion_array_integer_greater) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_integer_greater), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_integer_greater( name, actual, low, open_low ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_integer_greater_index, rast )

end function xfunit_assertion_array_integer_greater_common


!> Constructor for integer range (greater than) array assertion
pure function xfunit_assertion_array_integer_greater_array( name, actual, low, open_low ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  integer(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  integer(kind=8), dimension(:), intent(in)  :: low

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The returned assertion
  type(t_xfunit_assertion_array_integer_greater) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_integer_greater), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_integer_greater( name, actual, low, open_low ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_integer_greater_index, rast )

end function xfunit_assertion_array_integer_greater_array

end module m_xfunit_assertion_array_integer_greater

