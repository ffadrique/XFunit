module m_xfunit_assertion_array_real_between

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests real range array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_real_between

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_real_between

  public xfunit_assertion_array_real_between

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The real array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_real_between
  end type t_xfunit_assertion_array_real_between

!> Constructor interface
  interface xfunit_assertion_array_real_between
    module procedure xfunit_assertion_array_real_between_common
    module procedure xfunit_assertion_array_real_between_array
  end interface xfunit_assertion_array_real_between

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real array assertion (common boundaries)
pure function xfunit_assertion_array_real_between_common( name, actual, low, high, open_low, open_high ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  real(kind=8), intent(in)  :: low

!> The high bound value
  real(kind=8), intent(in)  :: high

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The high bound is open (opetional)
  logical, optional, intent(in)  :: open_high

!> The returned assertion
  type(t_xfunit_assertion_array_real_between) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_between), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_between( name, actual, low, high, open_low, open_high ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_between_index, rast )

end function xfunit_assertion_array_real_between_common


!> Constructor for real array assertion
pure function xfunit_assertion_array_real_between_array( name, actual, low, high, open_low, open_high ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The low bound value
  real(kind=8), dimension(:), intent(in)  :: low

!> The high bound value
  real(kind=8), dimension(:), intent(in)  :: high

!> The low bound is open (opetional)
  logical, optional, intent(in)  :: open_low

!> The high bound is open (opetional)
  logical, optional, intent(in)  :: open_high

!> The returned assertion
  type(t_xfunit_assertion_array_real_between) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_between), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_between( name, actual, low, high, open_low, open_high ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_between_index, rast )

end function xfunit_assertion_array_real_between_array

end module m_xfunit_assertion_array_real_between
