module m_xfunit_assertion_array_real_less

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests real range (less than) array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_real_less

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_real_less
  public xfunit_assertion_array_real_less

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The real range (less than) array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_real_less
  end type t_xfunit_assertion_array_real_less

!> Constructor interface
  interface xfunit_assertion_array_real_less
    module procedure xfunit_assertion_array_real_less_common
    module procedure xfunit_assertion_array_real_less_array
  end interface xfunit_assertion_array_real_less

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real range (less than) array assertion (common boundary)
pure function xfunit_assertion_array_real_less_common( name, actual, high, open_high ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The high bound value
  real(kind=8), intent(in)  :: high

!> The high bound is open (opetional)
  logical, optional, intent(in)  :: open_high

!> The returned assertion
  type(t_xfunit_assertion_array_real_less) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_less), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_less( name, actual, high, open_high ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_less_index, rast )

end function xfunit_assertion_array_real_less_common


!> Constructor for real range (less than) array assertion
pure function xfunit_assertion_array_real_less_array( name, actual, high, open_high ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in)  :: actual

!> The high bound value
  real(kind=8), dimension(:), intent(in)  :: high

!> The high bound is open (opetional)
  logical, optional, intent(in)  :: open_high

!> The returned assertion
  type(t_xfunit_assertion_array_real_less) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_real_less), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_real_less( name, actual, high, open_high ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_real_less_index, rast )

end function xfunit_assertion_array_real_less_array

end module m_xfunit_assertion_array_real_less
