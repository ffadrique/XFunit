module m_xfunit_assertion_array_real

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests real array assertion
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

!> The real array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_real
  end type t_xfunit_assertion_array_real

!> Constructor interface
  interface xfunit_assertion_array_real
    module procedure xfunit_assertion_array_real_common
    module procedure xfunit_assertion_array_real_array_common
    module procedure xfunit_assertion_array_real_array
  end interface xfunit_assertion_array_real

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real array assertion (common expected and threshold)
pure function xfunit_assertion_array_real_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, intent(in) :: threshold

!> The returned assertion
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


!> Constructor for real array assertion (array expected and common threshold)
pure function xfunit_assertion_array_real_array_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), dimension(:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), intent(in) :: threshold

!> The returned assertion
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


!> Constructor for real assertion
pure function xfunit_assertion_array_real_array( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), dimension(:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, dimension(:), intent(in) :: threshold

!> The returned assertion
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

