module m_xfunit_assertion_array_complex

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests complex array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_complex

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_complex
  public xfunit_assertion_array_complex

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> Unit tests complex array assertion
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_complex
  end type t_xfunit_assertion_array_complex

!> Constructor interface
  interface xfunit_assertion_array_complex
    module procedure xfunit_assertion_array_complex_common
    module procedure xfunit_assertion_array_complex_array_common
    module procedure xfunit_assertion_array_complex_array
  end interface xfunit_assertion_array_complex

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for complex array assertion (common expected and threshold)
pure function xfunit_assertion_array_complex_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  complex(kind=8), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, intent(in) :: threshold

!> The returned assertion
  type(t_xfunit_assertion_array_complex) :: res

! Local variables
  integer :: n
  real(kind=8) :: localt
  type(t_xfunit_assertion_complex), allocatable, dimension(:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(real(actual))
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_complex( name, actual, expected, threshold ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_complex_index, rast )

end function xfunit_assertion_array_complex_common


!> Constructor for complex array assertion (array expected and common threshold)
pure function xfunit_assertion_array_complex_array_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  complex(kind=8), dimension(:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), intent(in) :: threshold

!> The returned assertion
  type(t_xfunit_assertion_array_complex) :: res

! Local variables
  integer :: n
  type(t_xfunit_assertion_complex), allocatable, dimension(:) :: rast

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_complex( name, actual, expected, threshold ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_complex_index, rast )

end function xfunit_assertion_array_complex_array_common


!> Constructor for complex assertion
pure function xfunit_assertion_array_complex_array( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The assertion expected value
  complex(kind=8), dimension(:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, dimension(:), intent(in) :: threshold

!> The returned assertion
  type(t_xfunit_assertion_array_complex) :: res

! Local variables
  integer :: n
  real(kind=8), dimension(size(actual)) :: localt
  type(t_xfunit_assertion_complex), allocatable, dimension(:) :: rast

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = xfunit_real_scale * epsilon(real(actual))
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_complex( name, actual, expected, localt ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, xfunit_assertion_array_complex_index, rast )

end function xfunit_assertion_array_complex_array

end module m_xfunit_assertion_array_complex

