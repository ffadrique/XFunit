module m_xfunit_assertion_matrix_real

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests real matrix assertion
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

!> The real matrix assertion type
  type, extends(t_xfunit_assertion_matrix) :: t_xfunit_assertion_matrix_real
  end type t_xfunit_assertion_matrix_real

!> Constructor interface
  interface xfunit_assertion_matrix_real
    module procedure xfunit_assertion_matrix_real_common
    module procedure xfunit_assertion_matrix_real_array_common
    module procedure xfunit_assertion_matrix_real_array
  end interface xfunit_assertion_matrix_real

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real matrix assertion
pure function xfunit_assertion_matrix_real_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, intent(in) :: threshold

!> The returned assertion
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


!> Constructor for real matrix assertion
pure function xfunit_assertion_matrix_real_array_common( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), intent(in) :: threshold

!> The returned assertion
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


!> Constructor for real assertion
pure function xfunit_assertion_matrix_real_array( name, actual, expected, threshold ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The assertion expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

!> The assertion evaluation threshold
  real(kind=8), optional, dimension(:,:), intent(in) :: threshold

!> The returned assertion
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
