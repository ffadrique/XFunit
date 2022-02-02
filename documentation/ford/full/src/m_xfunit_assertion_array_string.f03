module m_xfunit_assertion_array_string

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests string array assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_string
  use m_util_convert

  use m_xfunit_assertion
  use m_xfunit_assertion_array
  use m_xfunit_assertion_string

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_string

  public xfunit_assertion_array_string

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The string array assertion type
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_string
  end type t_xfunit_assertion_array_string

!> Constructor interface
  interface xfunit_assertion_array_string
    module procedure xfunit_assertion_array_string_common
    module procedure xfunit_assertion_array_string_array
  end interface xfunit_assertion_array_string

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for string array assertion (common expected)
pure function xfunit_assertion_array_string_common( name, actual, expected, matching, ignorecase ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  type(t_string), dimension(:), intent(in)  :: actual

!> The assertion expected value
  type(t_string), intent(in)  :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in)  :: matching

!> Ignore case in comparison
  logical, optional, intent(in)  :: ignorecase

!> The returned assertion
  type(t_xfunit_assertion_array_string) :: res

! Local variables
  integer :: n
  logical :: icase
  integer :: assertion_case_index
  type(t_xfunit_assertion_string), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_string) :: mold

! Check case utilisation
  if( present(ignorecase) ) then
    icase = ignorecase
  else
    icase = .false.
  end if

! Initialise the common information
  if( icase ) then
    assertion_case_index = xfunit_assertion_array_string_nocase_index
  else
    assertion_case_index = xfunit_assertion_array_string_index
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_string( name, actual, expected, matching=matching, ignorecase=ignorecase ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, assertion_case_index, rast, mold )

end function xfunit_assertion_array_string_common


!> Constructor for string array assertion
pure function xfunit_assertion_array_string_array( name, actual, expected, matching, ignorecase ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion actual value
  type(t_string), dimension(:), intent(in)  :: actual

!> The assertion expected value
  type(t_string), dimension(:), intent(in)  :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in)  :: matching

!> Ignore case in comparison
  logical, optional, intent(in)  :: ignorecase

!> The returned assertion
  type(t_xfunit_assertion_array_string) :: res

! Local variables
  integer :: n
  logical :: icase
  integer :: assertion_case_index
  type(t_xfunit_assertion_string), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_string) :: mold

! Check case utilisation
  if( present(ignorecase) ) then
    icase = ignorecase
  else
    icase = .false.
  end if

! Initialise the common information
  if( icase ) then
    assertion_case_index = xfunit_assertion_array_string_nocase_index
  else
    assertion_case_index = xfunit_assertion_array_string_index
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_string( name, actual, expected, matching=matching, ignorecase=ignorecase ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, assertion_case_index, rast, mold )

end function xfunit_assertion_array_string_array

end module m_xfunit_assertion_array_string

