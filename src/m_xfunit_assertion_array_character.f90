module m_xfunit_assertion_array_character

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests character array assertion
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
  use m_xfunit_assertion_array
  use m_xfunit_assertion_character

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_array_character

  public xfunit_assertion_array_character

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! Unit tests character array assertion
  type, extends(t_xfunit_assertion_array) :: t_xfunit_assertion_array_character
  end type t_xfunit_assertion_array_character

! Constructor interface
  interface xfunit_assertion_array_character
    module procedure xfunit_assertion_array_character_common
    module procedure xfunit_assertion_array_character_array
  end interface xfunit_assertion_array_character

!---End of declaration of module variables--------------------------------------

contains

! Constructor for character array assertion (common expected)
function xfunit_assertion_array_character_common( name, actual, expected, matching, ignorecase ) result(res)

! The assertion name
  character(len=*), intent(in)  :: name

! The assertion actual value
  character(len=*), dimension(:), intent(in)  :: actual

! The assertion expected value
  character(len=*), intent(in)  :: expected

! The character matching strategy (optional, default to exact)
! Enumerated values in m_xfunit_assertion
  integer, optional, intent(in)  :: matching

! Ignore case in comparison
  logical, optional, intent(in)  :: ignorecase

! The returned assertion
  type(t_xfunit_assertion_array_character) :: res

! Local variables
  integer :: n
  logical :: icase
  integer :: assertion_case_index
  type(t_xfunit_assertion_character), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_character) :: mold

! Check case utilisation
  if( present(ignorecase) ) then
    icase = ignorecase
  else
    icase = .false.
  end if

! Initialise the common information
  if( icase ) then
    assertion_case_index = xfunit_assertion_array_character_nocase_index
  else
    assertion_case_index = xfunit_assertion_array_character_index
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_character( name, actual, expected, matching=matching, ignorecase=ignorecase ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, assertion_case_index, rast, mold )

end function xfunit_assertion_array_character_common


! Constructor for character array assertion
function xfunit_assertion_array_character_array( name, actual, expected, matching, ignorecase ) result(res)

! The assertion name
  character(len=*), intent(in)  :: name

! The assertion actual value
  character(len=*), dimension(:), intent(in)  :: actual

! The assertion expected value
  character(len=*), dimension(:), intent(in)  :: expected

! The character matching strategy (optional, default to exact)
! Enumerated values in m_xfunit_assertion
  integer, optional, intent(in)  :: matching

! Ignore case in comparison
  logical, optional, intent(in)  :: ignorecase

! The returned assertion
  type(t_xfunit_assertion_array_character) :: res

! Local variables
  integer :: n
  logical :: icase
  integer :: assertion_case_index
  type(t_xfunit_assertion_character), allocatable, dimension(:) :: rast
  type(t_xfunit_assertion_character) :: mold

! Check case utilisation
  if( present(ignorecase) ) then
    icase = ignorecase
  else
    icase = .false.
  end if

! Initialise the common information
  if( icase ) then
    assertion_case_index = xfunit_assertion_array_character_nocase_index
  else
    assertion_case_index = xfunit_assertion_array_character_index
  end if

! Store the array information (assume all arrays conform to actual)
  n = size(actual)
  allocate( rast(n), source=xfunit_assertion_character( name, actual, expected, matching=matching, ignorecase=ignorecase ) )
  res%t_xfunit_assertion_array = xfunit_assertion_array( name, assertion_case_index, rast, mold )

end function xfunit_assertion_array_character_array

end module m_xfunit_assertion_array_character

