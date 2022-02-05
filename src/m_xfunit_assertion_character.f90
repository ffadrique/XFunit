module m_xfunit_assertion_character

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests character assertion
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

  use m_string
  use m_util_convert

  use m_xfunit_assertion
  use m_xml

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_character

  public xfunit_assertion_character

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The character assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_character
    private

!     Actual asserted value
      character(len=:), allocatable :: actual

!     Expected value
      character(len=:), allocatable :: expected

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_character_assign

!     Serialization interface
      procedure :: write_xml => xfunit_assertion_character_write_xml
      procedure :: write => xfunit_assertion_character_write

  end type t_xfunit_assertion_character

!---End of declaration of module variables--------------------------------------

contains

! Constructor for character assertion
elemental function xfunit_assertion_character( name, actual, expected, matching, ignorecase ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  character(len=*), intent(in) :: actual

! The assertion expected value
  character(len=*), intent(in) :: expected

! The character matching strategy (optional, default to exact)
! Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

! Ignore case in comparison
  logical, optional, intent(in) :: ignorecase

! The returned assertion
  type(t_xfunit_assertion_character) :: res

! Local variables
  integer :: lactual, lexpected
  logical :: lmatch
  integer :: status
  logical :: icase
  integer :: assertion_case_index
  integer :: imatch
  character(len=:), allocatable :: xactual
  character(len=:), allocatable :: xexpected

! Check case utilisation
  if( present(ignorecase) ) then
    icase = ignorecase
  else
    icase = .false.
  end if

! Check matching strtegy
  if( present(matching) ) then
    imatch = matching
  else
    imatch = xfunit_assertion_character_match_exact
  end if

! Initialise locals
  lactual = len_trim(actual)
  lexpected = len_trim(expected)
  if( icase ) then
    xactual = lowercase(actual)
    xexpected = lowercase(expected)
  else
    xactual = actual
    xexpected = expected
  end if

! Compute result
  select case( imatch )

!   Do the global match check
    case( xfunit_assertion_character_match_global )
      lmatch = match( xactual, xexpected )

!   Do the regular expression check
!   NOT IMPLEMENTED YET
    case( xfunit_assertion_character_match_regexp )
      lmatch = .false.

!   Do the exact check
    case default
      lmatch = ( lactual == lexpected )
      if( lmatch ) lmatch = ( xactual(:lactual) == xexpected(:lactual) )

  end select

! Set the precise assertion type
  if( icase ) then
    assertion_case_index = xfunit_assertion_character_nocase_index
  else
    assertion_case_index = xfunit_assertion_character_index
  end if

! Set the assertions status
  if( lmatch ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Invoke the base assertion constructor
  res%t_xfunit_assertion = xfunit_assertion( name, assertion_case_index, status )

! Complete the assertion construction
  if( len_trim(actual) > 0 ) then
    res%actual   = trim(actual)
  else
    res%actual = ' '
  end if
  if( len_trim(expected) > 0 ) then
    res%expected = trim(expected)
  else
    res%expected = ' '
  end if

end function xfunit_assertion_character


! Assignment
elemental subroutine xfunit_assertion_character_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_character), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_character)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_character_assign


! Serialize in XML
subroutine xfunit_assertion_character_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_character), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'expected', this%expected )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_character_write_xml


! Serialize in plain text
subroutine xfunit_assertion_character_write( this, unit )

! The assertion
  class(t_xfunit_assertion_character), intent(in) :: this

! The open fortran unit
  integer, intent(in) :: unit

! Local variables
  type(t_xml_encoder) :: encoder
  character(len=:), allocatable :: text

! Write the assertion header
  call this%write_header( unit )

! Initialise encoder
  encoder = xml_encoder()

! Write the assertion details
  text = encoder%encode( this%actual )
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(text)
  text = encoder%encode( this%expected )
  write( unit, '(2x,a,1x,a)' ) 'expected:', trim(text)

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_character_write

end module m_xfunit_assertion_character

