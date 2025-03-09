module m_xfunit_assertion_string

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests string assertion
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

  public t_xfunit_assertion_string

  public xfunit_assertion_string

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The string assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_string
    private

!     Actual asserted value
      type(t_string) :: actual

!     Expected value
      type(t_string) :: expected

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_string_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_string_write_xml
      procedure :: write => xfunit_assertion_string_write

  end type t_xfunit_assertion_string

!---End of declaration of module variables--------------------------------------

contains

! Constructor for string assertion
impure elemental function xfunit_assertion_string( name, actual, expected, matching, ignorecase ) result(res)

! The assertion name
  character(len=*), intent(in)  :: name

! The assertion actual value
  type(t_string), intent(in)  :: actual

! The assertion expected value
  type(t_string), intent(in)  :: expected

! The character matching strategy (optional, default to exact)
! Enumerated values in m_xfunit_assertion
  integer, optional, intent(in)  :: matching

! Ignore case in comparison
  logical, optional, intent(in)  :: ignorecase

! The returned assertion
  type(t_xfunit_assertion_string) :: res

! Local variables
  integer :: lactual, lexpected
  logical :: lmatch
  integer :: status
  logical :: icase
  integer :: assertion_case_index
  integer :: imatch
  type(t_string) :: xactual
  type(t_string) :: xexpected

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
    imatch = xfunit_assertion_string_match_exact
  end if

! Initialise locals
  lactual = len_trim(actual)
  lexpected = len_trim(expected)
  if( icase ) then
    xactual = actual%lowercase()
    xexpected = expected%lowercase()
  else
    xactual = actual
    xexpected = expected
  end if

! Compute result
  select case( imatch )

!   Do the global match check
    case( xfunit_assertion_string_match_global )
    lmatch = match( xactual, xexpected )

!   Do the regular expression check
!   NOT IMPLEMENTED YET
    case( xfunit_assertion_string_match_regexp )
      lmatch = .false.

!   Do the exact check
    case default
      lmatch = ( lactual == lexpected )
      if( lmatch ) lmatch = ( string(xactual,1,lactual) == string(xexpected,1,lactual) )

  end select

! Set the precise assertion type
  if( icase ) then
    assertion_case_index = xfunit_assertion_string_nocase_index
  else
    assertion_case_index = xfunit_assertion_string_index
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
  res%actual   = trim(actual)
  res%expected = trim(expected)

end function xfunit_assertion_string


! Assignment
impure elemental subroutine xfunit_assertion_string_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_string), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_string)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_string_assign


! Serialize in XML
subroutine xfunit_assertion_string_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_string), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'expected', this%expected )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_string_write_xml


! Serialize in plain text
subroutine xfunit_assertion_string_write( this, unit )

! The assertion
  class(t_xfunit_assertion_string), intent(in) :: this

! The XML context structure
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', character(this%actual)
  write( unit, '(2x,a,1x,a)' ) 'expected:', character(this%expected)

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_string_write

end module m_xfunit_assertion_string
