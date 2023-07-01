module m_xfunit_assertion_real_between

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests real range assertion
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

  public t_xfunit_assertion_real_between

  public xfunit_assertion_real_between

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The real range assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_real_between
    private

!     Actual asserted value
      real(kind=8) :: actual = 0.0_8

!     Lower value
      real(kind=8) :: low = 0.0_8

!     Higher value
      real(kind=8) :: high = 0.0_8

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_real_between_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_real_between_write_xml
      procedure :: write => xfunit_assertion_real_between_write

  end type t_xfunit_assertion_real_between

!---End of declaration of module variables--------------------------------------

contains

! Constructor for real range assertion
impure elemental function xfunit_assertion_real_between( name, actual, low, high, open_low, open_high ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  real(kind=8), intent(in) :: actual

! The assertion low bound value
  real(kind=8), intent(in) :: low

! The assertion high bound value
  real(kind=8), intent(in) :: high

! The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

! The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

! The returned assertion
  type(t_xfunit_assertion_real_between) :: res

! Local variables
  logical :: ok_low, ok_high
  logical :: is_open_low, is_open_high
  integer :: status

! Compute low bound reusult
  if( present(open_low) ) then
    is_open_low = open_low
  else
    is_open_low = .false.
  end if
  if( is_open_low ) then
    ok_low = ( actual > low )
  else
    ok_low = ( actual >= low )
  end if

! Compute high bound reusult
  if( present(open_high) ) then
    is_open_high = open_high
  else
    is_open_high = .false.
  end if
  if( is_open_high ) then
    ok_high = ( actual < high )
  else
    ok_high = ( actual <= high )
  end if

! Compute result
  if( ok_low .and. ok_high ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_real_between_index, status )
  res%actual   = actual
  res%low      = low
  res%high     = high

end function xfunit_assertion_real_between


! Assignment
impure elemental subroutine xfunit_assertion_real_between_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_real_between), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_real_between)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%low = other%low
      this%high = other%high

  end select

end subroutine xfunit_assertion_real_between_assign


! Serialize in XML
subroutine xfunit_assertion_real_between_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_real_between), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'low', this%low )
  call xml%write_terminal( 'high', this%high )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_real_between_write_xml


! Serialize in plain text
subroutine xfunit_assertion_real_between_write( this, unit )

! The assertion
  class(t_xfunit_assertion_real_between), intent(in) :: this

! The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(character(this%actual))
  write( unit, '(2x,a,1x,a)' ) 'high:    ', trim(character(this%high))
  write( unit, '(2x,a,1x,a)' ) 'low:     ', trim(character(this%low))

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_real_between_write

end module m_xfunit_assertion_real_between
