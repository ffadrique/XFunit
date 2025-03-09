module m_xfunit_assertion_logical

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests logical assertion
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
  use m_xml

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_logical

  public xfunit_assertion_logical

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The logical assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_logical
    private

!     Actual asserted value
      logical(kind=4) :: actual = .false.

!     Expected value
      logical(kind=4) :: expected = .false.

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_logical_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_logical_write_xml
      procedure :: write => xfunit_assertion_logical_write

  end type t_xfunit_assertion_logical

!---End of declaration of module variables--------------------------------------

contains

! Constructor for logical assertion
impure elemental function xfunit_assertion_logical( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  logical(kind=4), intent(in) :: actual

! The assertion expected value
  logical(kind=4), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_logical) :: res

! Local variables
  integer :: status

! Compute result
  if( actual .eqv. expected ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_logical_index, status )
  res%actual   = actual
  res%expected = expected

end function xfunit_assertion_logical


! Assignment
impure elemental subroutine xfunit_assertion_logical_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_logical), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_logical)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_logical_assign


! Serialize in XML
subroutine xfunit_assertion_logical_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_logical), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'expected', this%expected )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_logical_write_xml


! Serialize in plain text
subroutine xfunit_assertion_logical_write( this, unit )

! The assertion
  class(t_xfunit_assertion_logical), intent(in) :: this

! The XML context structure
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,l1)' ) 'actual:  ', this%actual
  write( unit, '(2x,a,1x,l1)' ) 'expected:', this%expected

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_logical_write

end module m_xfunit_assertion_logical
