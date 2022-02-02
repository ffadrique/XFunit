module m_xfunit_assertion_integer

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests integer assertion
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

  public t_xfunit_assertion_integer

  public xfunit_assertion_integer

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The integer assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_integer
    private

!     Actual asserted value
      integer(kind=8) :: actual = 0_8

!     Expected value
      integer(kind=8) :: expected = 0_8

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_integer_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_integer_write_xml
      procedure :: write => xfunit_assertion_integer_write

  end type t_xfunit_assertion_integer

!---End of declaration of module variables--------------------------------------

contains

! Constructor for integer assertion
elemental function xfunit_assertion_integer( name, actual, expected ) result(res)

! The assertion name
  character(len=*), intent(in) :: name

! The assertion actual value
  integer(kind=8), intent(in) :: actual

! The assertion expected value
  integer(kind=8), intent(in) :: expected

! The returned assertion
  type(t_xfunit_assertion_integer) :: res

! Local variables
  integer :: status

! Compute result
  if( actual == expected ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_integer_index, status )
  res%actual   = actual
  res%expected = expected

end function xfunit_assertion_integer


! Assignment
elemental subroutine xfunit_assertion_integer_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_integer), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_integer)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_integer_assign


! Serialize in XML
subroutine xfunit_assertion_integer_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_integer), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'expected', this%expected )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_integer_write_xml


! Serialize in plain text
subroutine xfunit_assertion_integer_write( this, unit )

! The assertion
  class(t_xfunit_assertion_integer), intent(in) :: this

! The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(character(this%actual))
  write( unit, '(2x,a,1x,a)' ) 'expected:', trim(character(this%expected))

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_integer_write

end module m_xfunit_assertion_integer
