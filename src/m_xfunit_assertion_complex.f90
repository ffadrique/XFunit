module m_xfunit_assertion_complex

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests complex assertion
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

  public t_xfunit_assertion_complex

  public xfunit_assertion_complex

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The complex assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_complex
    private

!     Actual asserted value
      complex(kind=8) :: actual = 0.0_8

!     Expected value
      complex(kind=8) :: expected = 0.0_8

!     Threshold
      real(kind=8) :: threshold = 0.0_8

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_complex_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_complex_write_xml
      procedure :: write => xfunit_assertion_complex_write

  end type t_xfunit_assertion_complex

!---End of declaration of module variables--------------------------------------

contains

! Constructor for complex assertion from character
impure elemental function xfunit_assertion_complex( name, actual, expected, threshold ) result(res)

! The assertion name
  character(len=*), intent(in)  :: name

! The assertion actual value
  complex(kind=8), intent(in)  :: actual

! The assertion expected value
  complex(kind=8), intent(in)  :: expected

! The assertion evaluation threshold
  real(kind=8), optional, intent(in)  :: threshold

! The returned assertion
  type(t_xfunit_assertion_complex) :: res

! Local variables
  integer :: status
  real(kind=8) :: localt

! Check threshold present
  if( present(threshold) ) then
    localt = threshold
  else
    localt = epsilon(real(expected))
  end if

! Compute result
  if( abs( actual - expected ) < abs(localt) ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_complex_index, status )
  res%actual   = actual
  res%expected = expected
  res%threshold = localt

end function xfunit_assertion_complex


! Assignment
impure elemental subroutine xfunit_assertion_complex_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_complex), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_complex)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_complex_assign


! Serialize in XML
subroutine xfunit_assertion_complex_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_complex), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'expected', this%expected )
  call xml%write_terminal( 'threshold', this%threshold )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_complex_write_xml


! Serialize in plain text
subroutine xfunit_assertion_complex_write( this, unit )

! The assertion
  class(t_xfunit_assertion_complex), intent(in) :: this

! The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(character(this%actual))
  write( unit, '(2x,a,1x,a)' ) 'expected:', trim(character(this%expected))

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_complex_write

end module m_xfunit_assertion_complex

