module m_xfunit_assertion_integer_greater

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests integer range (greater than) assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xml

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_integer_greater

  public xfunit_assertion_integer_greater

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The integer ragne (greater than) assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_integer_greater
    private

!>     Actual asserted value
      integer(kind=8) :: actual = 0

!>     Lower value
      integer(kind=8) :: low = 0

    contains

!>     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_integer_greater_assign

!>     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_integer_greater_write_xml
      procedure :: write => xfunit_assertion_integer_greater_write

  end type t_xfunit_assertion_integer_greater

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for real range (greater than) assertion
elemental function xfunit_assertion_integer_greater( name, actual, low, open_low ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  integer(kind=8), intent(in) :: actual

!> The assertion low bound value
  integer(kind=8), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The returned assertion
  type(t_xfunit_assertion_integer_greater) :: res

! Local variables
  logical :: ok_low
  logical :: is_open_low
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

! Compute result
  if( ok_low ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_integer_greater_index, status )
  res%actual   = actual
  res%low      = low

end function xfunit_assertion_integer_greater


!> Assignment
elemental subroutine xfunit_assertion_integer_greater_assign( this, other )

!> The target assertion
  class(t_xfunit_assertion_integer_greater), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_integer_greater)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%low = other%low

  end select

end subroutine xfunit_assertion_integer_greater_assign


!> Serialize in XML
subroutine xfunit_assertion_integer_greater_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion_integer_greater), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%actual )
  call xml%write_terminal( 'low', this%low )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_integer_greater_write_xml


!> Serialize in plain text
subroutine xfunit_assertion_integer_greater_write( this, unit )

!> The assertion
  class(t_xfunit_assertion_integer_greater), intent(in) :: this

!> The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(character(this%actual))
  write( unit, '(2x,a,1x,a)' ) 'low:     ', trim(character(this%low))

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_integer_greater_write

end module m_xfunit_assertion_integer_greater
