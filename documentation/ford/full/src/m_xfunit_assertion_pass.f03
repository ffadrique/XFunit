module m_xfunit_assertion_pass

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests pass assertion
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_util_convert

  use m_xfunit_assertion
  use m_xml

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_pass

  public xfunit_assertion_pass

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The pass assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_pass
    private

    contains

!>     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_pass_assign

!>     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_pass_write_xml
      procedure :: write => xfunit_assertion_pass_write

  end type t_xfunit_assertion_pass

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for pass assertion
elemental function xfunit_assertion_pass( name ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The returned assertion
  type(t_xfunit_assertion_pass) :: res

! Local variables
  integer :: status

! Compute result
  status = xfunit_assertion_is_pass

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_pass_index, status )

end function xfunit_assertion_pass


!> Assignment
elemental subroutine xfunit_assertion_pass_assign( this, other )

!> The target assertion (ifort accepts out; gfortran forces inout)
  class(t_xfunit_assertion_pass), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_pass)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion

end select

end subroutine xfunit_assertion_pass_assign


!> Serialize in XML
subroutine xfunit_assertion_pass_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion_pass), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_pass_write_xml


!> Serialize in plain text
subroutine xfunit_assertion_pass_write( this, unit )

!> The assertion
  class(t_xfunit_assertion_pass), intent(in) :: this

!> The XML context structure
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_pass_write

end module m_xfunit_assertion_pass

