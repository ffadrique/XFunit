module m_xfunit_assertion_class

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests infinite polymorphic assertion
!>
!> License   : This file is part of XFunit.
!>
!>             XFunit is free software: you can redistribute it and/or modify
!>             it under the terms of the GNU Lesser General Public License as
!>             published by the Free Software Foundation, either version 3 of
!>             the License, or (at your option) any later version.
!>
!>             XFunit is distributed in the hope that it will be useful,
!>             but WITHOUT ANY WARRANTY; without even the implied warranty of
!>             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!>             See the GNU Lesser General Public License for more details.
!>
!>             You should have received a copy of the GNU Lesser General Public
!>             License along with Zofu.
!>             If not, see <http://www.gnu.org/licenses/>.
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

  public t_xfunit_assertion_class

  public xfunit_assertion_class, &
         xfunit_assertion_class_equal, &
         xfunit_assertion_class_serialize

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The infinite polymorphic assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_class
    private

!>     Actual asserted value
      class(*), allocatable :: actual

!>     Expected value
      class(*), allocatable :: expected

!>     String representation of actual value
      type(t_string) :: sactual

!>     String representation of expected value
      type(t_string) :: sexpected

    contains

!>     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_class_assign

!>     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_class_write_xml
      procedure :: write => xfunit_assertion_class_write

  end type t_xfunit_assertion_class

!> Comparison interface
!> Required to implement the equality logic for the unit test assertion
!> Cannot be pure to allow assignment of elements of complex types
  interface
    function xfunit_assertion_class_equal( actual, expected ) result(res)
      class(*), intent(in) :: actual
      class(*), intent(in) :: expected
      logical :: res
    end function xfunit_assertion_class_equal
  end interface

!> Serialization interface
!> Required to show the class contents in the unit test report
!> Cannot be pure to allow assignment of elements of complex types
  interface
    function xfunit_assertion_class_serialize( object ) result(res)
      class(*), intent(in) :: object
      character(len=:), allocatable :: res
    end function xfunit_assertion_class_serialize
  end interface

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for infinite polymorphic assertion
!> Cannot be pure to allow assignment of elements of complex types
function xfunit_assertion_class( name, actual, expected, equal, serialize ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion actual value
  class(*), intent(in) :: actual

!> The assertion expected value
  class(*), intent(in) :: expected

!> Comparison function
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The returned assertion
  type(t_xfunit_assertion_class) :: res

! Local variables
  integer :: status

! Compute result
  if( equal( actual, expected ) ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Store the information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_class_index, status )
  allocate( res%actual, source=actual )
  allocate( res%expected, source=expected )

! Store the string representation
  if( present(serialize) ) then
    res%sactual = string( serialize( actual ) )
    res%sexpected = string( serialize( expected ) )
  end if

end function xfunit_assertion_class


!> Assignment
elemental subroutine xfunit_assertion_class_assign( this, other )

!> The target assertion
  class(t_xfunit_assertion_class), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_class)

!     Assign base assertion
      this%t_xfunit_assertion = other%t_xfunit_assertion

!     Assign actual value
      if( allocated(this%actual) ) deallocate(this%actual)
      allocate( this%actual, source=other%actual )
      this%sactual = other%sactual

!     Assign expected value
      if( allocated(this%expected) ) deallocate(this%expected)
      allocate( this%expected, source=other%expected )
      this%sexpected = other%sexpected

  end select

end subroutine xfunit_assertion_class_assign


!> Serialize in XML
subroutine xfunit_assertion_class_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion_class), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  call xml%write_terminal( 'actual', this%sactual )
  call xml%write_terminal( 'expected', this%sexpected )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_class_write_xml


!> Serialize in plain text
subroutine xfunit_assertion_class_write( this, unit )

!> The assertion
  class(t_xfunit_assertion_class), intent(in) :: this

!> The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', this%sactual%character()
  write( unit, '(2x,a,1x,a)' ) 'expected:', this%sexpected%character()

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_class_write

end module m_xfunit_assertion_class

