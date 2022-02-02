module m_xfunit_assertion_array

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Uunit tests array assertion parent type
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

  public t_xfunit_assertion_array

  public xfunit_assertion_array

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> Type to allow polymorphism in the array of objects
  type t_array_item
    private

!>     The assertion parent class pointer
      class(t_xfunit_assertion), allocatable :: assertion

  end type t_array_item


!> Uunit tests array assertion parent type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_array
    private

!>     Array of individual assertions
      type(t_array_item), dimension(:), allocatable :: array

    contains

!>     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_array_assign

!>     Count of indiidual assertions
      procedure :: count => xfunit_assertion_array_count

!>     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_array_write_xml
      procedure :: write => xfunit_assertion_array_write

  end type t_xfunit_assertion_array

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for array assertion
pure function xfunit_assertion_array( name, type, rast, mold ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion type
  integer, intent(in) :: type

!> The array of individual assertions
  class(t_xfunit_assertion), dimension(:), intent(in) :: rast

!> Mold to be used in the polymorphic assertion instantiation
!> This prevents the use of this%rast to allocate the final structure and then the
!> deallocation of internal allocatable structures when this%rast is deallocated
  class(t_xfunit_assertion), optional, intent(in) :: mold

!> The returned assertion
  type(t_xfunit_assertion_array) :: res

! Local variables
  integer :: i, n, status

! Set the assertion name
  call res%set_name( string(name) )

! Set the assertion type
  call res%set_type( type )

! Store the array information
  n = size(rast)
  allocate( res%array(n) )
  do i = 1, n

!   Allocate the assertion using the mold if provided
    if( present(mold) ) then
      allocate( res%array(i)%assertion, source=mold )
    else
      allocate( res%array(i)%assertion, source=rast(i) )
    end if

!   Copy assertion data
    res%array(i)%assertion = rast(i)

  end do

! Update overall assertion status
  if( all( rast%get_status() == xfunit_assertion_is_pass ) ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Set the assertion status
  call res%set_status( status )

end function xfunit_assertion_array


!> Assignment
elemental subroutine xfunit_assertion_array_assign( this, other )

!> The target assertion
  class(t_xfunit_assertion_array), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Local variables
  integer :: i, n

! Cast the other object
  select type(other)
    class is(t_xfunit_assertion_array)

!     Initialise base
      this%t_xfunit_assertion = other%t_xfunit_assertion

!     Allocate array of assertions
      n = size(other%array)
      if( allocated(this%array) ) deallocate(this%array)
      allocate( this%array(n) )

!     Copy the individual assertions
      do i = 1, n
        allocate( this%array(i)%assertion, source=other%array(i)%assertion )
!        this%array(i)%assertion = other%array(i)%assertion
      end do

  end select

end subroutine xfunit_assertion_array_assign


!> Number of individual assertions
elemental function xfunit_assertion_array_count( this ) result(res)

!> The assertion
  class(t_xfunit_assertion_array), intent(in) :: this

!> The count of individual assertions
  integer :: res

! Return the count of individual assertions
  res = size(this%array)

end function xfunit_assertion_array_count


!> Serialize in XML
subroutine xfunit_assertion_array_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion_array), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  integer :: i, n
  type(t_xml_attribute), dimension(1) :: attr

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  n = size(this%array)
  do i = 1, n

!   Add intermediate container start tag
    attr(1) = xml_attribute( 'name', character(i) )
    call xml%write_start_tag( 'element', attr=attr, newline=.true. )

!   Add assertion details
    call this%array(i)%assertion%write_xml( xml )

!   Add intermediate container end tag
    call xml%write_end_tag( 'element' )

  end do

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_array_write_xml


!> Serialize in plain text
subroutine xfunit_assertion_array_write( this, unit )

!> The assertion
  class(t_xfunit_assertion_array), intent(in) :: this

!> The XML context structure
  integer, intent(in) :: unit

! Local variables
  integer :: i, n

! Add details
  n = size(this%array)
  do i = 1, n

!   Add the array element identifier
    write( unit, '(i4.4,1x)', advance='no' ) i

!   Add assertion details
    call this%array(i)%assertion%write( unit )

  end do

end subroutine xfunit_assertion_array_write

end module m_xfunit_assertion_array


