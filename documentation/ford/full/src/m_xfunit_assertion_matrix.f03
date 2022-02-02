module m_xfunit_assertion_matrix

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests matrix assertion parent type
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

  public t_xfunit_assertion_matrix

  public xfunit_assertion_matrix

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> Type to allow polymorphism in the matrix of objects
  type t_matrix_item
    private

!>     The assertion parent class pointer
      class(t_xfunit_assertion), allocatable :: assertion

  end type t_matrix_item


!> The matrix character assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_matrix
    private

!>     Matrix of individual assertions
      type(t_matrix_item), dimension(:,:), allocatable :: matrix

    contains

!>     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_matrix_assign

!>     Count of indiidual assertions
      procedure :: count => xfunit_assertion_matrix_count

!>     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_matrix_write_xml
      procedure :: write => xfunit_assertion_matrix_write

  end type t_xfunit_assertion_matrix

!---End of declaration of module variables--------------------------------------

contains

!> Constructor for array assertion
pure function xfunit_assertion_matrix( name, type, rast, mold ) result(res)

!> The assertion name
  character(len=*), intent(in)  :: name

!> The assertion type
  integer, intent(in) :: type

!> The matrix of individual assertions
  class(t_xfunit_assertion), dimension(:,:), intent(in) :: rast

!> Mold to be used in the polymorphic assertion creation
!> This prevents the use of rast to allocate the final structure and then the deallocation of
!> internal allocatable structures when rast is deallocated
  class(t_xfunit_assertion), optional, intent(in) :: mold

!> The returned assertion
  type(t_xfunit_assertion_matrix) :: res

! Local variables
  integer :: i1, i2, n1, n2, status

! Set the assertion name
  call res%set_name( string(name) )

! Set the assertion type
  call res%set_type( type )

! Store the matrix information
  n1 = size(rast,1)
  n2 = size(rast,2)
  allocate( res%matrix(n1,n2) )
  do i1 = 1, n1
    do i2 = 1, n2

!     Allocate the assertion using the mold if provided
      if( present(mold) ) then
        allocate( res%matrix(i1,i2)%assertion, source=mold )
      else
        allocate( res%matrix(i1,i2)%assertion, source=rast(i1,i2) )
      end if

!     Copy assertion data
      res%matrix(i1,i2)%assertion = rast(i1,i2)

    end do
  end do

! Update overall assertion status
  if( all( rast%get_status() == xfunit_assertion_is_pass ) ) then
    status = xfunit_assertion_is_pass
  else
    status = xfunit_assertion_is_fail
  end if

! Set the assertion status
  call res%set_status( status )

end function xfunit_assertion_matrix


!> Assignment
elemental subroutine xfunit_assertion_matrix_assign( this, other )

!> The target assertion
  class(t_xfunit_assertion_matrix), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Local variables
  integer :: i1, i2, n1, n2

! Cast the other object
  select type(other)
    class is(t_xfunit_assertion_matrix)

!     Initialise base
      this%t_xfunit_assertion = other%t_xfunit_assertion

!     Allocate matrix of assertions
      n1 = size(other%matrix,1)
      n2 = size(other%matrix,2)
      allocate( this%matrix(n1,n2) )

!     Copy the individual assertions
      do i1 = 1, n1
        do i2 = 1, n2
          allocate( this%matrix(i1,i2)%assertion, source=other%matrix(i1,i2)%assertion )
!          this%matrix(i1,i2)%assertion = other%matrix(i1,i2)%assertion
        end do
      end do

  end select

end subroutine xfunit_assertion_matrix_assign


!> Number of individual assertions
elemental function xfunit_assertion_matrix_count( this ) result(res)

!> The assertion
  class(t_xfunit_assertion_matrix), intent(in) :: this

!> The count of individual assertions
  integer :: res

! Return the count of individual assertions
  res = size(this%matrix)

end function xfunit_assertion_matrix_count


!> Serialize in XML
subroutine xfunit_assertion_matrix_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion_matrix), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  integer :: i1, i2, n1, n2
  type(t_xml_attribute), dimension(1) :: attr

! Serializa start tag
  call this%write_xml_start_tag( xml )

! Add details
  n1 = size(this%matrix,1)
  n2 = size(this%matrix,2)
  do i1 = 1, n1
    do i2 = 1, n2

!     Add intermediate container start tag
      attr(1) = xml_attribute( 'name', trim(character(i1)) // "," // trim(character(i2)) )
      call xml%write_start_tag( 'element', attr=attr, newline=.true. )

!     Add assertion details
      call this%matrix(i1,i2)%assertion%write_xml( xml )

!     Add intermediate container end tag
      call xml%write_end_tag( 'element' )

    end do
  end do

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_matrix_write_xml


!> Serialize in plain text
subroutine xfunit_assertion_matrix_write( this, unit )

!> The assertion
  class(t_xfunit_assertion_matrix), intent(in) :: this

!> The XML context structure
  integer, intent(in) :: unit

! Local variables
  integer :: i1, i2, n1, n2

! Add details
  n1 = size(this%matrix,1)
  n2 = size(this%matrix,2)
  do i1 = 1, n1
    do i2 = 1, n2

!     Add the matrix element identifier
      write( unit, '(2(i4.4,1x))', advance='no' ) i1, i2

!     Add assertion details
      call this%matrix(i1,i2)%assertion%write( unit )

    end do
  end do

end subroutine xfunit_assertion_matrix_write

end module m_xfunit_assertion_matrix

