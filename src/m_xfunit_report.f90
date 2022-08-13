module m_xfunit_report

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests reports (detailed report added to JUnit compliant report)
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

  use m_xfunit_assertion_list_ftl

  use m_xml

  use m_xfunit_assertion

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_report

  public xfunit_report

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The unit test case detailed report type
  type, extends(t_xfunit_assertion_list_ftl) :: t_xfunit_report
    private

    contains

!     Serialization interfaces
      procedure :: write_xml => xfunit_report_write_xml
      procedure :: write => xfunit_report_write

  end type t_xfunit_report

! Constructor interface
  interface xfunit_report
    module procedure xfunit_report_default
  end interface xfunit_report

!---End of declaration of module variables--------------------------------------

contains

! Constructor
function  xfunit_report_default() result(res)

! The unit test case report
  type(t_xfunit_report) :: res


end function xfunit_report_default


! Generate the report dump
subroutine xfunit_report_write( this, unit, fail_only )

! The unit test report
  class(t_xfunit_report), intent(in) :: this

! The open fortran unit to write to
  integer, intent(in) :: unit

! The flag to output failed assertions only
  logical, intent(in) :: fail_only

! Local variables
  type(t_xfunit_assertion_list_ftl_iterator) :: it
  class(t_xfunit_assertion), allocatable :: assertion

! Loop on the assertions
  it = this%begin()
  do while( it%associated() )

!   Check allocations. This is a gfortran limitation
    if( allocated(assertion) ) deallocate(assertion)

!   Get the assertion
    allocate( assertion, source=it%get_element() )

!   Check if the assertion is to be reported
    if( .not. fail_only .or. ( fail_only .and. .not. assertion%is_passed() ) ) then

!     Write the assertion record
      call assertion%write( unit )

    end if

!   Iterate
    it = it%next()

  end do

end subroutine xfunit_report_write


! Generate the report in XML format
subroutine xfunit_report_write_xml( this, xml, fail_only )

! The unit test report
  class(t_xfunit_report), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! The flag to output failed assertions only
  logical, intent(in) :: fail_only

! Local variables
  type(t_xfunit_assertion_list_ftl_iterator) :: it
  class(t_xfunit_assertion), pointer :: assertion

! Loop on the assertions
  it = this%begin()
  do while( it%associated() )

!   Get the assertion
    assertion => it%get_element_ptr()

!   Check if the assertion is to be reported
    if( .not. fail_only .or. ( fail_only .and. .not. assertion%is_passed() ) ) then

!     Write the assertion record
      call assertion%write_xml( xml )

    end if

!   Iterate
    it = it%next()

  end do

end subroutine xfunit_report_write_xml

end module m_xfunit_report

! 2022-07-30T18:39:34