module m_xfunit_suite

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests suite top level implementation and interface
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

  use, intrinsic :: iso_fortran_env

  use m_xfunit_unit_list_ftl

  use m_object
  use m_string
  use m_iso8601_date_time
  use m_path

  use m_file_handler
  use m_xml
  use m_xfunit_unit
  use m_xfunit_summary

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_suite

  public xfunit_suite

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The test suite type
  type, extends(t_object) :: t_xfunit_suite
    private

!     Package name
      type(t_string) :: package

!     Source file unit tested
      type(t_string) :: source

!     Test suite test name
      type(t_string) :: name

!     Test suite annotation
      type(t_string) :: annotation

!     List of unit tests in the test suite
      type(t_xfunit_unit_list_ftl) :: ut

!     Pointer to the subroutine to be executer prior to the unit test execution
      procedure (xfunit_suite_executer), pointer :: before => null()

!     Pointer to the subroutine to be executer after the unit test execution
      procedure (xfunit_suite_executer), pointer :: after => null()

!     Test suite execution status
      integer :: status = 0

    contains

!     Write in XML
      generic :: write_xml => xfunit_suite_write_xml
      procedure, private :: xfunit_suite_write_xml
      procedure, private :: xfunit_suite_summary
      procedure, private :: xfunit_suite_write_properties_xml

!     Add a unit test to the suite
      procedure :: add_unit_test => xfunit_suite_add_unit_test

!     Execute the unit tests in the suite; with filter
      generic :: execute => xfunit_suite_execute_string, &
                            xfunit_suite_execute_character
      procedure, private :: xfunit_suite_execute_string
      procedure, private :: xfunit_suite_execute_character

!     Error handling
      procedure :: error => xfunit_suite_error
      procedure :: is_error => xfunit_suite_is_error

!     Getters (no setters required)
      procedure :: get_package => xfunit_suite_get_package
      procedure :: get_source => xfunit_suite_get_source
      procedure :: get_name => xfunit_suite_get_name
      procedure :: get_annotation => xfunit_suite_get_annotation

!     Assignment operator
      generic :: assignment(=) => xfunit_suite_assign_xfunit_suite
      procedure, private :: xfunit_suite_assign_xfunit_suite

  end type t_xfunit_suite

! Constructor interface
  interface xfunit_suite
    module procedure xfunit_suite_character
    module procedure xfunit_suite_string
  end interface xfunit_suite

! Interface for the executer subroutine
  interface
    subroutine xfunit_suite_executer( suite )
      import t_xfunit_suite
      class(t_xfunit_suite), intent(inout) :: suite
    end subroutine xfunit_suite_executer
  end interface

!---End of declaration of module variables--------------------------------------

contains

! Constructor from character
function xfunit_suite_character( package, source, annotation, before, after ) result(res)

! The name of the package that the source belongs to
  character(len=*), intent(in)  :: package

! The name of the source on which the test is performed
  character(len=*), intent(in)  :: source

! The test suite annotation
  character(len=*), optional, intent(in)  :: annotation

! The procedure to execute before the test suite
  procedure(xfunit_suite_executer), optional :: before

! The procedure to execute after the test suite
  procedure(xfunit_suite_executer), optional :: after

! The test suite
  type(t_xfunit_suite) :: res

! Initialise the package
  res%package = package

! Set the source and name of the test suite
  res%source = source
  res%name = path_name( source )

! Set the annotation of the test suite
  if( present(annotation) ) then
    res%annotation = annotation
  end if

! Initialise list of unit tests
  call res%ut%clear()

! Initialise the procedures to prepare the test suite execution
  if( present(before) ) then
    res%before => before
  end if

! Initialise the procedures to clean-up test suite execution
  if( present(after) ) then
    res%after => after
  end if

end function xfunit_suite_character


! Constructor from string
function xfunit_suite_string( package, source, annotation, before, after ) result(res)

! The name of the package that the source belongs to
  type(t_string), intent(in)  :: package

! The name of the source on which the test is performed
  type(t_string), intent(in)  :: source

! The test suite annotation
  type(t_string), optional, intent(in)  :: annotation

! The procedure to execute before the test suite
  procedure(xfunit_suite_executer), optional :: before

! The procedure to execute after the test suite
  procedure(xfunit_suite_executer), optional :: after

! The test suite
  type(t_xfunit_suite) :: res

! Invoke the character interface
  res = xfunit_suite( package%character(), source%character(), annotation%character(), &
                      before, after )

end function xfunit_suite_string


! Generate the suite block
subroutine xfunit_suite_write_xml( suite, xml, junit_strict, fail_only, stdout, stderr )

! The test suite
  class(t_xfunit_suite), intent(in) :: suite

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! The flag for strict JUnit generation report
  logical, intent(in) :: junit_strict

! The flag to output failed assertions only
  logical, intent(in) :: fail_only

! Captured stdout file path
  type(t_file_handler), intent(in) :: stdout

! Captured stderr file path
  type(t_file_handler), intent(in) :: stderr

! Local variables
  type(t_xfunit_unit_list_ftl_iterator) :: it
  type(t_xml_attribute), dimension(7) :: attr
  type(t_xfunit_unit), pointer :: ut
  type(t_xfunit_summary) :: summary
  type(t_iso8601_date_time) :: timestamp

! Compute the suite summary
  call suite%xfunit_suite_summary( summary )

! Generate the time stamp
  timestamp = iso8601_date_time_now()

! Write the suite start tag
  attr(1) = xml_attribute( 'name', suite%name )
  attr(2) = xml_attribute( 'timestamp', timestamp%to_string( zone=.false. ) )
  attr(3) = xml_attribute( 'hostname', "localhost" )
  attr(4) = xml_attribute( 'tests', summary%get_tests() )
  attr(5) = xml_attribute( 'failures', summary%get_failed() )
  attr(6) = xml_attribute( 'errors', summary%get_errors() )
  attr(7) = xml_attribute( 'time', real(summary%get_time(),4) )
  call xml%write_root_start_tag( 'testsuite', schema='JUnit.xsd', attr=attr )

! Write the porperties section
  call suite%xfunit_suite_write_properties_xml( xml )

! Loop on the unit tests
  it = suite%ut%begin()
  do while( it%associated() )

!   Write the unit test
    ut => it%get_element_ptr()
    call ut%write_xml( xml, junit_strict, fail_only )

!   Iterate
    it = it%next()

  end do

! Generate the output report
  call xfunit_suite_generate_text_report( xml, string('system-out'), stdout%get_file_path() )

! Generate the error report
  call xfunit_suite_generate_text_report( xml, string('system-err'), stderr%get_file_path() )

! Write the suite end tag
  call xml%write_root_end_tag( 'testsuite' )

end subroutine xfunit_suite_write_xml


! Generate the suite properties block
subroutine xfunit_suite_write_properties_xml( suite, xml )

! The test suite
  class(t_xfunit_suite), intent(in) :: suite

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Write the properties start tag
  call xml%write_start_tag( 'properties', newline=.true. )

! Add the package
  attr(1) = xml_attribute( 'name', 'package' )
  attr(2) = xml_attribute( 'value', suite%package )
  call xml%write_terminal( 'property', '', attr=attr )

! Add the source file
  attr(1) = xml_attribute( 'name', 'src' )
  attr(2) = xml_attribute( 'value', suite%source )
  call xml%write_terminal( 'property', '', attr=attr )

! Add the module name
  attr(1) = xml_attribute( 'name', 'module' )
  attr(2) = xml_attribute( 'value', suite%name )
  call xml%write_terminal( 'property', '', attr=attr )

! Add the suite description
  attr(1) = xml_attribute( 'name', 'annotation' )
  attr(2) = xml_attribute( 'value', suite%annotation )
  call xml%write_terminal( 'property', '', attr=attr )

! Write the properties end tag
  call xml%write_end_tag( 'properties' )

end subroutine xfunit_suite_write_properties_xml


! Generate text report
subroutine xfunit_suite_generate_text_report( xml, tag, path )

! XML writer object
  type(t_xml_writer), intent(inout) :: xml

! The tag for the text report container
  type(t_string), intent(in) :: tag

! Path to the file containing the report
  type(t_string), intent(in) :: path

! Local variables
  type(t_file_handler) :: fhandler
  character(len=1024) :: rec
  integer :: iunit, ounit, iostat

! Write the text report start tag
  call xml%write_start_tag( tag, newline=.true. )

! Open the report file
  fhandler = file_handler( path )
  call fhandler%open( write=.false. )
  if( fhandler%is_open() ) then

!   Transfer the report
    iunit = fhandler%get_unit()
    ounit = xml%get_unit()
    iostat = 0
    do
      read( iunit, '(a1024)', iostat=iostat ) rec
      if( iostat == 0 .or. iostat == iostat_eor ) then
        write( ounit, '(a)' ) trim(rec)
      else
        exit
      end if
    end do

!   Close the file
    call fhandler%close()

  end if

! Write the text report end tag
  call xml%write_end_tag( tag )

end subroutine xfunit_suite_generate_text_report


! Insert a new unit test in the suite
subroutine xfunit_suite_add_unit_test( suite, ut, skip )

! The unit test suite
  class(t_xfunit_suite), intent(inout) :: suite

! The assertion
  type(t_xfunit_unit), intent(in)    :: ut

! Flag to add the test but to skip the execution (optional; .false. by default)
  logical, optional, intent(in) :: skip

! Local variables
  type(t_xfunit_unit_list_ftl_iterator) :: it
  type(t_xfunit_unit), pointer :: put

! Add the unit test to the suite
  call suite%ut%push_back( ut )

! Check skip flag
  if( present(skip) ) then
    it = suite%ut%end()
    put => it%get_element_ptr()
    call put%set_skip( skip )
  end if

end subroutine xfunit_suite_add_unit_test


! Execute the unit tests in the suite (string interface)
subroutine xfunit_suite_execute_character( suite, names )

! The unit test suite
  class(t_xfunit_suite), intent(inout) :: suite

! The names of the tests to execute (optional)
  character(len=*), dimension(:), intent(in) :: names

! Local variables
  type(t_string), dimension(size(names)) :: lnames
  integer :: i

! Call the character interface
  do i = 1, size(names)
    lnames(i) = string(names(i))
  end do
  call suite%xfunit_suite_execute_string( lnames )

end subroutine xfunit_suite_execute_character


! Execute the unit tests in the suite (character interface)
subroutine xfunit_suite_execute_string( suite, names )

! The unit test suite
  class(t_xfunit_suite), intent(inout) :: suite

! The names of the tests to execute (optional)
  type(t_string), optional, dimension(:), intent(in) :: names

! Local variables
  type(t_xfunit_unit_list_ftl_iterator) :: it
  type(t_xfunit_unit), pointer :: put
  logical :: do_execute
  type(t_string) :: utname
  integer :: n

! Execute the preparation routine
  if( associated(suite%before) ) then
    call suite%before()
  end if

! Check initialisation status
  if( suite%status == 0 ) then

!   Loop on the unit tests
    it = suite%ut%begin()
    do while( it%associated() )

!     Point to test case
      put => it%get_element_ptr()

!     Check if this test is to be skipped
      do_execute = .false.
      if( .not. put%get_skip() ) then

!       Check if test execution by name is requested from the command line
        if( present(names) ) then
          n = size(names)
          if( size(names) > 0 ) then
            utname = put%get_name()
            do_execute = any( match( utname, names ) )
          else
            do_execute = .true.
          end if
        else
          do_execute = .true.
        end if

      end if

!     Execute the unit test
      if( do_execute ) then
        call put%execute()
      end if

!     Iterate
      it = it%next()

    end do

!   Execute the teardown routine
    if( associated(suite%after) ) then
      call suite%after()
    end if

  end if

end subroutine xfunit_suite_execute_string


! Set the suite execution on error (status=0 to reset)
elemental subroutine xfunit_suite_error( suite, status )

! The unit test suite
  class(t_xfunit_suite), intent(inout) :: suite

! The provided error status
  integer, intent(in) :: status

! Set the status
  suite%status = status

end subroutine xfunit_suite_error


! Check the suite error status
elemental function xfunit_suite_is_error( suite ) result(res)

! The unit test suite
  class(t_xfunit_suite), intent(in) :: suite

! The on error status
  logical :: res

! Return the on error status
  res = ( suite%status /= 0 )

end function xfunit_suite_is_error


! Generate the unit test summary
subroutine xfunit_suite_summary( suite, summary )

! The suite test
  class(t_xfunit_suite), intent(in) :: suite

! The summary
  type(t_xfunit_summary), intent(out) :: summary

! Local variables
  type(t_xfunit_unit_list_ftl_iterator) :: it
  integer :: npassed, nexecuted, nerrors, nfailed
  real(kind=8) :: time
  type(t_xfunit_unit), pointer :: put

! Initialised
  npassed = 0
  nexecuted = 0
  nerrors = 0
  time = 0.0_8

! Loop on the unit tests
  it = suite%ut%begin()
  do while( it%associated() )

!   Check if test is executed
    put => it%get_element_ptr()
    if( put%is_executed() ) then
      nexecuted = nexecuted + 1

!     Check if test is passed
      if( put%is_passed() ) then
        npassed = npassed + 1
      else if( put%is_error() ) then
        nerrors = nerrors + 1
      end if

!     Accumulate execution time
      time = time + put%get_elapsed()

    end if

 !  Iterate
    it = it%next()

  end do

! Generate the summary
  nfailed = nexecuted - npassed - nerrors
  summary = xfunit_summary( suite%ut%size(), nexecuted, npassed, nfailed, nerrors, time )

end subroutine xfunit_suite_summary


! Assignment operator
subroutine xfunit_suite_assign_xfunit_suite( left, right )

! Left operand
  class(t_xfunit_suite), intent(out) :: left
  
! Right operand
  type(t_xfunit_suite), intent(in)  :: right

! Assign the object properties
  left%package = right%package
  left%source = right%source
  left%name = right%name
  left%annotation = right%annotation
  left%ut = right%ut
  left%before => right%before
  left%after => right%after
  left%status = right%Status

end subroutine xfunit_suite_assign_xfunit_suite


! Getter for package name
elemental function xfunit_suite_get_package( this ) result(res)

! Calling object
  class(t_xfunit_suite), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%package

end function xfunit_suite_get_package


! Getter for Fortran source file
elemental function xfunit_suite_get_source( this ) result(res)

! Calling object
  class(t_xfunit_suite), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%source

end function xfunit_suite_get_source


! Getter for unit test suite name
elemental function xfunit_suite_get_name( this ) result(res)

! Calling object
  class(t_xfunit_suite), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%name

end function xfunit_suite_get_name


! Getter for unit test suite annotation
pure function xfunit_suite_get_annotation( this ) result(res)

! Calling object
  class(t_xfunit_suite), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%annotation

end function xfunit_suite_get_annotation

end module m_xfunit_suite