module m_xfunit_manager

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Management of the unit test execution
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

  use, intrinsic :: iso_fortran_env

  use m_object
  use m_string
  use m_util_convert
  use m_messages
  use m_file_handler
  use m_path

  use m_xml

  use m_xfunit_unit
  use m_xfunit_suite

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_manager

  public xfunit_manager, xfunit_manager_jxml, xfunit_manager_eclipse

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The unit test manager type
  type, extends(t_object) :: t_xfunit_manager
    private

!     File handler for report generation
      type(t_file_handler) :: handler

!     Root directory where output is organised
      type(t_string) :: xfunit_root_dir

!     Directory where unit test data is stored
      type(t_string) :: unit_data_dir

!     Directory where reference files (for comparison) are stored
      type(t_string) :: unit_ref_dir

!     Directory where the unit test reports (.jxml files) are stored
      type(t_string) :: unit_jxml_dir

!     Junit XML file
      type(t_string) :: unit_jxml_file

!     Filter to select the unit tests to run
      type(t_string), dimension(:), allocatable :: unit_test_filter

!     Flag to generate output reports (.jxml) strictly compliant with Junit.xsd
      logical :: junit_strict = .false.

!     Report on failures only (not implemented yet)
      logical :: fail_only = .false.

!     Error message stack
      type(t_messages) :: msg

!     Temporary file for stdout capture
      type(t_file_handler) :: stdout

!     Temporary file for stderr capture
      type(t_file_handler) :: stderr

    contains

!     Getters
      procedure :: get_unit_root_dir => xfunit_manager_unit_root_dir_get
      procedure :: get_unit_data_dir => xfunit_manager_unit_data_dir_get
      procedure :: get_unit_ref_dir  => xfunit_manager_unit_ref_dir_get
      procedure :: get_unit_jxml_dir => xfunit_manager_unit_jxml_dir_get
      procedure :: get_unit_jxml_file => xfunit_manager_unit_jxml_file_get
      procedure :: get_unit_test_filter => xfunit_manager_unit_test_filter_get

!     Process the command line
      procedure, private :: xfunit_manager_process_command_line

!     Assignment
      generic :: assignment(=) => xfunit_manager_assignment
      procedure, private :: xfunit_manager_assignment

!     Execute the suiteof test
      procedure :: execute => xfunit_manager_execute_suite

!     Serialization in XML
      procedure :: write_xml => xfunit_manager_write_xml_suite

!     Error handling
      procedure :: is_error => xfunit_manager_is_error
      procedure :: dump_error => xfunit_manager_dump_error

  end type t_xfunit_manager

!---End of declaration of module variables--------------------------------------

contains

! Constructor (generic)
function xfunit_manager( fname, &
                         xfunit_root_dir, &
                         unit_data_dir, unit_ref_dir, unit_jxml_dir, &
                         junit_strict, fail_only, free_out, free_err ) result(res)

! The output file name (default to module.jxml or module.junit.xml)
  character(len=*), intent(in) :: fname

! The path to the root directory where the tests are implemented (default to .)
  character(len=*), optional, intent(in) :: xfunit_root_dir

! The path to the unit data (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_data_dir

! The path to the unit test reference (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_ref_dir

! The path to the unit test jxml (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_jxml_dir

! The flag for strict JUnit generation report
  logical, optional, intent(in) :: junit_strict

! The flag for report only the failures
  logical, optional, intent(in) :: fail_only

! The flag to not capture the standard output for the jxml report
  logical, optional, intent(in) :: free_out

! The flag to not capture the standard error for the jxml report
  logical, optional, intent(in) :: free_err

! The test manager
  type(t_xfunit_manager) :: res

! Set the reference directory (force unix representation)
  if( present(xfunit_root_dir) ) then
    res%xfunit_root_dir = string( trim(xfunit_root_dir) )
  else
    res%xfunit_root_dir = string( '.' )
  end if

! Set unit data directory
  if( present(unit_data_dir) ) then
    res%unit_data_dir = unit_data_dir
  else
    res%unit_data_dir = res%xfunit_root_dir
  end if

! Set the unit test reference directory
  if( present(unit_ref_dir) ) then
    res%unit_ref_dir = unit_ref_dir
  else
    res%unit_ref_dir = res%xfunit_root_dir
  end if

! Set the unit test reference directory
  if( present(unit_jxml_dir) ) then
    res%unit_jxml_dir = unit_jxml_dir
  else
    res%unit_jxml_dir = res%xfunit_root_dir
  end if

! Store the flag for JUnit strict report
  if( present(junit_strict) ) then
    res%junit_strict = junit_strict
  else
    res%junit_strict = .false.
  end if

! Store the flag for reporting failures only
  if( present(fail_only) ) then
    res%fail_only = fail_only
  else
    res%fail_only = .false.
  end if

! Process the command line to allow using it for test execution filtering
  call res%xfunit_manager_process_command_line()

! Initialise handler
  res%unit_jxml_file = trim(fname)
  res%handler = file_handler( res%unit_jxml_file )

! Initialise stdout capture handler
  if( present(free_out) ) then
    if( .not. free_out ) then
      res%stdout = file_handler( path_temp_file_name() )
    end if
  else
    res%stdout = file_handler( path_temp_file_name() )
  end if

! Initialise stderr capture handler
  if( present(free_err) ) then
    if( .not. free_err ) then
      res%stderr = file_handler( path_temp_file_name() )
    end if
  else
    res%stderr = file_handler( path_temp_file_name() )
  end if

end function xfunit_manager


! Constructor (tests separated from code)
! Structure:
!   datadir = xfunit_root_dir / data / <package>
!   refdir  = xfunit_root_dir / <package> / reference
!   jxmldir = xfunit_root_dir / <package>
! This is consistent with the original structure (tests separated from code)
!   xfunit_root_dir := <workspace> / unit_testing
function xfunit_manager_jxml( module, package, xfunit_root_dir, junit_strict, fail_only, free_out, free_err ) result(res)

! The name of the module being tested
  character(len=*), intent(in) :: module

! The package to which the module belongs to (default to empty)
  character(len=*), intent(in) :: package

! The path to the root directory where the tests are implemented (default to .)
  character(len=*), optional, intent(in) :: xfunit_root_dir

! The flag for strict JUnit generation report
  logical, optional, intent(in) :: junit_strict

! The flag for reports failures only
  logical, optional, intent(in) :: fail_only

! The flag to not capture the standard output for the jxml report
  logical, optional, intent(in) :: free_out

! The flag to not capture the standard error for the jxml report
  logical, optional, intent(in) :: free_err

! The test manager
  type(t_xfunit_manager) :: res

! Local variables
  character(len=:), allocatable :: root_dir, data_dir, ref_dir, jxml_dir, jxml_file

! Set the reference directory
  if( present(xfunit_root_dir) ) then
    root_dir = string( trim(xfunit_root_dir) )
  else
    root_dir = string( '.' )
  end if

! Set unit data directory
  data_dir = trim(root_dir) // '/' // 'data' // '/' // trim(package)

! Set the unit test reference directory
  ref_dir = trim(root_dir) // '/' // trim(package) // '/' // 'reference'

! Set the unit test JXML directory
  jxml_dir = trim(root_dir) // '/' // trim(package)

! Generate the JXML file name
  jxml_file = trim(jxml_dir) // '/' // trim(module) // '.jxml'

! Use the general constructor
  res = xfunit_manager( jxml_file, root_dir, data_dir, ref_dir, jxml_dir, junit_strict, fail_only, free_out, free_err )

end function xfunit_manager_jxml


! Constructor (unit testing contained in source code folder tree, similar to Eclipse projects)
! Structure:
!   datadir = xfunit_root_dir / data
!   refdir  = xfunit_root_dir / reference
!   jxmldir = xfunit_root_dir
! This is consistent with the integration in eclipse:
!   xfunit_root_dir := workspace / src / <package> / utest
function xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict, fail_only, free_out, free_err ) result(res)

! The name of the module being tested
  character(len=*), intent(in) :: module

! The path to the root directory where the tests are implemented (default to .)
  character(len=*), optional, intent(in) :: xfunit_root_dir

! The flag for strict JUnit generation report
  logical, optional, intent(in) :: junit_strict

! The flag for reporting failures only
  logical, optional, intent(in) :: fail_only

! The flag to not capture the standard output for the jxml report
  logical, optional, intent(in) :: free_out

! The flag to not capture the standard error for the jxml report
  logical, optional, intent(in) :: free_err

! The test manager
  type(t_xfunit_manager) :: res

! Local variables
  character(len=:), allocatable :: root_dir, data_dir, ref_dir, jxml_dir, jxml_file

! Set the reference directory
  if( present(xfunit_root_dir) ) then
    root_dir = string( trim(xfunit_root_dir) )
  else
    root_dir = string( '.' )
  end if

! Set unit data directory
  data_dir = trim(root_dir) // '/' // 'data'

! Set the unit test reference directory
  ref_dir = trim(root_dir) // '/' // 'reference'

! Set the unit test JXML directory
  jxml_dir = trim(root_dir)

! Generate the JXML file name
  jxml_file = trim(jxml_dir) // '/' // trim(module) // '.jxml'

! Process the command line to allow using it for test execution filtering
  call res%xfunit_manager_process_command_line()

! Use the general constructor
  res = xfunit_manager( jxml_file, root_dir, data_dir, ref_dir, jxml_dir, junit_strict, fail_only, free_out, free_err )

end function xfunit_manager_eclipse


! Process the command line to get test names to use as filter for the execution
subroutine xfunit_manager_process_command_line( this )

! The unit test manager
  class(t_xfunit_manager), intent(inout) :: this

! Local variables
  integer :: argc, iarg, larg
  character(len=256) :: argv

! Get the number of arguments in the command line
  argc = command_argument_count()
  if( argc > 0 ) then

!   Compute the maximum argument length
    larg = 0
    do iarg = 1, argc
      call get_command_argument( iarg, argv )
      larg = max( larg, len_trim(argv) )
    end do

!   Allocate the argument list
    allocate( this%unit_test_filter(argc) )

!   Loop reading command line arguments
    do iarg = 1, argc
      call get_command_argument( iarg, argv )
      this%unit_test_filter(iarg) = string(trim(adjustl(argv)))
    end do

  end if

end subroutine xfunit_manager_process_command_line


! Assignment
subroutine xfunit_manager_assignment( this, other )

! Calling object
  class(t_xfunit_manager), intent(out) :: this

! THe other object
  class(t_xfunit_manager), intent(in) :: other

! Assign elements
  this%handler = other%handler
  this%xfunit_root_dir = other%xfunit_root_dir
  this%unit_data_dir = other%unit_data_dir
  this%unit_ref_dir = other%unit_ref_dir
  this%unit_jxml_dir = other%unit_jxml_dir
  this%unit_jxml_file = other%unit_jxml_file
  if( allocated(other%unit_test_filter) ) then
    allocate( this%unit_test_filter, source=other%unit_test_filter )
  end if
  this%junit_strict = other%junit_strict
  this%fail_only = other%fail_only
  this%msg = other%msg
  this%stdout = other%stdout
  this%stderr = other%stderr

end subroutine xfunit_manager_assignment


! Write suite in XML
subroutine xfunit_manager_execute_suite( this, suite )

! The test manager
  class(t_xfunit_manager), intent(inout) :: this

! The test suite
  type(t_xfunit_suite), intent(inout) :: suite

! Capture standard output (try to open)
  call this%stdout%open( write=.true., unit=output_unit )

! Capture standard error (try to open)
  call this%stderr%open( write=.true., unit=error_unit )

! Execute tests
  call suite%execute( this%get_unit_test_filter() )
  if( suite%is_error() ) then

!     Report the error
      call this%msg%error( 'm_xfunit_manager', &
                           'xfunit_manager_execute_suite', 1, &
                           'Failed during execution of suite ' // trim(character(suite%get_name())) // "'" )

  end if

! Close the captured standard output
  if( this%stdout%is_open() ) then
    call this%stdout%close()
  end if

! Close the captured standard error
  if( this%stderr%is_open() ) then
    call this%stderr%close( )
  end if

end subroutine xfunit_manager_execute_suite


! Write suite in XML
subroutine xfunit_manager_write_xml_suite( this, suite )

! The test manager
  class(t_xfunit_manager), intent(inout) :: this

! The test suite
  type(t_xfunit_suite), intent(in) :: suite

! Local variables
  integer :: ios
  type(t_xml_writer) :: xml
  type(t_xml_writer_settings) :: settings

! Open the file
  call this%handler%open( write=.true. )
  if( this%handler%is_open() ) then

!   Initialise the XML handler
    settings = xml_writer_settings()
    call settings%set_indent(.true.)
    call settings%set_indent_chars('  ')
    xml = xml_writer( this%handler%get_unit(), settings )

!   Initialise the file
    call xml%write_version()

!   Write the suite
    call suite%write_xml( xml, this%junit_strict, this%fail_only, this%stdout, this%stderr )

!   Close the file
    call this%handler%close()

  else

!   Report the error
    ios = this%handler%get_iostat()
    call this%msg%error( 'm_xfunit_manager', &
                         'xfunit_manager_write_xml_suite', 1, &
                         'Failed to open ' // trim(this%unit_jxml_file%character()) // &
                         ' with iostat=' // trim(character(ios)) )

  end if

end subroutine xfunit_manager_write_xml_suite


! Get the unit test execution patterns
pure function xfunit_manager_unit_test_filter_get( this ) result(res)

! The unit test manager
  class(t_xfunit_manager), intent(in) :: this

! The unit test filter patterns
  type(t_string), dimension(:), allocatable :: res

! Local variables
  integer :: nfilter, lfilter

! Check for patterns
  if( allocated(this%unit_test_filter) ) then

!   Intialise structure
    nfilter = size(this%unit_test_filter)
    lfilter = len_trim(this%unit_test_filter(1))
    allocate( res(nfilter) )

!   Return the patterns
    res = this%unit_test_filter

  else

!   Allocate empty
    allocate( res(0) )

  end if

end function xfunit_manager_unit_test_filter_get


! Access functions

! Getter for the unit testing root directory
elemental function xfunit_manager_unit_root_dir_get( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%xfunit_root_dir

end function xfunit_manager_unit_root_dir_get


! Getter for unit test data directory
elemental function xfunit_manager_unit_data_dir_get( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%unit_data_dir

end function xfunit_manager_unit_data_dir_get


! Getter for unit test reference data directory
elemental function xfunit_manager_unit_ref_dir_get( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%unit_ref_dir

end function xfunit_manager_unit_ref_dir_get


! Getter for unit test JXML file directory
elemental function xfunit_manager_unit_jxml_dir_get( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%unit_jxml_dir

end function xfunit_manager_unit_jxml_dir_get


! Getter for unit test JXML file
elemental function xfunit_manager_unit_jxml_file_get( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  type(t_string) :: res

! Set the return value
  res = this%unit_jxml_file

end function xfunit_manager_unit_jxml_file_get


! Check error condition
pure function xfunit_manager_is_error( this ) result(res)

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Return value
  logical :: res

! Return the status
  res = this%msg%on_error()

end function xfunit_manager_is_error


! Dump error messages to selected unit
subroutine xfunit_manager_dump_error( this, unit )

! Callling object
  class(t_xfunit_manager), intent(in) :: this

! Output unit to dump errors
  integer, intent(in) :: unit

! Dump error messages
  call this%msg%dump_errors( unit )

! Dump warning messages
  call this%msg%dump_warnings( unit )

end subroutine xfunit_manager_dump_error

end module m_xfunit_manager
