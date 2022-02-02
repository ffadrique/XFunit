module m_xfunit_unit

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit test top level implementation and interface
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_object
  use m_string
  use m_messages

  use m_xml

  use m_xfunit_assertion
  use m_xfunit_assertion_array

  use m_xfunit_assertion_integer
  use m_xfunit_assertion_real
  use m_xfunit_assertion_complex
  use m_xfunit_assertion_character
  use m_xfunit_assertion_string
  use m_xfunit_assertion_logical
  use m_xfunit_assertion_class
  use m_xfunit_assertion_pass
  use m_xfunit_assertion_fail

  use m_xfunit_assertion_integer_between
  use m_xfunit_assertion_real_between

  use m_xfunit_assertion_integer_greater
  use m_xfunit_assertion_real_greater

  use m_xfunit_assertion_integer_less
  use m_xfunit_assertion_real_less

  use m_xfunit_assertion_array_integer
  use m_xfunit_assertion_array_real
  use m_xfunit_assertion_array_complex
  use m_xfunit_assertion_array_character
  use m_xfunit_assertion_array_string
  use m_xfunit_assertion_array_logical
  use m_xfunit_assertion_array_class

  use m_xfunit_assertion_array_integer_between
  use m_xfunit_assertion_array_real_between

  use m_xfunit_assertion_array_integer_greater
  use m_xfunit_assertion_array_real_greater

  use m_xfunit_assertion_array_integer_less
  use m_xfunit_assertion_array_real_less

  use m_xfunit_assertion_matrix_integer
  use m_xfunit_assertion_matrix_real
  use m_xfunit_assertion_matrix_class

  use m_xfunit_assertion_files

  use m_xfunit_report

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_unit

  public xfunit_unit

  public xfunit_character_match_exact, &
         xfunit_character_match_global, &
         xfunit_character_match_regexp

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> The character and string matching strategies (for character and strinc assertions)
  integer, parameter :: xfunit_character_match_exact = xfunit_assertion_character_match_exact
  integer, parameter :: xfunit_character_match_global = xfunit_assertion_character_match_global
  integer, parameter :: xfunit_character_match_regexp = xfunit_assertion_character_match_regexp

!> The unit test case class
  type, extends(t_object) :: t_xfunit_unit
    private

!>     Unit test name
      type(t_string) :: name

!>     Unit test annotation
      type(t_string) :: annotation

!>     Class name covered by the unit test
      type(t_string) :: classname

!>     Flag to skip the test (user selected)
      logical :: skip = .false.
      
!>     Flag for test execution
      logical :: executed = .false.

!>     Count of passed assertions
      integer :: passed_assertions = 0

!>     Count of failed assertions
      integer :: failed_assertions = 0

!>     Unit test report
      type(t_xfunit_report), allocatable :: report

!>     Variables for unit test execution timing
      real(kind=8) :: time0 = 0.0_8
      real(kind=8) :: time1 = 0.0_8

!>     Pointer to the unit test executer subroutine
      procedure (xfunit_unit_executer), pointer :: executer => null()

!>     Pointer to the subroutine to be executer prior to the unit test
      procedure (xfunit_unit_executer), pointer :: before => null()

!>     Pointer to the subroutine to be executer after the unit test
      procedure (xfunit_unit_executer), pointer :: after => null()

!>     Unit test execution status (0=success; otherwise error)
      integer :: status = 0

!>     Unit test error message
      type(t_string) :: error_message

!>     Error messages strucutre
!>     Contais messages stack from overall execution
      type(t_messages) :: msg

    contains

!     Assertions for comparison (equal)
      generic :: assert_equal => xfunit_unit_assert_equal_integer_k1, &
                                 xfunit_unit_assert_equal_integer_k2, &
                                 xfunit_unit_assert_equal_integer_k4, &
                                 xfunit_unit_assert_equal_integer_k8, &
                                 xfunit_unit_assert_equal_real_k4, &
                                 xfunit_unit_assert_equal_real_k8, &
                                 xfunit_unit_assert_equal_complex_k4, &
                                 xfunit_unit_assert_equal_complex_k8, &
                                 xfunit_unit_assert_equal_character, &
                                 xfunit_unit_assert_equal_string, &
                                 xfunit_unit_assert_equal_logical_k1, &
                                 xfunit_unit_assert_equal_logical_k2, &
                                 xfunit_unit_assert_equal_logical_k4, &
                                 xfunit_unit_assert_equal_class, &
                                 xfunit_unit_assert_equal_array_integer_k1, &
                                 xfunit_unit_assert_equal_array_integer_k2, &
                                 xfunit_unit_assert_equal_array_integer_k4, &
                                 xfunit_unit_assert_equal_array_integer_k8, &
                                 xfunit_unit_assert_equal_array_integer_k1_common, &
                                 xfunit_unit_assert_equal_array_integer_k2_common, &
                                 xfunit_unit_assert_equal_array_integer_k4_common, &
                                 xfunit_unit_assert_equal_array_integer_k8_common, &
                                 xfunit_unit_assert_equal_array_real_k4, &
                                 xfunit_unit_assert_equal_array_real_k4_thr, &
                                 xfunit_unit_assert_equal_array_real_k4_common, &
                                 xfunit_unit_assert_equal_array_real_k8, &
                                 xfunit_unit_assert_equal_array_real_k8_thr, &
                                 xfunit_unit_assert_equal_array_real_k8_common, &
                                 xfunit_unit_assert_equal_array_complex_k4, &
                                 xfunit_unit_assert_equal_array_complex_k4_thr, &
                                 xfunit_unit_assert_equal_array_complex_k4_common, &
                                 xfunit_unit_assert_equal_array_complex_k8, &
                                 xfunit_unit_assert_equal_array_complex_k8_thr, &
                                 xfunit_unit_assert_equal_array_complex_k8_common, &
                                 xfunit_unit_assert_equal_array_character, &
                                 xfunit_unit_assert_equal_array_character_common, &
                                 xfunit_unit_assert_equal_array_string, &
                                 xfunit_unit_assert_equal_array_string_common, &
                                 xfunit_unit_assert_equal_array_logical_k1, &
                                 xfunit_unit_assert_equal_array_logical_k2, &
                                 xfunit_unit_assert_equal_array_logical_k4, &
                                 xfunit_unit_assert_equal_array_logical_k1_common, &
                                 xfunit_unit_assert_equal_array_logical_k2_common, &
                                 xfunit_unit_assert_equal_array_logical_k4_common, &
                                 xfunit_unit_assert_equal_array_class, &
                                 xfunit_unit_assert_equal_array_class_common, &
                                 xfunit_unit_assert_equal_matrix_integer_k1, &
                                 xfunit_unit_assert_equal_matrix_integer_k2, &
                                 xfunit_unit_assert_equal_matrix_integer_k4, &
                                 xfunit_unit_assert_equal_matrix_integer_k8, &
                                 xfunit_unit_assert_equal_matrix_integer_k1_common, &
                                 xfunit_unit_assert_equal_matrix_integer_k2_common, &
                                 xfunit_unit_assert_equal_matrix_integer_k4_common, &
                                 xfunit_unit_assert_equal_matrix_integer_k8_common, &
                                 xfunit_unit_assert_equal_matrix_real_k4, &
                                 xfunit_unit_assert_equal_matrix_real_k4_thr, &
                                 xfunit_unit_assert_equal_matrix_real_k4_common, &
                                 xfunit_unit_assert_equal_matrix_real_k8, &
                                 xfunit_unit_assert_equal_matrix_real_k8_thr, &
                                 xfunit_unit_assert_equal_matrix_real_k8_common, &
                                 xfunit_unit_assert_equal_matrix_class, &
                                 xfunit_unit_assert_equal_matrix_class_common
      procedure, private :: xfunit_unit_assert_equal_integer_k1
      procedure, private :: xfunit_unit_assert_equal_integer_k2
      procedure, private :: xfunit_unit_assert_equal_integer_k4
      procedure, private :: xfunit_unit_assert_equal_integer_k8
      procedure, private :: xfunit_unit_assert_equal_real_k4
      procedure, private :: xfunit_unit_assert_equal_real_k8
      procedure, private :: xfunit_unit_assert_equal_complex_k4
      procedure, private :: xfunit_unit_assert_equal_complex_k8
      procedure, private :: xfunit_unit_assert_equal_character
      procedure, private :: xfunit_unit_assert_equal_string
      procedure, private :: xfunit_unit_assert_equal_logical_k1
      procedure, private :: xfunit_unit_assert_equal_logical_k2
      procedure, private :: xfunit_unit_assert_equal_logical_k4
      procedure, private :: xfunit_unit_assert_equal_class
      procedure, private :: xfunit_unit_assert_equal_array_integer_k1
      procedure, private :: xfunit_unit_assert_equal_array_integer_k2
      procedure, private :: xfunit_unit_assert_equal_array_integer_k4
      procedure, private :: xfunit_unit_assert_equal_array_integer_k8
      procedure, private :: xfunit_unit_assert_equal_array_integer_k1_common
      procedure, private :: xfunit_unit_assert_equal_array_integer_k2_common
      procedure, private :: xfunit_unit_assert_equal_array_integer_k4_common
      procedure, private :: xfunit_unit_assert_equal_array_integer_k8_common
      procedure, private :: xfunit_unit_assert_equal_array_real_k4
      procedure, private :: xfunit_unit_assert_equal_array_real_k4_thr
      procedure, private :: xfunit_unit_assert_equal_array_real_k4_common
      procedure, private :: xfunit_unit_assert_equal_array_real_k8
      procedure, private :: xfunit_unit_assert_equal_array_real_k8_thr
      procedure, private :: xfunit_unit_assert_equal_array_real_k8_common
      procedure, private :: xfunit_unit_assert_equal_array_complex_k4
      procedure, private :: xfunit_unit_assert_equal_array_complex_k4_thr
      procedure, private :: xfunit_unit_assert_equal_array_complex_k4_common
      procedure, private :: xfunit_unit_assert_equal_array_complex_k8
      procedure, private :: xfunit_unit_assert_equal_array_complex_k8_thr
      procedure, private :: xfunit_unit_assert_equal_array_complex_k8_common
      procedure, private :: xfunit_unit_assert_equal_array_character
      procedure, private :: xfunit_unit_assert_equal_array_character_common
      procedure, private :: xfunit_unit_assert_equal_array_string
      procedure, private :: xfunit_unit_assert_equal_array_string_common
      procedure, private :: xfunit_unit_assert_equal_array_logical_k1
      procedure, private :: xfunit_unit_assert_equal_array_logical_k2
      procedure, private :: xfunit_unit_assert_equal_array_logical_k4
      procedure, private :: xfunit_unit_assert_equal_array_logical_k1_common
      procedure, private :: xfunit_unit_assert_equal_array_logical_k2_common
      procedure, private :: xfunit_unit_assert_equal_array_logical_k4_common
      procedure, private :: xfunit_unit_assert_equal_array_class
      procedure, private :: xfunit_unit_assert_equal_array_class_common
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k1
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k2
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k4
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k8
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k1_common
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k2_common
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k4_common
      procedure, private :: xfunit_unit_assert_equal_matrix_integer_k8_common
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k4
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k4_thr
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k4_common
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k8
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k8_thr
      procedure, private :: xfunit_unit_assert_equal_matrix_real_k8_common
      procedure, private :: xfunit_unit_assert_equal_matrix_class
      procedure, private :: xfunit_unit_assert_equal_matrix_class_common

!     Assertions for range comparison (between)
      generic :: assert_between => xfunit_unit_assert_between_integer_k1, &
                                   xfunit_unit_assert_between_integer_k2, &
                                   xfunit_unit_assert_between_integer_k4, &
                                   xfunit_unit_assert_between_integer_k8, &
                                   xfunit_unit_assert_between_real_k4, &
                                   xfunit_unit_assert_between_real_k8, &
                                   xfunit_unit_assert_between_array_integer_k1, &
                                   xfunit_unit_assert_between_array_integer_k2, &
                                   xfunit_unit_assert_between_array_integer_k4, &
                                   xfunit_unit_assert_between_array_integer_k8, &
                                   xfunit_unit_assert_between_array_real_k4, &
                                   xfunit_unit_assert_between_array_real_k8, &
                                   xfunit_unit_assert_between_array_integer_k1_common, &
                                   xfunit_unit_assert_between_array_integer_k2_common, &
                                   xfunit_unit_assert_between_array_integer_k4_common, &
                                   xfunit_unit_assert_between_array_integer_k8_common, &
                                   xfunit_unit_assert_between_array_real_k4_common, &
                                   xfunit_unit_assert_between_array_real_k8_common
      procedure, private :: xfunit_unit_assert_between_integer_k1
      procedure, private :: xfunit_unit_assert_between_integer_k2
      procedure, private :: xfunit_unit_assert_between_integer_k4
      procedure, private :: xfunit_unit_assert_between_integer_k8
      procedure, private :: xfunit_unit_assert_between_real_k4
      procedure, private :: xfunit_unit_assert_between_real_k8
      procedure, private :: xfunit_unit_assert_between_array_integer_k1
      procedure, private :: xfunit_unit_assert_between_array_integer_k2
      procedure, private :: xfunit_unit_assert_between_array_integer_k4
      procedure, private :: xfunit_unit_assert_between_array_integer_k8
      procedure, private :: xfunit_unit_assert_between_array_real_k4
      procedure, private :: xfunit_unit_assert_between_array_real_k8
      procedure, private :: xfunit_unit_assert_between_array_integer_k1_common
      procedure, private :: xfunit_unit_assert_between_array_integer_k2_common
      procedure, private :: xfunit_unit_assert_between_array_integer_k4_common
      procedure, private :: xfunit_unit_assert_between_array_integer_k8_common
      procedure, private :: xfunit_unit_assert_between_array_real_k4_common
      procedure, private :: xfunit_unit_assert_between_array_real_k8_common

!     Assertions for inequality comparison (greater)
      generic :: assert_greater => xfunit_unit_assert_greater_integer_k1, &
                                   xfunit_unit_assert_greater_integer_k2, &
                                   xfunit_unit_assert_greater_integer_k4, &
                                   xfunit_unit_assert_greater_integer_k8, &
                                   xfunit_unit_assert_greater_real_k4, &
                                   xfunit_unit_assert_greater_real_k8, &
                                   xfunit_unit_assert_greater_array_integer_k1, &
                                   xfunit_unit_assert_greater_array_integer_k2, &
                                   xfunit_unit_assert_greater_array_integer_k4, &
                                   xfunit_unit_assert_greater_array_integer_k8, &
                                   xfunit_unit_assert_greater_array_real_k4, &
                                   xfunit_unit_assert_greater_array_real_k8, &
                                   xfunit_unit_assert_greater_array_integer_k1_common, &
                                   xfunit_unit_assert_greater_array_integer_k2_common, &
                                   xfunit_unit_assert_greater_array_integer_k4_common, &
                                   xfunit_unit_assert_greater_array_integer_k8_common, &
                                   xfunit_unit_assert_greater_array_real_k4_common, &
                                   xfunit_unit_assert_greater_array_real_k8_common
      procedure, private :: xfunit_unit_assert_greater_integer_k1
      procedure, private :: xfunit_unit_assert_greater_integer_k2
      procedure, private :: xfunit_unit_assert_greater_integer_k4
      procedure, private :: xfunit_unit_assert_greater_integer_k8
      procedure, private :: xfunit_unit_assert_greater_real_k4
      procedure, private :: xfunit_unit_assert_greater_real_k8
      procedure, private :: xfunit_unit_assert_greater_array_integer_k1
      procedure, private :: xfunit_unit_assert_greater_array_integer_k2
      procedure, private :: xfunit_unit_assert_greater_array_integer_k4
      procedure, private :: xfunit_unit_assert_greater_array_integer_k8
      procedure, private :: xfunit_unit_assert_greater_array_real_k4
      procedure, private :: xfunit_unit_assert_greater_array_real_k8
      procedure, private :: xfunit_unit_assert_greater_array_integer_k1_common
      procedure, private :: xfunit_unit_assert_greater_array_integer_k2_common
      procedure, private :: xfunit_unit_assert_greater_array_integer_k4_common
      procedure, private :: xfunit_unit_assert_greater_array_integer_k8_common
      procedure, private :: xfunit_unit_assert_greater_array_real_k4_common
      procedure, private :: xfunit_unit_assert_greater_array_real_k8_common

!     Assertions for inequality comparison (less)
      generic :: assert_less => xfunit_unit_assert_less_integer_k1, &
                                xfunit_unit_assert_less_integer_k2, &
                                xfunit_unit_assert_less_integer_k4, &
                                xfunit_unit_assert_less_integer_k8, &
                                xfunit_unit_assert_less_real_k4, &
                                xfunit_unit_assert_less_real_k8, &
                                xfunit_unit_assert_less_array_integer_k1, &
                                xfunit_unit_assert_less_array_integer_k2, &
                                xfunit_unit_assert_less_array_integer_k4, &
                                xfunit_unit_assert_less_array_integer_k8, &
                                xfunit_unit_assert_less_array_real_k4, &
                                xfunit_unit_assert_less_array_real_k8, &
                                xfunit_unit_assert_less_array_integer_k1_common, &
                                xfunit_unit_assert_less_array_integer_k2_common, &
                                xfunit_unit_assert_less_array_integer_k4_common, &
                                xfunit_unit_assert_less_array_integer_k8_common, &
                                xfunit_unit_assert_less_array_real_k4_common, &
                                xfunit_unit_assert_less_array_real_k8_common
      procedure, private :: xfunit_unit_assert_less_integer_k1
      procedure, private :: xfunit_unit_assert_less_integer_k2
      procedure, private :: xfunit_unit_assert_less_integer_k4
      procedure, private :: xfunit_unit_assert_less_integer_k8
      procedure, private :: xfunit_unit_assert_less_real_k4
      procedure, private :: xfunit_unit_assert_less_real_k8
      procedure, private :: xfunit_unit_assert_less_array_integer_k1
      procedure, private :: xfunit_unit_assert_less_array_integer_k2
      procedure, private :: xfunit_unit_assert_less_array_integer_k4
      procedure, private :: xfunit_unit_assert_less_array_integer_k8
      procedure, private :: xfunit_unit_assert_less_array_real_k4
      procedure, private :: xfunit_unit_assert_less_array_real_k8
      procedure, private :: xfunit_unit_assert_less_array_integer_k1_common
      procedure, private :: xfunit_unit_assert_less_array_integer_k2_common
      procedure, private :: xfunit_unit_assert_less_array_integer_k4_common
      procedure, private :: xfunit_unit_assert_less_array_integer_k8_common
      procedure, private :: xfunit_unit_assert_less_array_real_k4_common
      procedure, private :: xfunit_unit_assert_less_array_real_k8_common

!     Assertions for true
      generic :: assert_true => xfunit_unit_assert_true_k1, &
                                xfunit_unit_assert_true_k2, &
                                xfunit_unit_assert_true_k4, &
                                xfunit_unit_assert_array_true_k1, &
                                xfunit_unit_assert_array_true_k2, &
                                xfunit_unit_assert_array_true_k4
      procedure, private :: xfunit_unit_assert_true_k1
      procedure, private :: xfunit_unit_assert_true_k2
      procedure, private :: xfunit_unit_assert_true_k4
      procedure, private :: xfunit_unit_assert_array_true_k1
      procedure, private :: xfunit_unit_assert_array_true_k2
      procedure, private :: xfunit_unit_assert_array_true_k4

!     Assertions for false
      generic :: assert_false => xfunit_unit_assert_false_k1, &
                                 xfunit_unit_assert_false_k2, &
                                 xfunit_unit_assert_false_k4, &
                                 xfunit_unit_assert_array_false_k1, &
                                 xfunit_unit_assert_array_false_k2, &
                                 xfunit_unit_assert_array_false_k4
      procedure, private :: xfunit_unit_assert_false_k1
      procedure, private :: xfunit_unit_assert_false_k2
      procedure, private :: xfunit_unit_assert_false_k4
      procedure, private :: xfunit_unit_assert_array_false_k1
      procedure, private :: xfunit_unit_assert_array_false_k2
      procedure, private :: xfunit_unit_assert_array_false_k4

!>     Assertions that force pass and fail
      procedure :: assert_pass => xfunit_unit_assert_pass
      procedure :: assert_fail => xfunit_unit_assert_fail

!     Assertion for file comparison
      generic :: assert_compare_files => xfunit_unit_assert_compare_files_character, &
                                         xfunit_unit_assert_compare_files_string
      procedure, private :: xfunit_unit_assert_compare_files_character
      procedure, private :: xfunit_unit_assert_compare_files_string

!>     Serialization interfaces
      procedure :: write => xfunit_unit_write
      procedure :: write_xml => xfunit_unit_write_xml

!>     Execution handling
      procedure :: execute => xfunit_unit_execute
      procedure :: is_passed => xfunit_unit_is_passed
      procedure :: is_executed => xfunit_unit_is_executed

!     Operator interfaces (to allow inclusion in dyanmic containers)
      generic :: operator(==) => xfunit_unit_equal
      procedure, private :: xfunit_unit_equal
      generic :: operator(>)  => xfunit_unit_greater
      procedure, private :: xfunit_unit_greater
      generic :: operator(<)  => xfunit_unit_less
      procedure, private :: xfunit_unit_less
      generic :: assignment(=) => xfunit_unit_assign_xfunit_unit
      procedure, private :: xfunit_unit_assign_xfunit_unit

!>     Error handling
      procedure :: error => xfunit_unit_error
      procedure :: is_error => xfunit_unit_is_errored

!>     Getters
      procedure :: get_name => xfunit_unit_get_name
      procedure :: get_passed_assertions => xfunit_unit_get_passed_assertions
      procedure :: get_failed_assertions => xfunit_unit_get_failed_assertions
      procedure :: get_total_assertions => xfunit_unit_get_total_assertions
      procedure :: get_msg_ptr => xfunit_unit_get_msg_ptr
      procedure :: get_elapsed => xfunit_unit_get_elapsed

!>     Skip unit test flag handling
      procedure :: get_skip => xfunit_unit_get_skip
      procedure :: set_skip => xfunit_unit_set_skip
      
!>     Generic interface for assertion process
      procedure, private :: process_assertion => xfunit_unit_process_assertion

  end type t_xfunit_unit

!> Constructor interface
  interface xfunit_unit
    module procedure xfunit_unit_character
    module procedure xfunit_unit_string
  end interface xfunit_unit

!> Interface for the executer subroutine
  interface
    subroutine xfunit_unit_executer( ut )
      import t_xfunit_unit
      class(t_xfunit_unit), intent(inout) :: ut
    end subroutine xfunit_unit_executer
  end interface

!---End of declaration of module variables--------------------------------------

contains

!> Constructor from character
function xfunit_unit_character( name, classname, annotation, executer, before, after ) result(res)

!> The unit test case name
  character(len=*), intent(in) :: name

!> The class name (for object oriented unit testing)
  character(len=*), optional, intent(in) :: classname

!> The test suite annotation
  character(len=*), optional, intent(in) :: annotation

!> The unit test executer
  procedure (xfunit_unit_executer) :: executer

!> The unit test procedure to be exectured before the test
  procedure (xfunit_unit_executer), optional :: before

!> The unit test procedure to be exectured after the test
  procedure (xfunit_unit_executer), optional :: after

!> The unit test case structure
  type(t_xfunit_unit) :: res

! Assign the unit case name
  res%name = name

! Assign the class name
  if( present(classname) ) then
    res%classname = classname
  end if

! Assign annotation
  if( present(annotation) ) then
    res%annotation = annotation
  end if

! Initialise report
  allocate( res%report )
  res%report = xfunit_report()

! Initialise execution time
  res%time0 = 0.0_8
  res%time1 = 0.0_8

! Initialise the test executer
  res%executer => executer

! Initialise the error condition
  res%status = 0
  res%error_message = ''
  call res%msg%reset_error()

! Initialise the procedures to prepare the test execution
  if( present(before) ) then
    res%before => before
  end if

! Initialise the procedures to clean-up test execution
  if( present(after) ) then
    res%after => after
  end if

end function xfunit_unit_character


!> Constructor from string
function xfunit_unit_string( name, classname, annotation, executer, before, after ) result(res)

!> The unit test case name
  type(t_string), intent(in) :: name

!> The class name (for object oriented unit testing)
  type(t_string), optional, intent(in) :: classname

!> The test suite annotation
  type(t_string), optional, intent(in) :: annotation

!> The unit test executer
  procedure (xfunit_unit_executer) :: executer

!> The unit test procedure to be exectured before the test
  procedure (xfunit_unit_executer), optional :: before

!> The unit test procedure to be exectured after the test
  procedure (xfunit_unit_executer), optional :: after

!> The unit test case structure
  type(t_xfunit_unit) :: res

! Invoke the character interface
  res = xfunit_unit( name%character(), classname%character(), annotation%character(), executer, before, after )

end function xfunit_unit_string


!> Execute the test
subroutine xfunit_unit_execute( ut )

!> The test case strcuture
  class(t_xfunit_unit), intent(inout) :: ut

! Initialise
  ut%status = 0

! Execute the preparation routine
  if( associated(ut%before) ) then
    call ut%before()
  end if

! Check status
  if( ut%status == 0 ) then

!   Initialise cummulated time
    call cpu_time( ut%time0 )

!   Execute the test
    call ut%executer()

!   Compute cummulated time
    call cpu_time( ut%time1 )

!   Set execution flag
    ut%executed = .true.

  end if

! Execute the teardown routine
  if( associated(ut%after) ) then
    call ut%after()
  end if

end subroutine xfunit_unit_execute


!> Retrieve passed status
elemental function xfunit_unit_is_passed( ut ) result(res)

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The passed status
  logical :: res

! Compute the result
  res = ut%executed .and. ut%status == 0 .and. ( ut%failed_assertions == 0 )

end function xfunit_unit_is_passed


!> Retrieve passed status
elemental function xfunit_unit_is_errored( ut ) result(res)

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The passed status
  logical :: res

! Compute the result
  res = ( ut%status /= 0 )

end function xfunit_unit_is_errored


!> Retrieve executed status
elemental function xfunit_unit_is_executed( ut ) result(res)

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The executed status
  logical :: res

! Compute the result
  res = ut%executed

end function xfunit_unit_is_executed


!> Set the unit test execution on error (status=0 to reset)
pure subroutine xfunit_unit_error( ut, status, text, msg )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> The provided error status
  integer, intent(in) :: status

!> The error message
  character(len=*), optional, intent(in) :: text

!> Message stack generated during the test execution
  type(t_messages), optional, intent(in) :: msg

! Set the status
  ut%status = status

! Set the text
  if( present(text) ) then
    ut%error_message = text
  else
    ut%error_message = ' '
  end if

! Set the error stack
  if( present(msg) ) then
    ut%msg = msg
  end if

end subroutine xfunit_unit_error


!> Write report in plain text
subroutine xfunit_unit_write( ut, unit, fail_only )

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The open fortran unit to write to
  integer, intent(in) :: unit

!> Flag to select only failed assertions for the report
  logical, intent(in) :: fail_only

! Local variables
  type(t_string) :: local

! Write the test header
  local = trim(adjustl(ut%name))
  write( unit, '(a)' ) repeat('#',78)
  write( unit, '(a,1x,a,1x,a)' ) repeat('#',10), character(local), repeat('#',66-len(local))
  write( unit, '(a)' ) repeat('#',78)

! Generate the report
  call ut%report%write( unit, fail_only )

! Write the test footer
  write( unit, '(a)' ) repeat('-',78)

end subroutine xfunit_unit_write


!> Write report in XML
subroutine xfunit_unit_write_xml( ut, xml, junit_strict, fail_only )

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The XML context
  type(t_xml_writer), intent(inout) :: xml

!> The flag for strict JUnit generation report
  logical, intent(in) :: junit_strict

!> The flag to output failed assertions only
  logical, intent(in) :: fail_only

! Local variables
  type(t_xml_attribute), dimension(3) :: attr
  character(len=32) :: stime

! Write the report start tag
  write( stime, '(f0.3)' ) ut%time1 - ut%time0
  attr(1) = xml_attribute( 'name', ut%name )
  attr(2) = xml_attribute( 'time', trim(stime) )
  attr(3) = xml_attribute( 'classname', ut%classname )
  call xml%write_start_tag( 'testcase', attr=attr, newline=.true. )

! Check is skip this test has been requested
  if( .not. ut%skip ) then

!   If on error, generate the error entry
    if( ut%is_error() ) then
      attr(1) = xml_attribute( 'type', ut%status )
      attr(2) = xml_attribute( 'message', ut%error_message )
      call xml%write_start_tag( 'error', attr=attr(:2), newline=.true. )
      call ut%msg%dump_errors( xml%get_unit() )
      call xml%write_end_tag( 'error' )
    end if

!   If test is failed, genereate the failure entry
    if( .not. ut%is_passed() ) then
      attr(1) = xml_attribute( 'type', 'assertion failed' )
      call xml%write_start_tag( 'failure', attr=attr(:1), newline=.true. )
      call ut%write( xml%get_unit(), fail_only=.true. )
      call xml%write_end_tag( 'failure' )
    end if

!   Check if detailed report is to be generated
    if( .not. junit_strict ) then

!     Write the start tag (not compatible with JUnit XML schema)
      attr(1) = xml_attribute( 'assertions', ut%passed_assertions + ut%failed_assertions )
      attr(2) = xml_attribute( 'failures',ut%failed_assertions )
      attr(3) = xml_attribute( 'annotation',ut%annotation )
      call xml%write_start_tag( 'testcase-detail', attr=attr, newline=.true. )

!     Generate the report
      call ut%report%write_xml( xml, fail_only )

!     Write the end tag
      call xml%write_end_tag( 'testcase-detail' )

    end if
    
  else
  
!   Test is skipped
    call xml%write_terminal( 'skipped' )
  
  end if

! Write the report end tag
  call xml%write_end_tag( 'testcase' )

end subroutine xfunit_unit_write_xml


!> Assertion for integer equality (kind=1)
subroutine xfunit_unit_assert_equal_integer_k1( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), intent(in) :: actual

!> The expected value
  integer(kind=1), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_equal_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_integer_k1


!> Assertion for integer equality (kind=2)
subroutine xfunit_unit_assert_equal_integer_k2( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), intent(in) :: actual

!> The expected value
  integer(kind=2), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_equal_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_integer_k2


!> Assertion for integer equality (kind=4)
subroutine xfunit_unit_assert_equal_integer_k4( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), intent(in) :: actual

!> The expected value
  integer(kind=4), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_equal_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_integer_k4


!> Assertion for integer equality (kind=4)
subroutine xfunit_unit_assert_equal_integer_k8( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), intent(in) :: actual

!> The expected value
  integer(kind=8), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_integer) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_integer( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_integer_k8


!> Assertion for real equality (kind=4)
subroutine xfunit_unit_assert_equal_real_k4( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real, intent(in) :: actual

!> The expected value
  real, intent(in) :: expected

!> The comparison threshold (optional)
  real, optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum real kind for assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_real_k8( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_real_k8( name, real(actual,8), real(expected,8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_real_k4


!> Assertion for real(kind=8) equality
subroutine xfunit_unit_assert_equal_real_k8( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), intent(in) :: actual

!> The expected value
  real(kind=8), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_real_k8


!> Assertion for complex equality
subroutine xfunit_unit_assert_equal_complex_k4( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=4), intent(in) :: actual

!> The expected value
  complex(kind=4), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum complex kind for assertion
! Use maximum real kind for assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_complex_k8( name, cmplx(actual,kind=8), cmplx(expected,kind=8), real(threshold,8), status )
  else
    call ut%xfunit_unit_assert_equal_complex_k8( name, cmplx(actual,kind=8), cmplx(expected,kind=8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_complex_k4


!> Assertion for complex(kind=8) equality
subroutine xfunit_unit_assert_equal_complex_k8( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=8), intent(in) :: actual

!> The expected value
  complex(kind=8), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_complex) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_complex( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_complex_k8


!> Assertion for character equality
subroutine xfunit_unit_assert_equal_character( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  character(len=*), intent(in) :: actual

!> The expected value
  character(len=*), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_character) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_character( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_character


!> Assertion for string equality
subroutine xfunit_unit_assert_equal_string( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  type(t_string), intent(in) :: actual

!> The expected value
  type(t_string), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_string) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_string( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_string


!> Assertion for logical equality (kind=1)
subroutine xfunit_unit_assert_equal_logical_k1( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), intent(in) :: actual

!> The expected value
  logical(kind=1), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum logical kind for assertion
  call ut%xfunit_unit_assert_equal_logical_k4( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_logical_k1


!> Assertion for logical equality (kind=2)
subroutine xfunit_unit_assert_equal_logical_k2( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), intent(in) :: actual

!> The expected value
  logical(kind=2), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum logical kind for assertion
  call ut%xfunit_unit_assert_equal_logical_k4( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_logical_k2


!> Assertion for logical equality (kind=4)
subroutine xfunit_unit_assert_equal_logical_k4( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), intent(in) :: actual

!> The expected value
  logical(kind=4), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_logical) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_logical( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_logical_k4


!> Assertion for logical equality (kind=4)
subroutine xfunit_unit_assert_equal_class( ut, name, actual, expected, equal, serialize, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  class(*), intent(in) :: actual

!> The expected value
  class(*), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_class) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_class( name, actual, expected, equal, serialize )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_class


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_between_integer_k1( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), intent(in) :: actual

!> The assertion low bound value
  integer(kind=1), intent(in) :: low

!> The assertion high bound value
  integer(kind=1), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum intege kind for assertion
  call ut%xfunit_unit_assert_between_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_between_integer_k2( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), intent(in) :: actual

!> The assertion low bound value
  integer(kind=2), intent(in) :: low

!> The assertion high bound value
  integer(kind=2), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_between_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_between_integer_k4( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), intent(in) :: actual

!> The assertion low bound value
  integer(kind=4), intent(in) :: low

!> The assertion high bound value
  integer(kind=4), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum intege kind for assertion
  call ut%xfunit_unit_assert_between_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_between_integer_k8( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), intent(in) :: actual

!> The assertion low bound value
  integer(kind=8), intent(in) :: low

!> The assertion high bound value
  integer(kind=8), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_integer_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_integer_between( name, actual, low, high, open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_integer_k8


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_greater_integer_k1( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), intent(in) :: actual

!> The assertion low bound value
  integer(kind=1), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum intege rkind for assertion
  call ut%xfunit_unit_assert_greater_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_greater_integer_k2( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), intent(in) :: actual

!> The assertion low bound value
  integer(kind=2), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_greater_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_greater_integer_k4( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), intent(in) :: actual

!> The assertion low bound value
  integer(kind=4), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum intege rkind for assertion
  call ut%xfunit_unit_assert_greater_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_greater_integer_k8( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), intent(in) :: actual

!> The assertion low bound value
  integer(kind=8), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_integer_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_integer_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_integer_k8


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_less_integer_k1( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), intent(in) :: actual

!> The assertion high bound value
  integer(kind=1), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_less_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_less_integer_k2( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), intent(in) :: actual

!> The assertion high bound value
  integer(kind=2), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_less_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_less_integer_k4( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), intent(in) :: actual

!> The assertion high bound value
  integer(kind=4), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum integer kind for assertion
  call ut%xfunit_unit_assert_less_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_less_integer_k8( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), intent(in) :: actual

!> The assertion high bound value
  integer(kind=8), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_integer_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_integer_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_integer_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_between_real_k4( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), intent(in) :: actual

!> The assertion low bound value
  real(kind=4), intent(in) :: low

!> The assertion high bound value
  real(kind=4), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum real kind for assertion
  call ut%xfunit_unit_assert_between_real_k8( name, real(actual,8), real(low,8), real(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_real_k4


!> Assertion for real range (kind=8)
subroutine xfunit_unit_assert_between_real_k8( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), intent(in) :: actual

!> The assertion low bound value
  real(kind=8), intent(in) :: low

!> The assertion high bound value
  real(kind=8), intent(in) :: high

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_real_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_real_between( name, actual, low, high, open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_real_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_less_real_k4( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), intent(in) :: actual

!> The assertion high bound value
  real(kind=4), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum real kind for assertion
  call ut%xfunit_unit_assert_less_real_k8( name, real(actual,8), real(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_real_k4


!> Assertion for real range (kind=8)
subroutine xfunit_unit_assert_less_real_k8( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), intent(in) :: actual

!> The assertion high bound value
  real(kind=8), intent(in) :: high

!> The high bound is open (optional; close by default)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_real_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_real_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_real_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_greater_real_k4( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), intent(in) :: actual

!> The assertion low bound value
  real(kind=4), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum real kind for assertion
  call ut%xfunit_unit_assert_greater_real_k8( name, real(actual,8), real(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_real_k4


!> Assertion for real range (kind=8)
subroutine xfunit_unit_assert_greater_real_k8( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), intent(in) :: actual

!> The assertion low bound value
  real(kind=8), intent(in) :: low

!> The low bound is open (optional; close by default)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_real_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_real_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_real_k8


!> Assertion for integer equality (kind=1)
subroutine xfunit_unit_assert_equal_array_integer_k1( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=1), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k1


!> Assertion for integer equality (kind=2)
subroutine xfunit_unit_assert_equal_array_integer_k2( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=2), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k2


!> Assertion for integer equality (kind=4)
subroutine xfunit_unit_assert_equal_array_integer_k4( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=4), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k4


!> Assertion for integer equality (kind=8)
subroutine xfunit_unit_assert_equal_array_integer_k8( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=8), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer) :: assertion
  integer :: na, ne
  integer :: i

! Initialise
  na = size(actual)
  ne = size(expected)

! Check the relative sizes
  if( na > ne ) then

!   Construct the assertion structure paddind the expected array
    assertion = xfunit_assertion_array_integer( name, actual, [ expected, (0_8, i=1, na - ne + 1) ] )

  else

!   Construct the assertion structure truncating the expected array
    assertion = xfunit_assertion_array_integer( name, actual, expected(:na) )

  end if

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_integer_k8


!> Assertion for integer equality (kind=1) with common expected
subroutine xfunit_unit_assert_equal_array_integer_k1_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=1), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k1_common


!> Assertion for integer equality (kind=2) with common expected
subroutine xfunit_unit_assert_equal_array_integer_k2_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=2), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k2_common


!> Assertion for integer equality (kind=4) with common expected
subroutine xfunit_unit_assert_equal_array_integer_k4_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=4), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_array_integer_k4_common


!> Assertion for integer equality (kind=8) with common expected
subroutine xfunit_unit_assert_equal_array_integer_k8_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  integer(kind=8), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_integer_k8_common


!> Assertion for real equality (array threshold, kind=4)
subroutine xfunit_unit_assert_equal_array_real_k4( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=4), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), optional, dimension(:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_array_real_k8( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_array_real_k8( name, real(actual,8), real(expected,8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_array_real_k4


!> Assertion for real equality (common threshold, kind=4)
subroutine xfunit_unit_assert_equal_array_real_k4_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=4), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_equal_array_real_k8_thr( name, real(actual,8), real(expected,8), real(threshold,8), status )

end subroutine xfunit_unit_assert_equal_array_real_k4_thr


!> Assertion for real equality (common expected and threshold, kind=4)
subroutine xfunit_unit_assert_equal_array_real_k4_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=4), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_array_real_k8_common( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_array_real_k8_common( name, real(actual,8), real(expected,8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_array_real_k4_common


!> Assertion for real (kind=8) equality
subroutine xfunit_unit_assert_equal_array_real_k8( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=8), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, dimension(:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_real_k8


!> Assertion for real (kind=8) equality (common threshold)
subroutine xfunit_unit_assert_equal_array_real_k8_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=8), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_real_k8_thr


!> Assertion for real (kind=8) equality (common expected and threshold)
subroutine xfunit_unit_assert_equal_array_real_k8_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  real(kind=8), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_real_k8_common


!> Assertion for complex equality (kind=4)
subroutine xfunit_unit_assert_equal_array_complex_k4( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex, dimension(:), intent(in) :: actual

!> The expected value
  complex, dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real, optional, dimension(:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind complex assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_array_complex_k8( name, cmplx(actual,kind=8), cmplx(expected,kind=8), &
                                                       real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_array_complex_k8( name, cmplx(actual,kind=8), cmplx(expected,kind=8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_array_complex_k4


!> Assertion for complex equality (common threshold, kind=4)
subroutine xfunit_unit_assert_equal_array_complex_k4_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  complex(kind=4), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind complex assertion
  call ut%xfunit_unit_assert_equal_array_complex_k8_thr( name, cmplx(actual,kind=8), cmplx(expected,kind=8), &
                                                         real(threshold,kind=8), status )

end subroutine xfunit_unit_assert_equal_array_complex_k4_thr


!> Assertion for complex equality (common expected and threshold, kind=4)
subroutine xfunit_unit_assert_equal_array_complex_k4_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex, dimension(:), intent(in) :: actual

!> The expected value
  complex, intent(in) :: expected

!> The comparison threshold (optional)
  real, optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind complex assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_array_complex_k8_common( name, cmplx(actual,kind=8), cmplx(expected,kind=8), &
                                                              real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_array_complex_k8_common( name, cmplx(actual,kind=8), cmplx(expected,kind=8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_array_complex_k4_common


!> Assertion for double complex equality
subroutine xfunit_unit_assert_equal_array_complex_k8( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  complex(kind=8), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, dimension(:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_complex) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_complex( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_complex_k8


!> Assertion for double complex equality (common threshold)
subroutine xfunit_unit_assert_equal_array_complex_k8_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  complex(kind=8), dimension(:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_complex) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_complex( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_complex_k8_thr


!> Assertion for double complex equality (common expected and threshold)
subroutine xfunit_unit_assert_equal_array_complex_k8_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  complex(kind=8), dimension(:), intent(in) :: actual

!> The expected value
  complex(kind=8), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_complex) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_complex( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_complex_k8_common


!> Assertion for character equality
subroutine xfunit_unit_assert_equal_array_character( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  character(len=*), dimension(:), intent(in) :: actual

!> The expected value
  character(len=*), dimension(:), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_character) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_character( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_character


!> Assertion for character equality
subroutine xfunit_unit_assert_equal_array_character_common( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  character(len=*), dimension(:), intent(in) :: actual

!> The expected value
  character(len=*), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_character) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_character( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_character_common


!> Assertion for string equality
subroutine xfunit_unit_assert_equal_array_string( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  type(t_string), dimension(:), intent(in) :: actual

!> The expected value
  type(t_string), dimension(:), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_string) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_string( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_string


!> Assertion for string equality
subroutine xfunit_unit_assert_equal_array_string_common( ut, name, actual, expected, matching, ignorecase, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  type(t_string), dimension(:), intent(in) :: actual

!> The expected value
  type(t_string), intent(in) :: expected

!> The character matching strategy (optional, default to exact)
!> Enumerated values in m_xfunit_assertion
  integer, optional, intent(in) :: matching

!> Ignore case in comparison (optional)
  logical, optional, intent(in) :: ignorecase

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_string) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_string( name, actual, expected, matching=matching, ignorecase=ignorecase )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_string_common


!> Assertion for logical equality (kind=1)
subroutine xfunit_unit_assert_equal_array_logical_k1( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=1), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_logical_k4( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_array_logical_k1


!> Assertion for logical equality (kind=2)
subroutine xfunit_unit_assert_equal_array_logical_k2( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=2), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_logical_k4( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_array_logical_k2


!> Assertion for logical equality (kind=4)
subroutine xfunit_unit_assert_equal_array_logical_k4( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=4), dimension(:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_logical) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_logical( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_logical_k4


!> Assertion for logical equality (kind=1, common expected)
subroutine xfunit_unit_assert_equal_array_logical_k1_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=1), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_logical_k4_common( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_array_logical_k1_common


!> Assertion for logical equality (kind=2, common expected)
subroutine xfunit_unit_assert_equal_array_logical_k2_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=2), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_array_logical_k4_common( name, logical(actual,4), logical(expected,4), status )

end subroutine xfunit_unit_assert_equal_array_logical_k2_common


!> Assertion for logical equality (kind=4, common expected)
subroutine xfunit_unit_assert_equal_array_logical_k4_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), dimension(:), intent(in) :: actual

!> The expected value
  logical(kind=4), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_logical) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_logical( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_logical_k4_common


!> Assertion for generic class equality
subroutine xfunit_unit_assert_equal_array_class( ut, name, actual, expected, equal, serialize, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  class(*), dimension(:), intent(in) :: actual

!> The expected value
  class(*), dimension(:), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_class) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_class( name, actual, expected, equal, serialize )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_class


!> Assertion for generic class equality
subroutine xfunit_unit_assert_equal_array_class_common( ut, name, actual, expected, equal, serialize, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  class(*), dimension(:), intent(in) :: actual

!> The expected value
  class(*), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_class) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_class( name, actual, expected, equal, serialize )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_array_class_common


!> Assertion for integer equality (kind=1)
subroutine xfunit_unit_assert_equal_matrix_integer_k1( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=1), dimension(:,:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k1


!> Assertion for integer equality (kind=2)
subroutine xfunit_unit_assert_equal_matrix_integer_k2( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=2), dimension(:,:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k2


!> Assertion for integer equality (kind=4)
subroutine xfunit_unit_assert_equal_matrix_integer_k4( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=4), dimension(:,:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k4


!> Assertion for integer equality (kind=8)
subroutine xfunit_unit_assert_equal_matrix_integer_k8( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=8), dimension(:,:), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_integer) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_integer( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k8


!> Assertion for integer equality (kind=1) with common expected
subroutine xfunit_unit_assert_equal_matrix_integer_k1_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=1), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k1_common


!> Assertion for integer equality (kind=2) with common expected
subroutine xfunit_unit_assert_equal_matrix_integer_k2_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=2), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k2_common


!> Assertion for integer equality (kind=4) with common expected
subroutine xfunit_unit_assert_equal_matrix_integer_k4_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=4), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_equal_matrix_integer_k8_common( name, int(actual,8), int(expected,8), status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k4_common


!> Assertion for integer equality (kind=8) with common expected
subroutine xfunit_unit_assert_equal_matrix_integer_k8_common( ut, name, actual, expected, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:,:), intent(in) :: actual

!> The expected value
  integer(kind=8), intent(in) :: expected

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_integer) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_integer( name, actual, expected )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_integer_k8_common


!> Assertion for real equality (matrix threshold, kind=4)
subroutine xfunit_unit_assert_equal_matrix_real_k4( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=4), dimension(:,:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), optional, dimension(:,:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_matrix_real_k8( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_matrix_real_k8( name, real(actual,8), real(expected,8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_matrix_real_k4


!> Assertion for real equality (common threshold, kind=4)
subroutine xfunit_unit_assert_equal_matrix_real_k4_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=4), dimension(:,:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_equal_matrix_real_k8_thr( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )

end subroutine xfunit_unit_assert_equal_matrix_real_k4_thr


!> Assertion for real equality (common expected and threshold, kind=4)
subroutine xfunit_unit_assert_equal_matrix_real_k4_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=4), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=4), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables

! Use maximum kind real assertion
  if( present(threshold) ) then
    call ut%xfunit_unit_assert_equal_matrix_real_k8_common( name, real(actual,8), real(expected,8), real(threshold,kind=8), status )
  else
    call ut%xfunit_unit_assert_equal_matrix_real_k8_common( name, real(actual,8), real(expected,8), status=status )
  end if

end subroutine xfunit_unit_assert_equal_matrix_real_k4_common


!> Assertion for real (kind=8) equality
subroutine xfunit_unit_assert_equal_matrix_real_k8( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, dimension(:,:), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_real_k8


!> Assertion for real (kind=8) equality (common threshold)
subroutine xfunit_unit_assert_equal_matrix_real_k8_thr( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=8), dimension(:,:), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_real) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_real( name, actual, expected, threshold )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_real_k8_thr


!> Assertion for real (kind=8) equality (common expected and threshold)
subroutine xfunit_unit_assert_equal_matrix_real_k8_common( ut, name, actual, expected, threshold, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:,:), intent(in) :: actual

!> The expected value
  real(kind=8), intent(in) :: expected

!> The comparison threshold (optional)
  real(kind=8), optional, intent(in) :: threshold

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_real) :: assertion

! Construct the assertion structure
  if( present(threshold) ) then
     assertion = xfunit_assertion_matrix_real( name, actual, expected, threshold )
  else
     assertion = xfunit_assertion_matrix_real( name, actual, expected )
  end if

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_real_k8_common


!> Assertion for generic class equality
subroutine xfunit_unit_assert_equal_matrix_class( ut, name, actual, expected, equal, serialize, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  class(*), dimension(:,:), intent(in) :: actual

!> The expected value
  class(*), dimension(:,:), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_class) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_class( name, actual, expected, equal, serialize )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_class


!> Assertion for generic class equality
subroutine xfunit_unit_assert_equal_matrix_class_common( ut, name, actual, expected, equal, serialize, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  class(*), dimension(:,:), intent(in) :: actual

!> The expected value
  class(*), intent(in) :: expected

!> The equality comparer
  procedure(xfunit_assertion_class_equal) :: equal

!> Serialization function
  procedure(xfunit_assertion_class_serialize), optional :: serialize

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_matrix_class) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_matrix_class( name, actual, expected, equal, serialize )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_equal_matrix_class_common


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_between_array_integer_k1( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=1), dimension(:), intent(in) :: low

!> The high bound value
  integer(kind=1), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_between_array_integer_k2( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=2), dimension(:), intent(in) :: low

!> The high bound value
  integer(kind=2), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_between_array_integer_k4( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=4), dimension(:), intent(in) :: low

!> The high bound value
  integer(kind=4), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8( name, int(actual,8), int(low,8), int(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_between_array_integer_k8( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=8), dimension(:), intent(in) :: low

!> The high bound value
  integer(kind=8), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_between( name, actual, low, high, open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_array_integer_k8


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_greater_array_integer_k1( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=1), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_greater_array_integer_k2( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=2), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_greater_array_integer_k4( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=4), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_greater_array_integer_k8( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=8), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_array_integer_k8


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_less_array_integer_k1( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=1), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k1


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_less_array_integer_k2( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=2), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k2


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_less_array_integer_k4( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=4), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k4


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_less_array_integer_k8( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=8), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_array_integer_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_between_array_real_k4( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=4), dimension(:), intent(in) :: low

!> The high bound value
  real(kind=4), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_between_array_real_k8( name, real(actual,8), real(low,8), real(high,8), open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_real_k4


!> Assertion for double range
subroutine xfunit_unit_assert_between_array_real_k8( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=8), dimension(:), intent(in) :: low

!> The high bound value
  real(kind=8), dimension(:), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_between( name, actual, low, high, open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_array_real_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_greater_array_real_k4( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=4), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_greater_array_real_k8( name, real(actual,8), real(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_real_k4


!> Assertion for double range
subroutine xfunit_unit_assert_greater_array_real_k8( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=8), dimension(:), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_array_real_k8


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_less_array_real_k4( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The high bound value
  real(kind=4), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_less_array_real_k8( name, real(actual,8), real(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_real_k4


!> Assertion for double range
subroutine xfunit_unit_assert_less_array_real_k8( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The high bound value
  real(kind=8), dimension(:), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_array_real_k8


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_between_array_integer_k1_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=1), intent(in) :: low

!> The high bound value
  integer(kind=1), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8_common( name, int(actual,8), int(low,8), int(high,8), &
                                                              open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k1_common


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_between_array_integer_k2_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=2), intent(in) :: low

!> The high bound value
  integer(kind=2), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8_common( name, int(actual,8), int(low,8), int(high,8), &
                                                              open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k2_common


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_between_array_integer_k4_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=4), intent(in) :: low

!> The high bound value
  integer(kind=4), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_between_array_integer_k8_common( name, int(actual,8), int(low,8), int(high,8), &
                                                              open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_integer_k4_common


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_between_array_integer_k8_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=8), intent(in) :: low

!> The high bound value
  integer(kind=8), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_between( name, int(actual,8), int(low,8), int(high,8), open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_array_integer_k8_common


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_greater_array_integer_k1_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=1), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8_common( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k1_common


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_greater_array_integer_k2_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=2), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8_common( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k2_common


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_greater_array_integer_k4_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=4), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_greater_array_integer_k8_common( name, int(actual,8), int(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_integer_k4_common


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_greater_array_integer_k8_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  integer(kind=8), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_array_integer_k8_common


!> Assertion for integer range (kind=1)
subroutine xfunit_unit_assert_less_array_integer_k1_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=1), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=1), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8_common( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k1_common


!> Assertion for integer range (kind=2)
subroutine xfunit_unit_assert_less_array_integer_k2_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=2), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=2), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8_common( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k2_common


!> Assertion for integer range (kind=4)
subroutine xfunit_unit_assert_less_array_integer_k4_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=4), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=4), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind for assertion evaluation
  call ut%xfunit_unit_assert_less_array_integer_k8_common( name, int(actual,8), int(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_integer_k4_common


!> Assertion for integer range (kind=8)
subroutine xfunit_unit_assert_less_array_integer_k8_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  integer(kind=8), dimension(:), intent(in) :: actual

!> The high bound value
  integer(kind=8), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_integer_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_integer_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_array_integer_k8_common


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_between_array_real_k4_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=4), intent(in) :: low

!> The high bound value
  real(kind=4), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_between_array_real_k8_common( name, real(actual,8), real(low,8), real(high,8), &
                                                           open_low, open_high, status )

end subroutine xfunit_unit_assert_between_array_real_k4_common


!> Assertion for double range
subroutine xfunit_unit_assert_between_array_real_k8_common( ut, name, actual, low, high, open_low, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=8), intent(in) :: low

!> The high bound value
  real(kind=8), intent(in) :: high

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_between) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_between( name, actual, low, high, open_low, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_between_array_real_k8_common


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_greater_array_real_k4_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=4), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_greater_array_real_k8_common( name, real(actual,8), real(low,8), open_low, status )

end subroutine xfunit_unit_assert_greater_array_real_k4_common


!> Assertion for double range
subroutine xfunit_unit_assert_greater_array_real_k8_common( ut, name, actual, low, open_low, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The low bound value
  real(kind=8), intent(in) :: low

!> The low bound is open (optional)
  logical, optional, intent(in) :: open_low

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_greater) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_greater( name, actual, low, open_low )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_greater_array_real_k8_common


!> Assertion for real range (kind=4)
subroutine xfunit_unit_assert_less_array_real_k4_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=4), dimension(:), intent(in) :: actual

!> The high bound value
  real(kind=4), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use maximum kind real assertion
  call ut%xfunit_unit_assert_less_array_real_k8_common( name, real(actual,8), real(high,8), open_high, status )

end subroutine xfunit_unit_assert_less_array_real_k4_common


!> Assertion for double range
subroutine xfunit_unit_assert_less_array_real_k8_common( ut, name, actual, high, open_high, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  real(kind=8), dimension(:), intent(in) :: actual

!> The high bound value
  real(kind=8), intent(in) :: high

!> The high bound is open (optional)
  logical, optional, intent(in) :: open_high

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_array_real_less) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_array_real_less( name, actual, high, open_high )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_less_array_real_k8_common


!> Assertion for logical expressions (.false. kind=1)
subroutine xfunit_unit_assert_false_k1( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._1, status )

end subroutine xfunit_unit_assert_false_k1


!> Assertion for logical expressions (.false. kind=2)
subroutine xfunit_unit_assert_false_k2( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._2, status )

end subroutine xfunit_unit_assert_false_k2


!> Assertion for logical expressions (.false. kind=4)
subroutine xfunit_unit_assert_false_k4( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._4, status )

end subroutine xfunit_unit_assert_false_k4


!> Assertion for logical expressions (.true. kind=1)
subroutine xfunit_unit_assert_true_k1( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._1, status )

end subroutine xfunit_unit_assert_true_k1


!> Assertion for logical expressions (.true. kind=2)
subroutine xfunit_unit_assert_true_k2( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._2, status )

end subroutine xfunit_unit_assert_true_k2


!> Assertion for logical expressions (.true. kind=4)
subroutine xfunit_unit_assert_true_k4( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._4, status )

end subroutine xfunit_unit_assert_true_k4


!> Assertion pass
subroutine xfunit_unit_assert_pass( ut, name, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_pass) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_pass( name )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_pass


!> Assertion fail
subroutine xfunit_unit_assert_fail( ut, name, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_fail) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_fail( name )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_fail


!> Assertion for logical expressions (.false. kind=1)
subroutine xfunit_unit_assert_array_false_k1( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._1, status )

end subroutine xfunit_unit_assert_array_false_k1


!> Assertion for logical expressions (.false. kind=2)
subroutine xfunit_unit_assert_array_false_k2( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._2, status )

end subroutine xfunit_unit_assert_array_false_k2


!> Assertion for logical expressions (.false. kind=4)
subroutine xfunit_unit_assert_array_false_k4( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .false._4, status )

end subroutine xfunit_unit_assert_array_false_k4


!> Assertion for logical expressions (.true. kind=1)
subroutine xfunit_unit_assert_array_true_k1( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=1), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._1, status )

end subroutine xfunit_unit_assert_array_true_k1


!> Assertion for logical expressions (.true. kind=2)
subroutine xfunit_unit_assert_array_true_k2( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=2), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._2, status )

end subroutine xfunit_unit_assert_array_true_k2


!> Assertion for logical expressions (.true. kind=4)
subroutine xfunit_unit_assert_array_true_k4( ut, name, actual, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  logical(kind=4), dimension(:), intent(in) :: actual

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Use equality assertion
  call ut%assert_equal( name, actual, .true._4, status )

end subroutine xfunit_unit_assert_array_true_k4


!> Assertion for file equality
subroutine xfunit_unit_assert_compare_files_character( ut, name, actual, expected, binary, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  character(len=*), intent(in) :: actual

!> The expected value
  character(len=*), intent(in) :: expected

!> Implement a binary comparison
  logical, optional, intent(in) :: binary

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_files) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_files( name, actual, expected, binary )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_compare_files_character


!> Assertion for file equality
subroutine xfunit_unit_assert_compare_files_string( ut, name, actual, expected, binary, status )

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> Name for the assertion
  character(len=*), intent(in) :: name

!> The actual computed value
  type(t_string), intent(in) :: actual

!> The expected value
  type(t_string), intent(in) :: expected

!> Implement a binary comparison
  logical, optional, intent(in) :: binary

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  type(t_xfunit_assertion_files) :: assertion

! Construct the assertion structure
  assertion = xfunit_assertion_files( name, actual%character(), expected%character(), binary )

! Process the assertion
  call ut%process_assertion( assertion, status )

end subroutine xfunit_unit_assert_compare_files_string


!> Generic processing of assertions
subroutine xfunit_unit_process_assertion( ut, assertion, status )

!> The unit test
  class(t_xfunit_unit), intent(inout) :: ut

!> The polymorphic pointer to the assertion
  class(t_xfunit_assertion), intent(in) :: assertion

!> The assertion result (optional)
  integer, optional, intent(out) :: status

! Local variables
  integer :: localstatus

! Process the assertion
  localstatus = assertion%get_status()
  if( localstatus == xfunit_assertion_is_pass ) then
    ut%passed_assertions = ut%passed_assertions + 1
  else
    ut%failed_assertions = ut%failed_assertions + 1
  end if
  if( present(status) ) then
    status = localstatus
  end if

! Add the assertion error to the error stack
  if( assertion%on_error() ) then
    call ut%msg%error( assertion%get_error() )
    ut%status = 1
  end if

! Add assertion to the report
  call ut%report%push_back( assertion )

end subroutine xfunit_unit_process_assertion


! Operators (to allow inclusion in dyanmic containers)

!> Equality operator
elemental function xfunit_unit_equal( left, right ) result(res)

!> Left operand
  class(t_xfunit_unit), intent(in) :: left

!> Right operand
  class(t_xfunit_unit), intent(in) :: right

!> Operation result
  logical :: res

! Set operation result
  res = ( left%name == right%name )

end function xfunit_unit_equal


!> Less than operator
elemental function xfunit_unit_less( left, right ) result(res)

!> Left operand
  class(t_xfunit_unit), intent(in) :: left

!> Right operand
  class(t_xfunit_unit), intent(in) :: right

!> Operation result
  logical :: res

! Set operation result
  res = ( left%name < right%name )

end function xfunit_unit_less


!> Greater than operator
elemental function xfunit_unit_greater( left, right ) result(res)

!> Left operand
  class(t_xfunit_unit), intent(in) :: left

!> Right operand
  class(t_xfunit_unit), intent(in) :: right

!> Operation result
  logical :: res

! Set operation result
  res = ( left%name > right%name )

end function xfunit_unit_greater


!> Assignment
subroutine xfunit_unit_assign_xfunit_unit( left, right )

!> Left operand
  class(t_xfunit_unit), intent(out) :: left

!> Right operand
  class(t_xfunit_unit), intent(in) :: right

! Assign fields
  left%name = right%name
  left%classname = right%classname
  left%annotation = right%annotation
  left%executed = right%executed
  left%passed_assertions = right%passed_assertions
  left%failed_assertions = right%failed_assertions
  left%time0 = right%time0
  left%time1 = right%time1
  left%executer => right%executer
  left%before => right%before
  left%after => right%after
  left%status = right%status
  left%error_message = right%error_message
  left%msg = right%msg

! Set report if there is one already allocated
  if( allocated(right%report) ) then
    allocate( left%report )
    left%report = right%report
  end if

end subroutine xfunit_unit_assign_xfunit_unit


!> Getter for unit test name
elemental function xfunit_unit_get_name( ut ) result(res)

!> Calling object
  class(t_xfunit_unit), intent(in) :: ut

!> Return value
  type(t_string) :: res

! Set the return value
  res = ut%name

end function xfunit_unit_get_name


!> Getter for the count of passed assertions
elemental function xfunit_unit_get_passed_assertions( ut ) result(res)

!> Calling object
  class(t_xfunit_unit), intent(in) :: ut

!> Return value
  integer :: res

! Set the return value
  res = ut%passed_assertions

end function xfunit_unit_get_passed_assertions


!> Getter for the count of failed assertions
elemental function xfunit_unit_get_failed_assertions( ut ) result(res)

!> Calling object
  class(t_xfunit_unit), intent(in) :: ut

!> Return value
  integer :: res

! Set the return value
  res = ut%failed_assertions

end function xfunit_unit_get_failed_assertions


!> Getter for the total count of assertions
elemental function xfunit_unit_get_total_assertions( ut ) result(res)

!> Calling object
  class(t_xfunit_unit), intent(in) :: ut

!> Return value
  integer :: res

! Set the return value
  res = ut%passed_assertions + ut%failed_assertions

end function xfunit_unit_get_total_assertions


!> Getter for the error message
function xfunit_unit_get_msg_ptr( ut ) result(res)

!> Calling object
  class(t_xfunit_unit), target, intent(in) :: ut

!> Return value
  type(t_messages), pointer :: res

! Set the return value
  res => ut%msg

end function xfunit_unit_get_msg_ptr


!> Get the test execution elapsed time
elemental function xfunit_unit_get_elapsed( this ) result(res)

!> Calling object
  class(t_xfunit_unit), intent(in) :: this

!> Elapsed time
  real(kind=8) :: res

! Compute elapsed time
  res = this%time1 - this%time0

end function xfunit_unit_get_elapsed


!> Get skip flag
elemental function xfunit_unit_get_skip( ut ) result(res)

!> The unit test case
  class(t_xfunit_unit), intent(in) :: ut

!> The skip flag
  logical :: res

! Compute the result
  res = ut%skip

end function xfunit_unit_get_skip


!> Set skip flag
elemental subroutine xfunit_unit_set_skip( ut, value ) 

!> The unit test case
  class(t_xfunit_unit), intent(inout) :: ut

!> The skip flag
  logical, intent(in) :: value

! Compute the result
  ut%skip = value

end subroutine xfunit_unit_set_skip


end module m_xfunit_unit
