module m_xfunit_assertion

!-------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Unit tests assertion parent type
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_object

  use m_string
  use m_util_convert

  use m_xml
  use m_msg

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion, &
         xfunit_assertion

  public xfunit_assertion_none_index, &
         xfunit_assertion_integer_index, &
         xfunit_assertion_real_index, &
         xfunit_assertion_complex_index, &
         xfunit_assertion_character_index, &
         xfunit_assertion_string_index, &
         xfunit_assertion_character_nocase_index, &
         xfunit_assertion_string_nocase_index, &
         xfunit_assertion_logical_index, &
         xfunit_assertion_class_index

  public xfunit_assertion_pass_index, &
         xfunit_assertion_fail_index

  public xfunit_assertion_integer_between_index, &
         xfunit_assertion_real_between_index

  public xfunit_assertion_integer_greater_index, &
         xfunit_assertion_real_greater_index

  public xfunit_assertion_integer_less_index, &
         xfunit_assertion_real_less_index

  public xfunit_assertion_array_integer_index, &
         xfunit_assertion_array_real_index, &
         xfunit_assertion_array_complex_index, &
         xfunit_assertion_array_character_index, &
         xfunit_assertion_array_string_index, &
         xfunit_assertion_array_character_nocase_index, &
         xfunit_assertion_array_string_nocase_index, &
         xfunit_assertion_array_logical_index, &
         xfunit_assertion_array_class_index

  public xfunit_assertion_array_integer_between_index, &
         xfunit_assertion_array_real_between_index

  public xfunit_assertion_array_integer_greater_index, &
         xfunit_assertion_array_real_greater_index

  public xfunit_assertion_array_integer_less_index, &
         xfunit_assertion_array_real_less_index

  public xfunit_assertion_matrix_integer_index, &
         xfunit_assertion_matrix_real_index, &
         xfunit_assertion_matrix_class_index

  public xfunit_assertion_files_index

  public xfunit_assertion_character_match_exact, &
         xfunit_assertion_character_match_global, &
         xfunit_assertion_character_match_regexp

  public xfunit_assertion_string_match_exact, &
         xfunit_assertion_string_match_global, &
         xfunit_assertion_string_match_regexp

  public xfunit_assertion_is_none, &
         xfunit_assertion_is_pass, &
         xfunit_assertion_is_fail

  public xfunit_real_scale

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

!> Assertion type enumeration
  integer, parameter :: xfunit_assertion_none_index                   =  0

  integer, parameter :: xfunit_assertion_integer_index                =  1
  integer, parameter :: xfunit_assertion_real_index                   =  2
  integer, parameter :: xfunit_assertion_complex_index                =  3
  integer, parameter :: xfunit_assertion_character_index              =  4
  integer, parameter :: xfunit_assertion_string_index                 =  5
  integer, parameter :: xfunit_assertion_character_nocase_index       =  6
  integer, parameter :: xfunit_assertion_string_nocase_index          =  7
  integer, parameter :: xfunit_assertion_logical_index                =  8
  integer, parameter :: xfunit_assertion_class_index                  =  9

  integer, parameter :: xfunit_assertion_pass_index                   = 10
  integer, parameter :: xfunit_assertion_fail_index                   = 11

  integer, parameter :: xfunit_assertion_integer_between_index        = 12
  integer, parameter :: xfunit_assertion_real_between_index           = 13

  integer, parameter :: xfunit_assertion_integer_greater_index        = 14
  integer, parameter :: xfunit_assertion_real_greater_index           = 15

  integer, parameter :: xfunit_assertion_integer_less_index           = 16
  integer, parameter :: xfunit_assertion_real_less_index              = 17

  integer, parameter :: xfunit_assertion_array_integer_index          = 18
  integer, parameter :: xfunit_assertion_array_real_index             = 19
  integer, parameter :: xfunit_assertion_array_complex_index          = 20
  integer, parameter :: xfunit_assertion_array_character_index        = 21
  integer, parameter :: xfunit_assertion_array_string_index           = 22
  integer, parameter :: xfunit_assertion_array_character_nocase_index = 23
  integer, parameter :: xfunit_assertion_array_string_nocase_index    = 24
  integer, parameter :: xfunit_assertion_array_logical_index          = 25
  integer, parameter :: xfunit_assertion_array_class_index            = 26

  integer, parameter :: xfunit_assertion_array_integer_between_index  = 27
  integer, parameter :: xfunit_assertion_array_real_between_index     = 28

  integer, parameter :: xfunit_assertion_array_integer_greater_index  = 29
  integer, parameter :: xfunit_assertion_array_real_greater_index     = 30

  integer, parameter :: xfunit_assertion_array_integer_less_index     = 31
  integer, parameter :: xfunit_assertion_array_real_less_index        = 32

  integer, parameter :: xfunit_assertion_matrix_real_index            = 33
  integer, parameter :: xfunit_assertion_matrix_integer_index         = 34
  integer, parameter :: xfunit_assertion_matrix_class_index           = 35

  integer, parameter :: xfunit_assertion_files_index                  = 36


!> Assertion type descriptors
  character(len=22), dimension(0:xfunit_assertion_files_index), parameter :: xfunit_assertion_name = [ &
    '                      ', &         ! none_index                   =  0
    'integer               ', &         ! integer_index                =  1
    'real                  ', &         ! real_index                   =  2
    'complex               ', &         ! complex_index                =  3
    'character             ', &         ! character_index              =  4
    'string                ', &         ! string_index                 =  5
    'character_nocase      ', &         ! character_nocase_index       =  6
    'string_nocase         ', &         ! string_nocase_index          =  7
    'logical               ', &         ! logical_index                =  8
    'class                 ', &         ! class_index                  =  9
    'pass                  ', &         ! pass_index                   = 10
    'fail                  ', &         ! fail_index                   = 11
    'integer_between       ', &         ! integer_between_index        = 12
    'real_between          ', &         ! real_between_index           = 13
    'integer_greater       ', &         ! integer_greater_index        = 14
    'real_greater          ', &         ! real_greater_index           = 15
    'integer_less          ', &         ! integer_less_index           = 16
    'real_less             ', &         ! real_less_index              = 17
    'array_integer         ', &         ! array_integer_index          = 18
    'array_real            ', &         ! array_real_index             = 19
    'array_complex         ', &         ! array_complex_index          = 20
    'array_character       ', &         ! array_character_index        = 21
    'array_string          ', &         ! array_string_index           = 22
    'array_character_nocase', &         ! array_character_nocase_index = 23
    'array_string_nocase   ', &         ! array_string_nocase_index    = 24
    'array_logical         ', &         ! array_logical_index          = 25
    'array_class           ', &         ! array_class_index            = 26
    'array_integer_between ', &         ! array_integer_between_index  = 27
    'array_real_between    ', &         ! array_real_between_index     = 28
    'array_integer_greater ', &         ! array_integer_greater_index  = 29
    'array_real_greater    ', &         ! array_real_greater_index     = 30
    'array_integer_less    ', &         ! array_integer_less_index     = 31
    'array_real_less       ', &         ! array_real_less_index        = 32
    'matrix_real           ', &         ! matrix_real_index            = 33
    'matrix_integer        ', &         ! matrix_integer_index         = 34
    'matrix_class          ', &         ! matrix_class_index           = 35
    'files                 ' ]          ! files_index                  = 36

!> The character and string matching strategies
  integer, parameter :: xfunit_assertion_character_match_exact = 0
  integer, parameter :: xfunit_assertion_character_match_global = 1
  integer, parameter :: xfunit_assertion_character_match_regexp = 2
  integer, parameter :: xfunit_assertion_string_match_exact = xfunit_assertion_character_match_exact
  integer, parameter :: xfunit_assertion_string_match_global = xfunit_assertion_character_match_global
  integer, parameter :: xfunit_assertion_string_match_regexp = xfunit_assertion_character_match_regexp

!> Assertion status enumeration
  integer, parameter :: xfunit_assertion_is_none = 0
  integer, parameter :: xfunit_assertion_is_pass = 1
  integer, parameter :: xfunit_assertion_is_fail = 2

!> Assertion status descriptors
  character(len=4), dimension(0:2), parameter :: xfunit_assertion_status = &
    [ 'none', 'pass', 'fail' ]

!> Scale factor for default real comparison (multiplies corresponding epsilon)
  real, parameter :: xfunit_real_scale = 100.0

!> The assertion general parent type
  type, extends(t_object) :: t_xfunit_assertion
    private

!>     Assertion mame
      type(t_string) :: name

!>     Assertion type
      integer :: type = xfunit_assertion_none_index

!>     Assertion status
      integer :: status = xfunit_assertion_is_none

!>     Error message
      type(t_msg) :: msg

    contains

!>     Getters and setters
      procedure :: get_name => xfunit_assertion_get_name
      procedure :: set_name => xfunit_assertion_set_name
      procedure :: get_type => xfunit_assertion_get_type
      procedure :: set_type => xfunit_assertion_set_type
      procedure :: get_status => xfunit_assertion_get_status
      procedure :: set_status => xfunit_assertion_set_status

!>     Interface (deferred) defintion for XML serialization
      procedure :: write_xml_start_tag => write_xfunit_assertion_start_tag
      procedure :: write_xml => xfunit_assertion_write_xml
      procedure, nopass :: write_xml_end_tag => write_xfunit_assertion_end_tag

!>     Interface (deferred) defintion for text serialization
      procedure :: write_header => write_xfunit_assertion_header
      procedure :: write => xfunit_assertion_write
      procedure, nopass :: write_footer => write_xfunit_assertion_footer

!>     Assertion verification
      procedure :: is_passed => xfunit_assertion_is_passed

!     Assignment
      generic :: assignment(=) => xfunit_assertion_assign
      procedure :: xfunit_assertion_assign

!     Operator interfaces (to allow inclusion in dyanmic containers)
      generic :: operator(==) => xfunit_assertion_equals
      procedure, private :: xfunit_assertion_equals
      generic :: operator(>)  => xfunit_assertion_greater
      procedure, private :: xfunit_assertion_greater
      generic :: operator(<)  => xfunit_assertion_less
      procedure, private :: xfunit_assertion_less

!>     Error handling
      procedure :: on_error => xfunit_assertion_on_error
      procedure :: get_error => xfunit_assertion_get_error
      procedure :: set_error => xfunit_assertion_set_error

  end type t_xfunit_assertion

!---End of declaration of module variables--------------------------------------

contains

!> General constructor
pure function xfunit_assertion( name, type, status ) result(res)

!> The assertion name
  character(len=*), intent(in) :: name

!> The assertion type
  integer, intent(in) :: type

!> The assertion evaluation status
  integer, intent(in) :: status

!> The assertion
  type(t_xfunit_assertion) :: res

! Store the assertion information
  res%name = name
  res%type = type
  res%status = status

end function xfunit_assertion


!> Serialize in XML (default in the base class)
subroutine xfunit_assertion_write_xml( this, xml )

!> The assertion
  class(t_xfunit_assertion), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Serialize empty tag
  attr(1) = xml_attribute( 'name', this%name )
  attr(2) = xml_attribute( 'type', xfunit_assertion_name(this%type) )
  call xml%write_terminal( 'assertion', attr=attr )

end subroutine xfunit_assertion_write_xml


!> Serialize in plain text (default in the base class)
subroutine xfunit_assertion_write( this, unit )

!> The assertion
  class(t_xfunit_assertion), intent(in) :: this

!> The open fortran unit
  integer, intent(in) :: unit

! Write default message
  write(unit,'(a,a,"(",a,")")') 'Not implemented for assertion: ', &
                                this%name%character(), &
                                xfunit_assertion_name(this%type)

end subroutine xfunit_assertion_write


!> Serialize in XML the start common section of the assertion
subroutine write_xfunit_assertion_start_tag( this, xml )

!> The assertion
  class(t_xfunit_assertion), intent(in) :: this

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Serialize start tag
  attr(1) = xml_attribute( 'name', this%name )
  attr(2) = xml_attribute( 'type', xfunit_assertion_name(this%type) )
  call xml%write_start_tag( 'assertion', attr=attr, newline=.true. )
  call xml%write_terminal( 'status', trim(xfunit_assertion_status(this%status)) )

end subroutine write_xfunit_assertion_start_tag


!> Serialize in XML the end common section of the assertion
subroutine write_xfunit_assertion_end_tag( xml )

!> The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Serialize end tag
  call xml%write_end_tag( 'assertion' )

end subroutine write_xfunit_assertion_end_tag


!> Serialize in plain text the start common section of the assertion
subroutine write_xfunit_assertion_header( this, unit )

!> The assertion
  class(t_xfunit_assertion), intent(in) :: this

!> The fortran open unit
  integer, intent(in) :: unit

! Local variables
  type(t_xml_encoder) :: encoder
  character(len=:), allocatable :: text

! Initialise encoder
  encoder = xml_encoder()

! Serialize header
  text = encoder%encode( this%name%character() )
  write( unit, '(a,1x,a)', advance='no' ) 'Assertion name:', trim(text)
  write( unit, '(3x,a,1x,a)', advance='no' ) 'type:', xfunit_assertion_name(this%type)
  write( unit, '(3x,a,1x,a)' ) 'status:', xfunit_assertion_status(this%status)

end subroutine write_xfunit_assertion_header


!> Serialize in plain text the end common section of the assertion
subroutine write_xfunit_assertion_footer( unit )

!> The fortran open unit
  integer, intent(in) :: unit

! Serialize footer
  write( unit, '(a)' ) repeat('-',78)

end subroutine write_xfunit_assertion_footer


!> Check assertion pass status
elemental function xfunit_assertion_is_passed( this ) result(res)

!> The assertion
  class(t_xfunit_assertion), intent(in) :: this

!> The assertion pass status
  logical :: res

! Get the pass status
  res = ( this%status == xfunit_assertion_is_pass )

end function xfunit_assertion_is_passed


!> Assignment
elemental subroutine xfunit_assertion_assign( this, other )

!> The target assertion
  class(t_xfunit_assertion), intent(inout) :: this

!> The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Assign elements
  this%name = other%name
  this%type = other%type
  this%status = other%status
  this%msg = other%msg

end subroutine xfunit_assertion_assign


! Operators (to allow inclusion in dyanmic containers)

!> Equality operator
elemental function xfunit_assertion_equals( left, right ) result(res)

!> Left operand
  class(t_xfunit_assertion), intent(in) :: left

!> Right operand
  class(t_xfunit_assertion), intent(in) :: right

!> Operation result
  logical :: res

! Compute operation result
  res = (left%type == right%type)

end function xfunit_assertion_equals


!> Less than operator
elemental function xfunit_assertion_less( left, right ) result(res)

!> Left operand
  class(t_xfunit_assertion), intent(in) :: left

!> Right operand
  class(t_xfunit_assertion), intent(in) :: right

!> Operation result
  logical :: res

! Compute operation result
  res = (left%type < right%type)

end function xfunit_assertion_less

!> Greater than operator
elemental function xfunit_assertion_greater( left, right ) result(res)

!> Left operand
  class(t_xfunit_assertion), intent(in) :: left

!> Right operand
  class(t_xfunit_assertion), intent(in) :: right

!> Operation result
  logical :: res

! Compute operation result
  res = (left%type > right%type)

end function xfunit_assertion_greater


!> Error handling check
pure function xfunit_assertion_on_error( this ) result(res)

!> Calling object
  class(t_xfunit_assertion), intent(in) :: this

!> Error status
  logical :: res

! Get error status
  res = (this%msg%get_code() /= 0)

end function xfunit_assertion_on_error


!> Getter for error structure
pure function xfunit_assertion_get_error( this ) result(res)

!> Calling object
  class(t_xfunit_assertion), intent(in) :: this

!> Error structure
  type(t_msg) :: res

! Return the error structure
  res = this%msg

end function xfunit_assertion_get_error

!> Setter for error structure
pure subroutine xfunit_assertion_set_error( this, msg )

!> Calling object
  class(t_xfunit_assertion), intent(inout) :: this

!> Error structure
  type(t_msg), intent(in) :: msg

! Set the error structure
  this%msg = msg

end subroutine xfunit_assertion_set_error


!> Getter for name
pure function xfunit_assertion_get_name( this ) result(res)

!> Calling object
  class(t_xfunit_assertion), intent(in) :: this

!> Return value
  type(t_string) :: res

! Set the return value
  res = this%name

end function xfunit_assertion_get_name


!> Setter for name
elemental subroutine xfunit_assertion_set_name( this, name )

!> Calling object
  class(t_xfunit_assertion), intent(inout) :: this

!> Value to set
  type(t_string), intent(in) :: name

! Store the input value
  this%name = name

end subroutine xfunit_assertion_set_name


!> Getter for type
elemental function xfunit_assertion_get_type( this ) result(res)

!> Calling object
  class(t_xfunit_assertion), intent(in) :: this

!> Set the return value
  integer :: res

! Set the return type
  res = this%type

end function xfunit_assertion_get_type


!> Setter for type
elemental subroutine xfunit_assertion_set_type( this, type )

!> Calling object
  class(t_xfunit_assertion), intent(inout) :: this

!> Value to set
  integer, intent(in) :: type

! Store the input value
  this%type = type

end subroutine xfunit_assertion_set_type


!> Getter for status
elemental function xfunit_assertion_get_status( this ) result(res)

!> Calling object
  class(t_xfunit_assertion), intent(in) :: this

!> Set the return value
  integer :: res

! Set the return value
  res = this%status

end function xfunit_assertion_get_status


!> Setter for status
elemental subroutine xfunit_assertion_set_status( this, status )

!> Calling object
  class(t_xfunit_assertion), intent(inout) :: this

!> Value to set
  integer, intent(in) :: status

! Stre the input value
  this%status = status

end subroutine xfunit_assertion_set_status

end module m_xfunit_assertion


