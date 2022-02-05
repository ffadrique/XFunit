module m_messages

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Message handling
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
!------------------------------------------------------------------------------

!- Start of use statements ----------------------------------------------------

  use m_object
  use m_string
  use m_msg

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_messages
  public messages

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

! Default aray sizes
  integer, parameter :: messages_errors_size = 20
  integer, parameter :: messages_warnings_size = 20

! The list of messages
  type, extends(t_object) :: t_messages
    private

!     Default unit to write messages to
      integer :: unit = -1

!     Current error stattus of the messages
      logical :: is_error = .false.

!     Error countes
      integer :: error_count = 0

!     Buffer of error messages
      type(t_msg), dimension(messages_errors_size) :: errors

!     Warning counte
      integer :: warning_count = 0

!     Buffer of warning messages
      type(t_msg), dimension(messages_warnings_size) :: warnings

    contains

!     Report error
      generic :: error => messages_error, &
                          messages_error_raw
      procedure, private :: messages_error
      procedure, private :: messages_error_raw

!     Report warning
      procedure :: warning => messages_warning

!     Check error status
      procedure :: on_error => messages_on_error

!     Reset error condition
      procedure :: reset_error => messages_reset_error

!     Reset warning condition
      procedure :: reset_warning => messages_reset_warning

!     Dump the errors to the selected fortran unit
      procedure :: dump_errors => messages_dump_errors

!     Dump the warnings to the selected fortran unit
      procedure :: dump_warnings => messages_dump_warnings

!     Get the errors as an array of messages
      procedure :: get_errors => messages_get_errors

!     Get the warnings as an array of messages
      procedure :: get_warnings => messages_get_warnings

  end type t_messages

! Constructor interface
  interface messages
    module procedure messages_default
  end interface messages

!- End of module variable declarations ----------------------------------------

contains

! Constructor
elemental function messages_default() result(res)

! Constructed structure
  type(t_messages) :: res


end function messages_default


! Report error
elemental subroutine messages_error( this, msg )

! The message handling structure
  class(t_messages), optional, intent(inout) :: this

! Input message
  type(t_msg), intent(in) :: msg

! Check if working with error list
  if( present(this) ) then

!   Check maximum buffer size
    if( this%error_count < messages_errors_size ) then

!     Add message to the list
      this%error_count = this%error_count + 1
      this%errors(this%error_count) = msg

!     Set the error flag
      this%is_error = .true.

    end if

  end if

end subroutine messages_error


! Report error from raw elements
elemental subroutine messages_error_raw( this, mod, proc, code, text )

! The message handling structure
  class(t_messages), optional, intent(inout) :: this

! The module of calling method
  character(len=*), intent(in) :: mod

! The calling method
  character(len=*), intent(in) :: proc

! The error code (whitin method)
  integer, intent(in) :: code

! The message text
  character(len=*), intent(in) :: text

! Local message
  type(t_msg) :: amsg

! Check if working with error list
  if( present(this) ) then

!   Check maximum buffer size
    if( this%error_count < messages_errors_size ) then

!     Set the message structure
      amsg = msg( code, string(text), string(mod), string(proc) )

!     Add message to the list
      this%error_count = this%error_count + 1
      this%errors(this%error_count) = amsg

!     Set the error flag
      this%is_error = .true.

    end if

  end if

end subroutine messages_error_raw


! Report warning
elemental subroutine messages_warning( this, mod, proc, code, text )

! The message handling structure
  class(t_messages), optional, intent(inout) :: this

! The module of calling method
  character(len=*), intent(in) :: mod

! The calling method
  character(len=*), intent(in) :: proc

! The error code (whitin method)
  integer, intent(in) :: code

! The message text
  character(len=*), intent(in) :: text

! Local message
  type(t_msg) :: amsg

! Check if working with error list
  if( present(this) ) then

!   Check maximum buffer size
    if( this%warning_count < messages_warnings_size ) then

!     Set the message structure
      amsg = msg( code, string(text), string(mod), string(proc) )

!     Add message to the list
      this%warning_count = this%warning_count + 1
      this%warnings(this%warning_count) = Amsg

    end if

  end if

end subroutine messages_warning


! Check error status
elemental function messages_on_error ( this ) result(error)

! The error messages structure
! Allow optional for cases when error checking is optional
  class(t_messages), optional, intent(in) :: this

! The error condition
  logical :: error

! Return the error status
  if( present(this) ) then
    error = this%is_error
  else
    error = .false.
  end if

end function messages_on_error


! Reset error condition
pure subroutine messages_reset_error( this )

! The error messages structure
  class(t_messages), optional, intent(inout) :: this

! Remove the error stack
  if( present(this) ) then
    this%errors = msg()
    this%error_count = 0
    this%is_error = .false.
  end if

end subroutine messages_reset_error


! Reset warning condition
pure subroutine messages_reset_warning( this )

! The error messages structure
  class(t_messages), optional, intent(inout) :: this

! Remove the error stack
  if( present(this) ) then
    this%warnings = msg()
    this%warning_count = 0
  end if

end subroutine messages_reset_warning


! Dump the errors to the selected fortran unit
subroutine messages_dump_errors( this, unit )

! The array of messages
  class(t_messages), optional, intent(in) :: this

! The output fortran unit
  integer, intent(in) :: unit

! Dump the error messages (if not empty)
  if( present(this) ) then
    call dump_messages( this%errors, this%error_count, unit, 'ERROR' )
  end if

end subroutine messages_dump_errors


! Dump the warnings to the selected fortran unit
subroutine messages_dump_warnings( this, unit )

! The array of messages
  class(t_messages), optional, intent(in) :: this

! The output fortran unit
  integer, intent(in) :: unit

! Dump the warning messages
  if( present(this) ) then
    call dump_messages( this%warnings, this%warning_count, unit, 'WARNING' )
  end if

end subroutine messages_dump_warnings


! Dump a message array to the selected fortran unit
subroutine dump_messages( buffer, count, unit, category )

! The buffer of messages
  type(t_msg), dimension(:), intent(in) :: buffer

! Number of messages in the buffer to dump
  integer, intent(in) :: count

! The output fortran unit
  integer, intent(in) :: unit

! The error category (ERROR, WARNING, INFO)
  character(len=*), intent(in) :: category

! Local variables
  integer :: imsg

! Loop on the messages starting on the last one
  do imsg = count, 1, -1

!   Write the error message
    call buffer(imsg)%write( unit, category )

  end do

end subroutine dump_messages


! Get the errors as an array of messages
pure subroutine messages_get_errors( this, array )

! The messages structure
  class(t_messages), intent(in) :: this

! The array of messages
  type(t_msg), allocatable, dimension(:), intent(out) :: array

! Get the error messages
  call messages_get_messages( this%errors, this%error_count, array )

end subroutine messages_get_errors


! Get the warnings as an array of messages
pure subroutine messages_get_warnings( this, array )

! The messages structure
  class(t_messages), intent(in) :: this

! The array of messages
  type(t_msg), allocatable, dimension(:), intent(out) :: array

! Get the warning messages
  call messages_get_messages( this%warnings, this%warning_count, array )

end subroutine messages_get_warnings


! Get message array to the selected fortran unit
pure subroutine messages_get_messages( buffer, count, array )

! The array of messages (input)
  type(t_msg), dimension(:), intent(in) :: buffer

! The number of messages to get
  integer, intent(in) :: count

! The array of messages (output)
  type(t_msg), allocatable, dimension(:), intent(out) :: array

! Check the number of messages
  if( count > 0 ) then

!   Allocate the return array
    allocate( array, source=buffer(:count) )

  end if

end subroutine messages_get_messages

end module m_messages
