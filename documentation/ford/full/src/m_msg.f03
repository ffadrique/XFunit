module m_msg

!------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Message container handling
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
!------------------------------------------------------------------------------

!- Start of use statements ----------------------------------------------------

  use m_object
  use m_string

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_msg
  public msg

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------


!> Message type
  type, extends(t_object) :: t_msg
    private

!>     Message code
      integer :: code = 0

!>     Message text
      type(t_string) :: text

!>     Module/type where the message is generated
      type(t_string) :: mod

!>     Procedure where the message is generated
      type(t_string) :: proc

    contains

!>     Write interfaces
      procedure :: write => msg_write

!     Operator interfaces (for soring based on message code)
      generic :: operator(<) => less
      procedure, private :: less
      generic :: operator(>) => greater
      procedure, private :: greater
      generic :: operator(==) => equal
      procedure, private :: equal

!     Assignment operator
      generic :: assignment(=) => msg_assing_msg
      procedure, private :: msg_assing_msg

!>     Generic get/set interfaces
      procedure :: get_code => msg_code_get
      procedure :: set_code => msg_code_set
      procedure :: get_text => msg_text_get
      procedure :: set_text => msg_text_set
      procedure :: get_module => msg_module_get
      procedure :: set_module => msg_module_set
      procedure :: get_procedure => msg_procedure_get
      procedure :: set_procedure => msg_procedure_set

  end type t_msg

!> Constructor interface
  interface msg
    module procedure msg_default
    module procedure msg_default_character
    module procedure msg_default_string
  end interface msg

!- End of module variable declarations ----------------------------------------

contains

!> Default constructor
elemental function msg_default() result(res)

! The message structure
  type(t_msg) :: res


end function msg_default


!> Constructor
elemental function msg_default_character( code, text, mod, proc ) result(res)

!> The message code
  integer, intent(in) :: code

!> Tte message text
  character(len=*), intent(in) :: text

!> The module name
  character(len=*), intent(in) :: mod

!> The procedure name
  character(len=*), intent(in) :: proc

!> The message structure
  type(t_msg) :: res

! Build the message
  res = msg( code, string(text), string(mod), string(proc) )

end function msg_default_character


!> Constructor
elemental function msg_default_string( code, text, mod, proc ) result(res)

!> The message code
  integer, intent(in) :: code

!> Tte message text
  type(t_string), intent(in) :: text

!> The module name
  type(t_string), intent(in) :: mod

!> The procedure name
  type(t_string), intent(in) :: proc

!> The message structure
  type(t_msg) :: res

! Build the message
  res%code = code
  res%text = text
  res%mod  = mod
  res%proc = proc

end function msg_default_string


!> Write an error structure to the selected open unit
subroutine msg_write( this, unit, type )

!> The message
  class(t_msg), intent(in) :: this

!> The open fortran unit
  integer, intent(in) :: unit

!> The error category
  character(len=*), optional, intent(in) :: type

! Local variables
  character(len=*), parameter :: short_fmt = "(a)"
  character(len=*), parameter :: long_fmt = "('*** ',a,1x,a,'::',a,' (',i6.6,') : ',a)"

! Write the message
  if( present(type) ) then
    write( unit, long_fmt ) trim(type), &
                            trim( this%mod%character() ), &
                            trim( this%proc%character() ), &
                            this%code, &
                            trim( this%text%character() )
  else

    write( unit, short_fmt ) trim( this%text%character() )

  end if

end subroutine msg_write


!> Comparison operator
elemental function less( this, right ) result(res)

!> The left element
  class(t_msg), intent(in) :: this

!> The right element
  type(t_msg), intent(in) :: right

!> The comparison result
  logical :: res

! Perform the comparison
  res = ( this%code < right%code )

end function less


!> Comparison operator
elemental function greater( this, right ) result(res)

!> The left element
  class(t_msg), intent(in) :: this

!> The right element
  type(t_msg), intent(in) :: right

!> The comparison result
  logical :: res

! Perform the comparison
  res = ( this%code > right%code )

end function greater


!> Comparison operator
elemental function equal( this, right ) result(res)

!> The left element
  class(t_msg), intent(in) :: this

!> The right element
  type(t_msg), intent(in) :: right

!> The comparison result
  logical :: res

! Perform the comparison
  res = ( this%code == right%code )

end function equal


!> Assignment operator
elemental subroutine msg_assing_msg( this, right )

!> The left element (intel accepts out; gfortran forces inout)
  class(t_msg), intent(inout) :: this

!> The right element
  class(t_msg), intent(in) :: right

! Do the assignment
  this%code = right%code
  this%text = right%text
  this%mod = right%mod
  this%proc = right%proc

end subroutine msg_assing_msg


!> -----------------------------------------------------------------------------
!> Getter for code
elemental function msg_code_get( this ) result(res)

!> Calling object
  class(t_msg), intent(in) :: this

!> Getter result
  integer :: res

! Return value
  res = this%code

end function msg_code_get

!> -----------------------------------------------------------------------------
!> Getter for text
elemental function msg_text_get( this ) result(res)

!> Calling object
  class(t_msg), intent(in) :: this

!> Getter result
  type(t_string) :: res

! Return value
  res = this%text

end function msg_text_get

!> -----------------------------------------------------------------------------
!> Getter for mod
elemental function msg_module_get( this ) result(res)

!> Calling object
  class(t_msg), intent(in) :: this

!> Getter result
  type(t_string) :: res

! Return value
  res = this%mod

end function msg_module_get

!> -----------------------------------------------------------------------------
!> Getter for proc
elemental function msg_procedure_get( this ) result(res)

!> Calling object
  class(t_msg), intent(in) :: this

!> Getter result
  type(t_string) :: res

! Return value
  res = this%proc

end function msg_procedure_get

!> -----------------------------------------------------------------------------
!> Setter for code
elemental subroutine msg_code_set( this, value )

!> Calling object
  class(t_msg), intent(inout) :: this

  integer, intent(in) :: value

! Set the value
  this%code = value

end subroutine msg_code_set

!> -----------------------------------------------------------------------------
!> Setter for text
elemental subroutine msg_text_set( this, value )

!> Calling object
  class(t_msg), intent(inout) :: this

  type(t_string), intent(in) :: value

! Set the value
  this%text = value

end subroutine msg_text_set

!> -----------------------------------------------------------------------------
!> Setter for mod
elemental subroutine msg_module_set( this, value )

!> Calling object
  class(t_msg), intent(inout) :: this

  type(t_string), intent(in) :: value

! Set the value
  this%mod = value

end subroutine msg_module_set

!> -----------------------------------------------------------------------------
!> Setter for proc
elemental subroutine msg_procedure_set( this, value )

!> Calling object
  class(t_msg), intent(inout) :: this

  type(t_string), intent(in) :: value

! Set the value
  this%proc = value

end subroutine msg_procedure_set


end module m_msg
