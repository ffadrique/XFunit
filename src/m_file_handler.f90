module m_file_handler

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Module for the implementation of file handler
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

  use, intrinsic :: iso_fortran_env

  use m_object
  use m_string

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_file_handler
  public file_handler

  public file_handler_unix_eol, file_handler_dos_eol, file_handler_mac_eol
  public file_handler_eol

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

! Carriage control (EOL) symbols
  integer, parameter :: file_handler_unix_eol = 1
  integer, parameter :: file_handler_dos_eol  = 2
  integer, parameter :: file_handler_mac_eol  = 3
  character(len=2), parameter, dimension(3) :: file_handler_eol = (/ char(10)//' ', char(13) // char(10), char(13)//' ' /)

! The file handler class
  type, extends(t_object) :: t_file_handler
    private

!     File path
      type(t_string) :: file_path

!     Type of end of line to use
      integer :: eol = file_handler_unix_eol

!     Fortran unit used by the handler
      integer :: unit = -1

!     Current handler unit access status
      integer :: iostat = 0

    contains

!     Access functions
      generic :: get_unit => file_handler_get_unit
      procedure, private :: file_handler_get_unit
      generic :: get_iostat => file_handler_get_iostat
      procedure, private :: file_handler_get_iostat
      generic :: get_file_path => file_handler_get_file_path
      procedure, private :: file_handler_get_file_path

!     Check if file exists
      procedure :: exists => file_handler_exists

!     Open the file
      procedure :: open => file_handler_open

!     Flush the contents of the buffer to file
      procedure :: flush => file_handler_flush

!     Rewind the file reading
      procedure :: rewind => file_handler_rewind

!     Close the file
      procedure :: close => file_handler_close

!     Check the open status
      procedure :: is_open => file_handler_is_open

!     Load the file contents to string
      procedure :: to_string => file_handler_to_string

!     Assignment operator
      generic :: assignment(=) => file_handler_assign
      procedure, private :: file_handler_assign

  end type t_file_handler

! Constructor interface
  interface file_handler
    module procedure file_handler_default
    module procedure file_handler_string
  end interface file_handler

!- End of module variable declarations ----------------------------------------

contains

! Constructor
elemental function file_handler_default( file, eol ) result(handler)

! The file name
  character(len=*), intent(in) :: file

! The record separator (defaults to unis=lf)
  integer, optional, intent(in) :: eol

! The file hander
  type(t_file_handler) :: handler

! Normalise the path
  handler%file_path = string( file )

! Initialise the file unit
  handler%unit = -1

! Set the EOL symbol
  if( present(eol) ) then
    handler%eol = eol
  else
    handler%eol = file_handler_unix_eol
  end if

end function file_handler_default


! Constructor
elemental function file_handler_string( path, eol ) result(handler)

! The file name
  type(t_string), intent(in) :: path

! The record separator (defaults to unis=lf)
  integer, optional, intent(in) :: eol

! The file hander
  type(t_file_handler) :: handler

! Invoke the character interface
  handler = file_handler_default( path%character(), eol )

end function file_handler_string


! Assignment operator
elemental subroutine file_handler_assign( left, right )

! The left file handler
  class(t_file_handler), intent(inout) :: left

! The left file handler
  type(t_file_handler), intent(in) :: right

! Implement the assignment
  left%file_path = right%file_path
  left%eol = right%eol
  left%unit = right%unit
  left%iostat = right%iostat

end subroutine file_handler_assign


! Check if file in handler exists
function file_handler_exists( this ) result(res)

! The file handler
  class(t_file_handler), intent(inout) :: this

! The status flag
  logical :: res

! Check if the file exists
  inquire( file=trim(this%file_path%character()), exist=res )

end function file_handler_exists


! Open the file
subroutine file_handler_open( this, write, binary, append, stream, recl, unit )

! The file handler
  class(t_file_handler), intent(inout) :: this

! The flag to open for writing (.false. by default)
  logical, optional, intent(in) :: write

! The flag to open a binary file (.false. by default)
  logical, optional, intent(in) :: binary

! The flag to open a file for append (.false. by default)
  logical, optional, intent(in) :: append

! Open the file as a stream
  logical, optional, intent(in) :: stream

! Record length for direct access
  integer, optional, intent(in) :: recl

! The file unit (optional; forced by user)
  integer, optional, intent(in) :: unit

! Local variables
  integer :: lun
  character(len=32) :: action, form, access, status
  type(t_string) :: local
  integer :: ios

! Get a free unit number
! IF given by user, take it without further checks
  if( present(unit) ) then
    lun = unit
  else
    lun = free_lun()
  end if
  if( lun >= 0 ) then

!   Select read/write
    if( present(write) ) then
      if( write ) then
        action = 'write'
        status = 'unknown'
      else
        action = 'read'
        status = 'old'
      end if
    else
      action = 'read'
      status = 'old'
    end if

!   Select binary
    if( present(binary) ) then
      if( binary ) then
        form = 'unformatted'
      else
        form = 'formatted'
      end if
    else
      form = 'formatted'
    end if

!   Select access
    if( present(recl) ) then
      access = 'direct'
    else if( present(append) ) then
      if( append ) then
        access = 'append'
      else
        access = 'sequential'
      end if
    else if( present(stream) ) then
      if( stream ) then
        access = 'stream'
        if( present(binary) ) then
          if( binary ) then
            form = 'unformatted'
          else
            form = 'formatted'
          end if
        else
          form = 'unformatted'
        end if
      else
        access = 'sequential'
      end if
    else
      access = 'sequential'
    end if

!   Open the file
    local = this%file_path%character()
    if( present(recl) ) then
      open( lun, &
            file=trim(local%character()), &
            status=trim(status), &
            action=trim(action), &
            form=trim(form), &
            access=trim(access), &
            recl=recl, &
            iostat=ios )
    else if( access == 'stream' ) then
      open( lun, &
            file=trim(local%character()), &
            status=trim(status), &
            action=trim(action), &
            form=trim(form), &
            access=trim(access), &
            iostat=ios )
    else
      open( lun, &
            file=trim(local%character()), &
            status=trim(status), &
            action=trim(action), &
            form=trim(form), &
            access=trim(access), &
            iostat=ios )
    end if

!   Check open status
    if( ios == 0 ) then
      this%unit = lun
      this%iostat = 0
    else
      this%unit = -1
      this%iostat = ios
    end if

  end if

end subroutine file_handler_open


! Flush the file
subroutine file_handler_flush( this )

! The file handler
  class(t_file_handler), intent(in) :: this

! Flush the file
  flush( this%unit )

end subroutine file_handler_flush


! Rewind the file
subroutine file_handler_rewind( this )

! The file handler
  class(t_file_handler), intent(in) :: this

! Rewind the file
  rewind( this%unit )

end subroutine file_handler_rewind


! Close the file
subroutine file_handler_close( this, delete )

! The file handler
  class(t_file_handler), intent(inout) :: this

! The flag to delete the file
  logical, optional, intent(in) :: delete

! Local variables
  type(t_string) :: status

! Set the close status
  status = 'keep'
  if( present(delete) ) then
    if( delete ) then
      status = 'delete'
    end if
  end if

! Close the file
  close( this%unit, status=status%character() )

! Reset the file unit
  this%unit = -1

end subroutine file_handler_close


! Verify if the file is open
elemental function file_handler_is_open( this ) result(res)

! The file handler
  class(t_file_handler), intent(in) :: this

! The file open status (.true. if open)
  logical :: res

! Return the file open status
  res = ( this%unit >= 0 )

end function file_handler_is_open


! Get the file path
elemental function file_handler_get_file_path( this ) result(res)

! The file handler
  class(t_file_handler), intent(in) :: this

! The file path
  type(t_string) :: res

! Return the file path
  res = this%file_path

end function file_handler_get_file_path


! Get the file unit
elemental function file_handler_get_unit( this ) result(res)

! The file handler
  class(t_file_handler), intent(in) :: this

! The file unit
  integer :: res

! Return the unit
  res = this%unit

end function file_handler_get_unit


! Get the file open iostat
elemental function file_handler_get_iostat( this ) result(res)

! The file handler
  class(t_file_handler), intent(in) :: this

! The iostat
  integer :: res

! Return the iostat
  res = this%iostat

end function file_handler_get_iostat


! Convert a file to a character string
function file_handler_to_string( this, eol, binary ) result(res)

! The file handler
  class(t_file_handler), intent(in) :: this

! The flag to select separation of record with carriage control symbol
  logical, optional, intent(in) :: eol

! Implement a binary comparison
  logical, optional, intent(in) :: binary

! The resulting character string
  type(t_string) :: res

! Local variables
  character(len=2) :: c_eol
  integer :: ios
  character(len=8192) :: rec
  type(t_file_handler) :: local
  logical :: bin

! Initialise
  ios = 0
  local = this
  res = ''
  rec = ''

! Set the EOL characters
  if( present(eol) ) then
    c_eol =  file_handler_eol(local%eol)
  else
    c_eol = ''
  end if

! Check for binary access
  if( present(binary) ) then
    bin = binary
  else
    bin = .false.
  end if

! Open the file
  if( bin ) then
    call local%open( stream=.true., binary=.true. )
  else
    call local%open()
  end if
  if( local%is_open() ) then

!   Loop reading records
    do

!     Read next record
      if( bin ) then
        read( local%unit, iostat=ios ) rec
      else
        read( local%unit, '(a8192)', iostat=ios ) rec
      end if
      if( ios < 0 ) then

!       End of file; exit
        exit

      else if( ios > 0 ) then

!       Error found; empty the resulting string and exit
        res = ''
        exit

      else

!       Append record with carraiage control
        res = res // trim(rec) // trim(c_eol)

      end if

    end do

!   Close the file
    call local%close()

  end if

end function file_handler_to_string


! Find a free unit to open a file
function free_lun() result(res)

! The free unit
  integer :: res

! Local variables
  integer :: unit, ios
  logical :: is_open

! Loop looking for available units
  do unit = 10, 99
    inquire( unit, opened=is_open, iostat=ios )
    if( ios /= 0 ) then
      exit
    else if( .not. is_open ) then
      res = unit
      exit
    end if
  end do

end function free_lun

end module m_file_handler
