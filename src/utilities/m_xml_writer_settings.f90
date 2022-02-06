module m_xml_writer_settings

! -----------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : XML serialization settings
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
! -----------------------------------------------------------------------------

! Dependencies ----------------------------------------------------------------

  use m_object

  use m_xml_encoder

  implicit none

! Public/Private declarations -------------------------------------------------

  private
  public t_xml_writer_settings, xml_writer_settings

  public xml_writer_settings_default_indent_chars

! Module declarations ---------------------------------------------------------

! Default intentation chars
  character(len=*), parameter :: xml_writer_settings_default_indent_chars = '    '

! XML serialization settings
  type, extends(t_object) :: t_xml_writer_settings
    private

!     Flag to activate indentation
      logical :: indent = .false.

!     Characters used for indentation (for valid XML use valid blank characters only)
      character(len=:), allocatable :: indent_chars

!     Defines the method to encode reserved XML characters (default to name from xml_encoder enumeration)
      integer :: xml_encoding = xml_encode_name

!     Flag indicating whether to omit an XML declaration
      logical :: omit_xml_declaration = .false.

!     Generate individual records
      logical :: lines = .true.

    contains

!     Getters/setters
      procedure :: get_indent => xml_writer_settings_get_indent
      procedure :: set_indent => xml_writer_settings_set_indent
      procedure :: get_indent_chars => xml_writer_settings_get_indent_chars
      procedure :: set_indent_chars => xml_writer_settings_set_indent_chars
      procedure :: get_xml_encoding => xml_writer_settings_get_xml_encoding
      procedure :: set_xml_encoding => xml_writer_settings_set_xml_encoding
      procedure :: get_omit_xml_declaration => xml_writer_settings_get_omit_xml_declaration
      procedure :: set_omit_xml_declaration => xml_writer_settings_set_omit_xml_declaration
      procedure :: get_lines => xml_writer_settings_get_lines
      procedure :: set_lines => xml_writer_settings_set_lines

  end type t_xml_writer_settings

! Constructor interface
  interface xml_writer_settings
    module procedure xml_writer_settings_general
  end interface xml_writer_settings

! Implementation --------------------------------------------------------------

contains

! General constructor
pure function xml_writer_settings_general( indent, indent_chars, xml_encoding, omit_xml_declaration, lines ) result(res)

! Flag to activate indentation
  logical, optional, intent(in) :: indent

! Characters used for indentation (for valid XML use valid black characters only)
  character(len=*), optional, intent(in) :: indent_chars

! Defines the method to encode reserved XML characters (default to name)
  integer, optional, intent(in) :: xml_encoding

! Flag indicating whether to omit an XML declaration
  logical, optional, intent(in) :: omit_xml_declaration

! Flag to generate individual records
  logical, optional, intent(in) :: lines

! Returned object
  type(t_xml_writer_settings) :: res

! Flag to activate indentation
  if( present(indent) ) then
    res%indent = indent
  end if

! Characters used for indentation (for valid XML use valid black characters only)
  if( present(indent_chars) ) then
    res%indent_chars = indent_chars
  else
    res%indent_chars = xml_writer_settings_default_indent_chars
  end if

! Defines the method to encode reserved XML characters (default to name)
  if( present(xml_encoding) ) then
    res%xml_encoding = xml_encoding
  end if

! Flag indicating whether to omit an XML declaration
  if( present(omit_xml_declaration) ) then
    res%omit_xml_declaration = omit_xml_declaration
  end if

! Characters to use as line breaks during serialization
  if( present(lines) ) then
    res%lines = lines
  end if

end function xml_writer_settings_general


! Getter for indent
pure function xml_writer_settings_get_indent( this ) result(res)

! Calling object
  class(t_xml_writer_settings), intent(in) :: this

! Getter result
  logical :: res

! Return value
  res = this%indent

end function xml_writer_settings_get_indent


! Getter for indent_chars
pure function xml_writer_settings_get_indent_chars( this ) result(res)

! Calling object
  class(t_xml_writer_settings), intent(in) :: this

! Getter result
  character(len=:), allocatable :: res

! Return value
  res = this%indent_chars

end function xml_writer_settings_get_indent_chars


! Getter for xml_encoding
pure function xml_writer_settings_get_xml_encoding( this ) result(res)

! Calling object
  class(t_xml_writer_settings), intent(in) :: this

! Getter result
  integer :: res

! Return value
  res = this%xml_encoding

end function xml_writer_settings_get_xml_encoding


! Getter for omit_xml_declaration
pure function xml_writer_settings_get_omit_xml_declaration( this ) result(res)

! Calling object
  class(t_xml_writer_settings), intent(in) :: this

! Getter result
  logical :: res

! Return value
  res = this%omit_xml_declaration

end function xml_writer_settings_get_omit_xml_declaration


! Getter for newline_chars
pure function xml_writer_settings_get_lines( this ) result(res)

! Calling object
  class(t_xml_writer_settings), intent(in) :: this

! Getter result
  logical :: res

! Return value
  res = this%lines

end function xml_writer_settings_get_lines


! Setter for indent
pure subroutine xml_writer_settings_set_indent( this, value )

! Calling object
  class(t_xml_writer_settings), intent(inout) :: this

! Value to set
  logical, intent(in) :: value

! Set the value
  this%indent = value

end subroutine xml_writer_settings_set_indent


! Setter for indent_chars
pure subroutine xml_writer_settings_set_indent_chars( this, value )

! Calling object
  class(t_xml_writer_settings), intent(inout) :: this

! Value to set
  character(len=*), intent(in) :: value

! Set the value
  this%indent_chars = value

end subroutine xml_writer_settings_set_indent_chars


! Setter for xml_encoding
pure subroutine xml_writer_settings_set_xml_encoding( this, value )

! Calling object
  class(t_xml_writer_settings), intent(inout) :: this

! Value to set
  integer, intent(in) :: value

! Set the value
  this%xml_encoding = value

end subroutine xml_writer_settings_set_xml_encoding


! Setter for omit_xml_declaration
pure subroutine xml_writer_settings_set_omit_xml_declaration( this, value )

! Calling object
  class(t_xml_writer_settings), intent(inout) :: this

! Value to set
  logical, intent(in) :: value

! Set the value
  this%omit_xml_declaration = value

end subroutine xml_writer_settings_set_omit_xml_declaration


! Setter for newline_chars
pure subroutine xml_writer_settings_set_lines( this, value )

! Calling object
  class(t_xml_writer_settings), intent(inout) :: this

! Value to set
  logical, intent(in) :: value

! Set the value
  this%lines = value

end subroutine xml_writer_settings_set_lines

end module m_xml_writer_settings
