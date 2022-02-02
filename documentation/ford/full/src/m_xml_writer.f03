module m_xml_writer

!------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>XML stream writer
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

  use, intrinsic :: iso_fortran_env

  use m_util_convert
  use m_object
  use m_string
  use m_messages
  use m_file_handler

  use m_xml_attribute
  use m_xml_writer_settings
  use m_xml_encoder

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_xml_writer
  public xml_writer

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

!> Buffer parameters
  integer, parameter :: xml_buffer_size = 4096

!> XML type for parser handling
  type, extends(t_object) :: t_xml_writer
    private

!>     Writing unit (must be initialised in constructor)
      integer :: unit = -1

!>     XML serialization settings
      type(t_xml_writer_settings) :: settings

!>     Encoder instance
      type(t_xml_encoder) :: encoder

!>     Flag to activate indentation
      logical :: indent = .false.

!>     Indentation level
      integer :: indent_level = 0

!>     Split into lines
      logical :: lines = .true.

!>     Indentation characters (acutal indent, not the same as settings)
      character(len=:), allocatable :: indent_chars

!>     Flag to generate independent records
      character(len=3) :: advance = 'yes'

    contains

!     Assignment operator
      generic :: Assignment(=) => xml_writer_assign
      procedure, private :: xml_writer_assign

!>     Access function interfaces
      procedure :: get_unit => xml_writer_get_unit

!     Interface for writing of terminal XML tags
      generic :: write_terminal => xml_writer_write_empty_terminal_character, &
                                   xml_writer_write_empty_terminal_string, &
                                   xml_writer_write_character_terminal_character, &
                                   xml_writer_write_character_terminal_string, &
                                   xml_writer_write_string_terminal_character, &
                                   xml_writer_write_string_terminal_string, &
                                   xml_writer_write_int1_terminal_character, &
                                   xml_writer_write_int1_terminal_string, &
                                   xml_writer_write_int2_terminal_character, &
                                   xml_writer_write_int2_terminal_string, &
                                   xml_writer_write_integer_terminal_character, &
                                   xml_writer_write_integer_terminal_string, &
                                   xml_writer_write_int8_terminal_character, &
                                   xml_writer_write_int8_terminal_string, &
                                   xml_writer_write_real_terminal_character, &
                                   xml_writer_write_real_terminal_string, &
                                   xml_writer_write_double_terminal_character, &
                                   xml_writer_write_double_terminal_string, &
                                   xml_writer_write_complex_terminal_character, &
                                   xml_writer_write_complex_terminal_string, &
                                   xml_writer_write_double_complex_terminal_character, &
                                   xml_writer_write_double_complex_terminal_string, &
                                   xml_writer_write_logical1_terminal_character, &
                                   xml_writer_write_logical1_terminal_string, &
                                   xml_writer_write_logical2_terminal_character, &
                                   xml_writer_write_logical2_terminal_string, &
                                   xml_writer_write_logical_terminal_character, &
                                   xml_writer_write_logical_terminal_string
      procedure, private :: xml_writer_write_empty_terminal_character
      procedure, private :: xml_writer_write_empty_terminal_string
      procedure, private :: xml_writer_write_character_terminal_character
      procedure, private :: xml_writer_write_character_terminal_string
      procedure, private :: xml_writer_write_string_terminal_character
      procedure, private :: xml_writer_write_string_terminal_string
      procedure, private :: xml_writer_write_int1_terminal_character
      procedure, private :: xml_writer_write_int1_terminal_string
      procedure, private :: xml_writer_write_int2_terminal_character
      procedure, private :: xml_writer_write_int2_terminal_string
      procedure, private :: xml_writer_write_integer_terminal_character
      procedure, private :: xml_writer_write_integer_terminal_string
      procedure, private :: xml_writer_write_int8_terminal_character
      procedure, private :: xml_writer_write_int8_terminal_string
      procedure, private :: xml_writer_write_real_terminal_character
      procedure, private :: xml_writer_write_real_terminal_string
      procedure, private :: xml_writer_write_double_terminal_character
      procedure, private :: xml_writer_write_double_terminal_string
      procedure, private :: xml_writer_write_complex_terminal_character
      procedure, private :: xml_writer_write_complex_terminal_string
      procedure, private :: xml_writer_write_double_complex_terminal_character
      procedure, private :: xml_writer_write_double_complex_terminal_string
      procedure, private :: xml_writer_write_logical1_terminal_character
      procedure, private :: xml_writer_write_logical1_terminal_string
      procedure, private :: xml_writer_write_logical2_terminal_character
      procedure, private :: xml_writer_write_logical2_terminal_string
      procedure, private :: xml_writer_write_logical_terminal_character
      procedure, private :: xml_writer_write_logical_terminal_string

!     Interfaces for writing start tag
      generic :: write_start_tag => xml_writer_write_start_tag_character, &
                                    xml_writer_write_start_tag_string
      procedure, private :: xml_writer_write_start_tag_character
      procedure, private :: xml_writer_write_start_tag_string

!     Interfaces for writing end tag
      generic :: write_end_tag => xml_writer_write_end_tag_character, &
                                  xml_writer_write_end_tag_string
      procedure, private :: xml_writer_write_end_tag_character
      procedure, private :: xml_writer_write_end_tag_string

!>     Serialize a list of attributes
      procedure, private :: xml_writer_attributes_to_string

!     Write the XML first record with version number
      generic :: write_version => xml_writer_write_version_default, &
                                  xml_writer_write_version_character, &
                                  xml_writer_write_version_string
      procedure, private :: xml_writer_write_version_default
      procedure, private :: xml_writer_write_version_character
      procedure, private :: xml_writer_write_version_string

!     Write the XML first record with version number
      generic :: write_stylesheet => xml_writer_write_stylesheet_character, &
                                     xml_writer_write_stylesheet_string
      procedure, private :: xml_writer_write_stylesheet_character
      procedure, private :: xml_writer_write_stylesheet_string

!     Write the XML root start tag
      generic :: write_root_start_tag => xml_writer_write_root_start_tag_character, &
                                         xml_writer_write_root_start_tag_string
      procedure, private :: xml_writer_write_root_start_tag_character
      procedure, private :: xml_writer_write_root_start_tag_string

!     Write the XML root end tag
      generic :: write_root_end_tag => xml_writer_write_root_end_tag_character, &
                                       xml_writer_write_root_end_tag_string
      procedure, private :: xml_writer_write_root_end_tag_character
      procedure, private :: xml_writer_write_root_end_tag_string

!     Write a terminal XML meta tag (enclosed in <? ?>)
      generic :: write_processing_instruction => xml_writer_write_processing_instruction_character, &
                                                 xml_writer_write_processing_instruction_string
      procedure, private :: xml_writer_write_processing_instruction_character
      procedure, private :: xml_writer_write_processing_instruction_string

!     Write a terminal XML comment (enclosed in <!-- -->)
      generic :: write_comment => xml_writer_write_comment_character, &
                                  xml_writer_write_comment_string
      procedure, private :: xml_writer_write_comment_character
      procedure, private :: xml_writer_write_comment_string

!>     Increase the indentation level
      procedure, private :: indent_increase => xml_writer_indent_increase

!>     Decrease the indentation level
      procedure, private :: indent_decrease => xml_writer_indent_decrease

!>     Get/Set indent level
      procedure :: get_indent_level => xml_writer_get_indent_level
      procedure :: set_indent_level => xml_writer_set_indent_level
      
  end type t_xml_writer

!> Constructor interface
  interface xml_writer
    module procedure xml_writer_default
    module procedure xml_writer_unit
    module procedure xml_writer_fhandler
  end interface xml_writer

!- End of module variable declarations ----------------------------------------

contains

!> Constructor from fortran unit
elemental function xml_writer_default() result(res)

! The XML document parsing structure
  type(t_xml_writer) :: res

! Set the default indentation chars (from settings)
  res%indent = .false.
  res%indent_chars = ''

! Initialise identation counter
  res%indent_level = 0

end function xml_writer_default


!> Constructor from fortran unit
elemental function xml_writer_unit( unit, settings ) result(res)

!> The open fortran unit to read from
  integer, intent(in) :: unit

!> The XML serialization settings
  type(t_xml_writer_settings), optional, intent(in) :: settings

!> The XML document parsing structure
  type(t_xml_writer) :: res

! Set the default values
  res%indent_level = 0

! Initialise the Fortran unit
  res%unit = unit

! Store settings
  if( present(settings) ) then
    res%settings = settings
  else
    res%settings = xml_writer_settings()
  end if

! Initialise the encoder
  res%encoder = xml_encoder( res%settings%get_xml_encoding() )

! Initialise indentation variables
  res%lines = res%settings%get_lines()
  res%indent = res%settings%get_indent()
  res%indent_chars = ''

! Initialise line advance
  if( res%settings%get_lines() ) then
    res%advance = 'yes'
  else
    res%advance = 'no'
  end if

! Initialise identation counter
  res%indent_level = 0

end function xml_writer_unit


!> Constructor from file handler
elemental function xml_writer_fhandler( fhandler, settings ) result(res)

!> The file handler
  type(t_file_handler), intent(in) :: fhandler

!> The XML serialization settings
  type(t_xml_writer_settings), optional, intent(in) :: settings

!> The XML document parsing structure
  type(t_xml_writer) :: res

! Invoke the interface with the fortran unit
  res = xml_writer( fhandler%get_unit(), settings )

end function xml_writer_fhandler


!> Convert attributes to attributes list
pure function xml_writer_attributes_to_string( this, attr ) result(res)

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> List of attributes
  type(t_xml_attribute), dimension(:), intent(in) :: attr

!> The serialized attributes
  character(len=:), allocatable :: res

! Local variables
  integer :: i
  character(len=:), allocatable :: attrstr

! Initialise
  res = attr(1)%to_string(this%encoder)

! Process attributes
  do i = 2, size(attr)

!   Add attribute
    attrstr = attr(i)%to_string(this%encoder)
    res = res//' '//trim(attrstr)

  end do

end function xml_writer_attributes_to_string


!> Write the XML first record with version number
subroutine xml_writer_write_version_default( this )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Check if the declaration is to be output
  if( .not. this%settings%get_omit_xml_declaration() ) then

!   Select the version
    attr(1) = xml_attribute( 'version', '1.0' )

!   Write the XML version record
    call this%write_processing_instruction( 'xml', attr=attr )

  end if

end subroutine xml_writer_write_version_default


!> Write the XML first record with version number
subroutine xml_writer_write_version_character( this, version, encoding )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML version as character string (default to 1.0)
  character(len=*), intent(in) :: version

!> The XML encoding (default to ISO-8859-1)
  character(len=*), intent(in) :: encoding

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Check if the declaration is to be output
  if( .not. this%settings%get_omit_xml_declaration() ) then

!   Select the version
    attr(1) = xml_attribute( 'version', version )

!   Select the encoding
    attr(2) = xml_attribute( 'encoding', encoding )

!   Write the XML version record
    call this%write_processing_instruction( 'xml', attr=attr )

  end if

end subroutine xml_writer_write_version_character


!> Write the XML first record with version number
subroutine xml_writer_write_version_string( this, version, encoding )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML version as character string (default to 1.0)
  type(t_string), intent(in) :: version

!> The XML encoding (default to ISO-8859-1)
  type(t_string), intent(in) :: encoding

! Call the character interface
  call this%write_version( version%character(), encoding%character() )

end subroutine xml_writer_write_version_string


!> Write the XML first record with version number
subroutine xml_writer_write_stylesheet_character( this, xsl )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> Stylesheet name as character string
  character(len=*), intent(in) :: xsl

! Local variables
  type(t_xml_attribute), dimension(2) :: attr

! Select the stylesheet type
  attr(1) = xml_attribute( 'type', 'text/xsl' )

! Select the stylesheet URL
  attr(2) = xml_attribute( 'href', trim(xsl) )

! Write the XML version record
  call this%write_processing_instruction( 'xml-stylesheet', attr=attr )

end subroutine xml_writer_write_stylesheet_character


!> Write the XML first record with version number
subroutine xml_writer_write_stylesheet_string( this, xsl )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> Stylesheet name as character string
  type(t_string), intent(in) :: xsl

! Call the character interface
  call this%write_stylesheet( xsl%character() )

end subroutine xml_writer_write_stylesheet_string


!> Write the XML root element
subroutine xml_writer_write_root_start_tag_character( this, tag, xmlns, schema, attr )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> XML root tag
  character(len=*), intent(in) :: tag

!> XML namespace
  character(len=*), optional, intent(in) :: xmlns

!> XML schema
  character(len=*), optional, intent(in) :: schema

!> Other attributes
  type(t_xml_attribute), optional, dimension(:) :: attr

! Local variables
  integer idx
  type(t_xml_attribute), dimension(:), allocatable :: local_attr

! intel-bug
! gfortran-bug
! There are some funnies with compilers.
! These two variables are needed to store locally the input information
! before passing it to the XML attribute assginment routines.
! Hopefully this will disappear in the future but for the moment ...
  type(t_xml_attribute) :: a
  character(len=128) :: c

! Initialise the local attribute array
  if( present(attr) ) then
    idx = size(attr)
    allocate(local_attr(idx+2))
    local_attr(:idx) = attr
  else
    idx = 0
    allocate(local_attr(3))
  end if

! Check XML namespace presence
  call a%set_name( 'xmlns:xsi' )
  if( present(xmlns) ) then
    c = xmlns
    call a%set_value( c )
  else
    call a%set_value( 'http://www.w3.org/2001/XMLSchema-instance' )
  end if
  local_attr(idx+1) = a

! Check for XML schema presence
  if( present(schema) ) then
    c = schema
    call a%set_name( 'xsi:noNamespaceSchemaLocation' )
    call a%set_value( c )
    local_attr(idx+2) = a
  else
    local_attr(idx+2) = xml_attribute( '', '' )
  end if

! Write the tag
  call this%write_start_tag( tag, local_attr )

end subroutine xml_writer_write_root_start_tag_character


!> Write the XML root element
subroutine xml_writer_write_root_start_tag_string( this, tag, xmlns, schema, attr )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> XML root tag
  type(t_string), intent(in) :: tag

!> XML namespace
  type(t_string), optional, intent(in) :: xmlns

!> XML schema
  type(t_string), optional, intent(in) :: schema

!> Other attributes
  type(t_xml_attribute), optional, dimension(:) :: attr

! Call the character interface
  call this%write_root_start_tag( tag%character(), xmlns%character(), schema%character(), attr )

end subroutine xml_writer_write_root_start_tag_string


!> Write the XML root element
subroutine xml_writer_write_root_end_tag_character( this, tag )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> XML root tag
  character(len=*), intent(in) :: tag

! Write the end tag
  call this%write_end_tag( tag )

! Check if the lie end terminator must be added.
! Fortran requires at least one line control to flush the buffer
  if( this%advance == 'no' ) then
    write( this%unit, '(a)' ) ''
  end if

end subroutine xml_writer_write_root_end_tag_character


!> Write the XML root element
subroutine xml_writer_write_root_end_tag_string( this, tag )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> XML root tag
  type(t_string), intent(in) :: tag

! Write the end tag
  call this%write_root_end_tag( tag%character() )

end subroutine xml_writer_write_root_end_tag_string


!> Write a terminal XML meta tag (enclosed in <? ?>)
subroutine xml_writer_write_processing_instruction_character( this, tag, attr )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), intent(in) :: attr

! Local variables
  integer :: i
  type(t_string) :: local

! Initialise the tag
  write( this%unit, '(a,a)', advance='no' ) '<?', trim(adjustl(tag))

! Write with attributes
  do i = lbound(attr,1), ubound(attr,1)

!   Process attributes that have a name
    if( attr(i)%get_name() /= '' ) then

!     Write the attribute
      local = attr(i)%to_string(this%encoder)
      write( this%unit, '(1x,a)', advance='no' ) local%character()

    end if

  end do

! Complete the tag
  write( this%unit, '(a)', advance=this%advance ) '?>'

end subroutine xml_writer_write_processing_instruction_character


!> Write a terminal XML meta tag (enclosed in <? ?>)
subroutine xml_writer_write_processing_instruction_string( this, tag, attr )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), intent(in) :: attr

! Call the character interface
  call this%write_processing_instruction( tag%character(), attr )

end subroutine xml_writer_write_processing_instruction_string


!> Write a terminal XML comment (enclosed in <!-- -->)
subroutine xml_writer_write_comment_character( this, text )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The comment text
  character(len=*), intent(in) :: text

! Write the indentation and stay in the line
  if( this%indent .and. this%lines ) then
    write( this%unit, '(a)', advance='no' ) this%indent_chars
  end if

! Write the comment
  write( this%unit,'(a,1x,a,1x,a)', advance=this%advance ) '<!--', trim(adjustl(text)), '-->'

end subroutine xml_writer_write_comment_character


!> Write a terminal XML comment (enclosed in <!-- -->)
subroutine xml_writer_write_comment_string( this, text )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The comment text
  type(t_string), intent(in) :: text

! Invoke the character interfacce
  call this%write_comment( text%character() )

end subroutine xml_writer_write_comment_string


!> Write a terminal XML tag with empty contents (character tag)
subroutine xml_writer_write_empty_terminal_character( this, tag, attr )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

! Write the indentation and stay in the line
  if( this%indent .and. this%lines ) then
    write( this%unit, '(a)', advance='no' ) this%indent_chars
  end if

! Write empty element
  if( present(attr) ) then
    write( this%unit, '(a,a,a,a,a)', advance=this%advance ) '<', trim(tag), ' ', &
                                                            this%xml_writer_attributes_to_string(attr), ' />'
  else
    write( this%unit, '(a,a,a)', advance=this%advance ) '<', trim(tag), ' />'
  end if

end subroutine xml_writer_write_empty_terminal_character


!> Write a terminal XML tag with empty contents (string tag)
subroutine xml_writer_write_empty_terminal_string( this, tag, attr )

!> The XML context structure
  class(t_xml_writer), intent(in) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

! Call the character interface
  call this%write_terminal( tag%character(), attr )

end subroutine xml_writer_write_empty_terminal_string


!> Write a terminal XML tag with character contents (character tag)
subroutine xml_writer_write_character_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  character(len=*), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Local variables
  type(t_string) :: local

! Check value
  if( len_trim(value) == 0 ) then

!   Write empy element
    call this%write_terminal( tag, attr )

  else

!   Write start tag
    call this%write_start_tag( tag, attr, .false. )

!   Encode value
    local = this%encoder%encode( value )

!   Write the value
    if( present(fmt) ) then
      write( this%unit, fmt, advance='no' ) local%character()
    else
      write( this%unit, '(a)', advance='no' ) local%character()
    end if

!   Write the end tag
    call this%write_end_tag( tag, .false. )

  end if

end subroutine xml_writer_write_character_terminal_character


!> Write a terminal XML tag with character contents (string tag)
subroutine xml_writer_write_character_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  character(len=*), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_character_terminal_string


!> Write a terminal XML tag with string contents (character tag)
subroutine xml_writer_write_string_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  type(t_string), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character version
  call this%write_terminal( tag, value%character(), attr, fmt )

end subroutine xml_writer_write_string_terminal_character


!> Write a terminal XML tag with string contents (string tag)
subroutine xml_writer_write_string_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  type(t_string), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value%character(), attr, fmt )

end subroutine xml_writer_write_string_terminal_string


!> Write a terminal XML tag with integer (kind=1) contents (character tag)
subroutine xml_writer_write_int1_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  integer(kind=1), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call default subroutine
  call this%write_terminal( tag, int(value,kind=4), attr, fmt )

end subroutine xml_writer_write_int1_terminal_character


!> Write a terminal XML tag with integer (kind=1) contents (string tag)
subroutine xml_writer_write_int1_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  integer(kind=1), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_int1_terminal_string


!> Write a terminal XML tag with integer (kind=2) contents (character tag)
subroutine xml_writer_write_int2_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  integer(kind=2), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call default subroutine
  call this%write_terminal( tag, int(value,kind=8), attr, fmt )

end subroutine xml_writer_write_int2_terminal_character


!> Write a terminal XML tag with integer (kind=2) contents (string tag)
subroutine xml_writer_write_int2_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  integer(kind=2), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_int2_terminal_string


!> Write a terminal XML tag with integer (default kind=4) contents (character tag)
subroutine xml_writer_write_integer_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  integer, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call default subroutine
  call this%write_terminal( tag, int(value,kind=8), attr, fmt )

end subroutine xml_writer_write_integer_terminal_character


!> Write a terminal XML tag with integer (default kind=4) contents (string tag)
subroutine xml_writer_write_integer_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  integer, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_integer_terminal_string


!> Write a terminal XML tag with integer (kind=2) contents (character tag)
subroutine xml_writer_write_int8_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  integer(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Local variables
  character(len=64) :: buffer

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the value
  if( present(fmt) ) then
    write( buffer, fmt ) value
  else
    write( buffer, '(i0)' ) value
  end if
  write( this%unit, '(a)', advance='no' ) trim(adjustl(buffer))

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_int8_terminal_character


!> Write a terminal XML tag with integer (kind=2) contents (string tag)
subroutine xml_writer_write_int8_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  integer(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_int8_terminal_string


!> Write a terminal XML tag with real contents (character tag)
subroutine xml_writer_write_real_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  real, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Local variables
  character(len=64) :: buffer

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the value
  if( present(fmt) .and. fmt /= '' ) then
    write( buffer, fmt ) value
  else
    write( buffer, '(a)' ) trim(character(value))
  end if
  write( this%unit, '(a)', advance='no' ) adjustl(trim(buffer))

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_real_terminal_character


!> Write a terminal XML tag with real contents (string tag)
subroutine xml_writer_write_real_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  real, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_real_terminal_string


!> Write a terminal XML tag with double precission contents (character tag)
subroutine xml_writer_write_double_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  real(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Local variables
  character(len=64) :: buffer

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the value
  if( present(fmt) .and. fmt /= '' ) then
    write( buffer, fmt ) value
  else
    write( buffer, '(a)' ) trim(character(value))
  end if
  write( this%unit, '(a)', advance='no' ) trim(adjustl(buffer))

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_double_terminal_character


!> Write a terminal XML tag with double precission contents (string tag)
subroutine xml_writer_write_double_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_String), intent(in) :: tag

!> The XML contents
  real(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_double_terminal_string


!> Write a terminal XML tag with complex contents (character tag)
subroutine xml_writer_write_complex_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  complex(kind=4), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Write start tag
  call this%write_start_tag( tag, attr )

! Write the value
  call this%write_terminal( 'r', real(value), fmt=fmt )
  call this%write_terminal( 'i', aimag(value), fmt=fmt )

! Write the end tag
  call this%write_end_tag( tag )

end subroutine xml_writer_write_complex_terminal_character


!> Write a terminal XML tag with complex contents (string tag)
subroutine xml_writer_write_complex_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  complex(kind=4), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_complex_terminal_string


!> Write a terminal XML tag with complex contents (character tag)
subroutine xml_writer_write_double_complex_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  complex(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Local variables
  real(kind=8) :: rx, ix

! Write start tag
  call this%write_start_tag( tag, attr )

! Write the value (no imag for real(kind=8) in 2003 standard)
  rx =  real(value,kind=8)
  ix = -real( cmplx(0d0,1d0,kind=8) * value, kind=8 )
  call this%write_terminal( 'r', rx, fmt=fmt )
  call this%write_terminal( 'i', ix, fmt=fmt )

! Write the end tag
  call this%write_end_tag( tag )

end subroutine xml_writer_write_double_complex_terminal_character


!> Write a terminal XML tag with complex contents (string tag)
subroutine xml_writer_write_double_complex_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  complex(kind=8), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_double_complex_terminal_string


!> Write a terminal XML tag with logical contents (character tag)
subroutine xml_writer_write_logical_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  logical, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the element
  write( this%unit, '(a)', advance='no' ) trim(character(value,fmt))

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_logical_terminal_character


!> Write a terminal XML tag with logical contents (string tag)
subroutine xml_writer_write_logical_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  logical, intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_logical_terminal_string


!> Write a terminal XML tag with logical contents (character tag)
subroutine xml_writer_write_logical2_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  logical(kind=2), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the value
  if( present(fmt) ) then
    write( this%unit, fmt, advance='no' ) value
  else
    write( this%unit, '(l1)', advance='no' ) value
  end if

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_logical2_terminal_character


!> Write a terminal XML tag with logical contents (string tag)
subroutine xml_writer_write_logical2_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  logical(kind=2), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_logical2_terminal_string


!> Write a terminal XML tag with logical contents (character tag)
subroutine xml_writer_write_logical1_terminal_character( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> The XML contents
  logical(kind=1), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Write start tag
  call this%write_start_tag( tag, attr, .false. )

! Write the value
  if( present(fmt) ) then
    write( this%unit, fmt, advance='no' ) value
  else
    write( this%unit, '(l1)', advance='no' ) value
  end if

! Write the end tag
  call this%write_end_tag( tag, .false. )

end subroutine xml_writer_write_logical1_terminal_character


!> Write a terminal XML tag with logical contents (string tag)
subroutine xml_writer_write_logical1_terminal_string( this, tag, value, attr, fmt )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> The XML contents
  logical(kind=1), intent(in) :: value

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optional output format
  character(len=*), optional, intent(in) :: fmt

! Call the character interface
  call this%write_terminal( tag%character(), value, attr, fmt )

end subroutine xml_writer_write_logical1_terminal_string


!> Write a start XML tag with/without attributes
subroutine xml_writer_write_start_tag_character( this, tag, attr, newline )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optionally add a new line at the end of the tag
  logical, optional, intent(in) :: newline

! Advance after the start tag
  character(len=3) :: advance

! Local variables
  integer :: i
  type(t_string) :: local

! Check the newline parameter
  if( present(newline) ) then
    if( newline ) then
      advance = 'yes'
    else
      advance = 'no'
    end if
  else
    advance = this%advance
  end if

! Write the indentation and stay in the line
  if( this%indent .and. this%lines ) then
    write( this%unit, '(a)', advance='no' ) this%indent_chars
  end if

! Check presence of attributes
  if( present(attr) ) then

!   Initialise the tag
    write( this%unit, '(a,a)', advance='no' ) '<', trim(adjustl(tag))

!   Loop on the attributes
    do i = lbound(attr,1), ubound(attr,1)

!     Process attributes that have a name
      if( attr(i)%get_name() /= '' ) then

!       Write the attribute
        local = attr(i)%to_string(this%encoder)
        write( this%unit, '(1x,a)', advance='no' ) local%character()

      end if

    end do

!   Complete the tag
    write( this%unit, '(a)', advance=advance ) '>'

  else

!   Write without attributes
    write( this%unit, '(a,a,a)', advance=advance ) '<', trim(adjustl(tag)), '>'

  end if

! Increase indentation
  call this%indent_increase()

end subroutine xml_writer_write_start_tag_character


!> Write a start XML tag with/without attributes
subroutine xml_writer_write_start_tag_string( this, tag, attr, newline )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> Optional attributes for the tag
  type(t_xml_attribute), dimension(:), optional, intent(in) :: attr

!> Optionally add a new line at the end of the tag
  logical, optional, intent(in) :: newline

! Call the character interface
  call this%write_start_tag( tag%character(), attr, newline )

end subroutine xml_writer_write_start_tag_string


!> Write an end XML tag
subroutine xml_writer_write_end_tag_character( this, tag, indent )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  character(len=*), intent(in) :: tag

!> Optionally add the indentation before the end tag
  logical, optional, intent(in) :: indent

! Keep the et tag in the line
  logical :: inline

! Check the newline parameter
  if( present(indent) ) then
    inline = indent
  else
    inline = .true.
  end if

! Decrease indentation
  call this%indent_decrease()

! Determine if this end tag is in line with its start tag
  if( inline ) then

!   Write the indentation and stay in the line
    if( this%indent .and. this%lines ) then
      write( this%unit, '(a)', advance='no' ) this%indent_chars
    end if

  end if

! Write end tag
  write( this%unit, '(a,a,a)', advance=this%advance ) '</', trim(adjustl(tag)), '>'

end subroutine xml_writer_write_end_tag_character


!> Write an end XML tag
subroutine xml_writer_write_end_tag_string( this, tag, indent )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML tag
  type(t_string), intent(in) :: tag

!> Optionally add the indentation before the end tag
  logical, optional, intent(in) :: indent

! Call the character interface
  call this%write_end_tag( tag%character(), indent )

end subroutine xml_writer_write_end_tag_string


!> Increase indentation level
pure subroutine xml_writer_indent_increase( this )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

! Increase the indentation level
  if( this%indent .and. this%lines ) then
    this%indent_level = this%indent_level + 1
    this%indent_chars = repeat( this%settings%get_indent_chars(), this%indent_level )
  end if

end subroutine xml_writer_indent_increase


!> Decrease indentation level
pure subroutine xml_writer_indent_decrease( this )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

! Decrease the indentation level
  if( this%indent .and. this%lines ) then
    this%indent_level = this%indent_level - 1
    this%indent_chars = repeat( this%settings%get_indent_chars(), this%indent_level )
  end if

end subroutine xml_writer_indent_decrease


!> Assignment operator
elemental subroutine xml_writer_assign( this, other )

!> The XML context structure
  class(t_xml_writer), intent(inout) :: this

!> The XML source structure
  type(t_xml_writer), intent(in) :: other

! Do the assignment
  this%unit = other%unit
  this%settings = other%settings
  this%encoder = other%encoder
  this%indent = other%indent
  this%indent_level = other%indent_level
  this%indent_chars = other%indent_chars
  this%advance = other%advance

end subroutine xml_writer_assign


!> Return the value of the unit attribute in the object
elemental function xml_writer_get_unit( this ) result(res)

!> The object
  class(t_xml_writer), intent(in) :: this

!> The returned object attribute
  integer :: res

! Set the return value
  res = this%unit

end function xml_writer_get_unit


!> Get indent level
pure function xml_writer_get_indent_level( this ) result(res)

!> The object
  class(t_xml_writer), intent(in) :: this

!> The indent level
  integer :: res
  
! Get the inent level
  res = this%indent_level
  
end function xml_writer_get_indent_level


!> Set indent level
pure subroutine xml_writer_set_indent_level( this, level )

!> The object
  class(t_xml_writer), intent(inout) :: this

!> The indent level
  integer, intent(in) :: level
  
! Set the indent level
  this%indent_level = level
  this%indent_chars = repeat( this%settings%get_indent_chars(), this%indent_level )
  
end subroutine xml_writer_set_indent_level

end module m_xml_writer
