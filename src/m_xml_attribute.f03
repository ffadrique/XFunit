module m_xml_attribute

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : XML attribute handling
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

  use m_util_convert
  use m_object
  use m_string

  use m_xml_encoder

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_xml_attribute
  public xml_attribute

  public xml_attribute_value

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------


! Attirbute type (associated to an XML tag)
  type, extends(t_object) :: t_xml_attribute
    private

!     Attribute name
      type(t_string) :: name

!     Attribute type
      type(t_string) :: value

    contains

!     Assignment
      generic :: assignment(=) => xml_attribute_assign
      procedure, private :: xml_attribute_assign

!     Encoding/decoding interfaces
      procedure :: encode => xml_attribute_encode
      procedure :: decode => xml_attribute_decode

!     Serialization interface (convert to character string)
      procedure :: to_string => xml_attribute_to_string

!     Access function interfaces
      procedure :: get_name => xml_attribute_get_name
      generic :: set_name => xml_attribute_set_name, &
                             xml_attribute_set_name_character
      procedure, private :: xml_attribute_set_name
      procedure, private :: xml_attribute_set_name_character
      procedure :: get_value => xml_attribute_get_value
      generic :: set_value => xml_attribute_set_value, &
                              xml_attribute_set_value_character
      procedure, private :: xml_attribute_set_value
      procedure, private :: xml_attribute_set_value_character

  end type t_xml_attribute

! Constructor interface
  interface xml_attribute
    module procedure xml_attribute_default
    module procedure xml_attribute_from_char_and_char
    module procedure xml_attribute_from_char_and_string
    module procedure xml_attribute_from_char_and_int1
    module procedure xml_attribute_from_char_and_int2
    module procedure xml_attribute_from_char_and_int4
    module procedure xml_attribute_from_char_and_real
    module procedure xml_attribute_from_char_and_double
    module procedure xml_attribute_from_char_and_logical1
    module procedure xml_attribute_from_char_and_logical2
    module procedure xml_attribute_from_char_and_logical4
    module procedure xml_attribute_from_string_and_string
    module procedure xml_attribute_from_string_and_char
    module procedure xml_attribute_from_string_and_int1
    module procedure xml_attribute_from_string_and_int2
    module procedure xml_attribute_from_string_and_int4
    module procedure xml_attribute_from_string_and_real
    module procedure xml_attribute_from_string_and_double
    module procedure xml_attribute_from_string_and_logical1
    module procedure xml_attribute_from_string_and_logical2
    module procedure xml_attribute_from_string_and_logical4
  end interface xml_attribute

!- End of module variable declarations ----------------------------------------

contains

! Initialise an attribute from name (character) and value (character)
elemental function xml_attribute_default() result(res)

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = t_string()
  res%value = t_string()

end function xml_attribute_default


! Initialise an attribute from name (character) and value (character)
elemental function xml_attribute_from_char_and_char( name, value ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  character(len=*), intent(in) :: value

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = value

end function xml_attribute_from_char_and_char


! Initialise an attribute from name (character) and value (string)
elemental function xml_attribute_from_char_and_string( name, value ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  type(t_string), intent(in) :: value

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = value

end function xml_attribute_from_char_and_string


! Initialise an attribute from name (character) and value (integer kind=1)
elemental function xml_attribute_from_char_and_int1( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  integer(kind=1), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_int1


! Initialise an attribute from name (character) and value (integer kind=2)
elemental function xml_attribute_from_char_and_int2( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  integer(kind=2), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_int2


! Initialise an attribute from name (character) and value (integer kind=4)
elemental function xml_attribute_from_char_and_int4( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  integer(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_int4


! Initialise an attribute from name (character) and value (real kind=4)
elemental function xml_attribute_from_char_and_real( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  real(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = character(value,fmt=fmt)

end function xml_attribute_from_char_and_real


! Initialise an attribute from name (character) and value (real kind=8)
elemental function xml_attribute_from_char_and_double( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  real(kind=8), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_double


! Initialise an attribute from name (character) and value (logical kind=1)
elemental function xml_attribute_from_char_and_logical1( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  logical(kind=1), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_logical1


! Initialise an attribute from name (character) and value (logical kind=2)
elemental function xml_attribute_from_char_and_logical2( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  logical(kind=2), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_logical2


! Initialise an attribute from name (character) and value (logical kind=4)
elemental function xml_attribute_from_char_and_logical4( name, value, fmt ) result(res)

! The name
  character(len=*), intent(in) :: name

! The value
  logical(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_char_and_logical4


! Initialise an attribute from name (string)  and value (string)
elemental function xml_attribute_from_string_and_string( name, value ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  type(t_string), intent(in) :: value

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = value

end function xml_attribute_from_string_and_string


! Initialise an attribute from name (string)  and value (character)
elemental function xml_attribute_from_string_and_char( name, value ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  character(len=*), intent(in) :: value

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = value

end function xml_attribute_from_string_and_char


! Initialise an attribute from name (string) and value (integer kind=1)
elemental function xml_attribute_from_string_and_int1( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  integer(kind=1), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_int1


! Initialise an attribute from name (string) and value (integer kind=2)
elemental function xml_attribute_from_string_and_int2( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  integer(kind=2), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_int2


! Initialise an attribute from name (string) and value (integer kind=4)
elemental function xml_attribute_from_string_and_int4( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  integer(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_int4


! Initialise an attribute from name (string) and value (real kind=4)
elemental function xml_attribute_from_string_and_real( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  real(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_real


! Initialise an attribute from name (string) and value (real kind=8)
elemental function xml_attribute_from_string_and_double( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  real(kind=8), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_double


! Initialise an attribute from name (string) and value (logical kind=1)
elemental function xml_attribute_from_string_and_logical1( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  logical(kind=1), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_logical1


! Initialise an attribute from name (string) and value (logical kind=2)
elemental function xml_attribute_from_string_and_logical2( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  logical(kind=2), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_logical2


! Initialise an attribute from name (string) and value (logical kind=4)
elemental function xml_attribute_from_string_and_logical4( name, value, fmt ) result(res)

! The name
  type(t_string), intent(in) :: name

! The value
  logical(kind=4), intent(in) :: value

! Optional output format
  character(len=*), optional, intent(in) :: fmt

! The returned attribute
  type(t_xml_attribute) :: res

! Set the attribute
  res%name = name
  res%value = trim(character(value,fmt=fmt))

end function xml_attribute_from_string_and_logical4


! Assignment
elemental subroutine xml_attribute_assign( this, other )

! The calling object (maybe in with intel; gofrtran forces inout)
  class(t_xml_attribute), intent(inout) :: this

! The other object
  class(t_xml_attribute), intent(in) :: other

! Assign the elements
  this%name = other%name
  this%value = other%value

end subroutine xml_attribute_assign


! Encode the contents of an attribute
elemental function xml_attribute_encode( this, encoder ) result(res)

! The input attribute
  class(t_xml_attribute), intent(in) :: this

! The XML encoder
  type(t_xml_encoder), intent(in) :: encoder

! The decoded attribute
  type(t_xml_attribute) :: res

! Decode the attribute value
  res%name = this%name
  res%value = encoder%encode( this%value )

end function xml_attribute_encode


! Decode the contents of an attribute
elemental function xml_attribute_decode( this, encoder ) result(res)

! The input attribute
  class(t_xml_attribute), intent(in) :: this

! The XML encoder
  type(t_xml_encoder), intent(in) :: encoder

! The decoded attribute
  type(t_xml_attribute) :: res

! Decode the attribute value
  res%name = this%name
  res%value = encoder%decode( this%value )

end function xml_attribute_decode


! to_string an attribute (convert to string)
elemental function xml_attribute_to_string( this, encoder ) result(res)

! The input attribute
  class(t_xml_attribute), intent(in) :: this

! The XML encoder
  type(t_xml_encoder), optional, intent(in) :: encoder

! The to_stringd attribute
  type(t_string) :: res

! Local storage for encoding
  type(t_string) :: local

! Encode the attribute value
  if( present(encoder) ) then
    local = encoder%encode( this%value )
  else
    local = xml_default_encoder%encode(this%value)
  end if

! Form the to_stringd attribute
  local = local%adjustl()
  res = this%name%adjustl()
  res = res%trim() // '=' // '"' // local%trim() // '"'

end function xml_attribute_to_string


! Retrieve the attribute value from an array of attribute structures
pure function xml_attribute_value( attr, name ) result(res)

! The array of input attribute structures
  type(t_xml_attribute), dimension(:), intent(in) :: attr

! The attribute name to look for
  character(len=*), intent(in) :: name

! The returned attribute value (empty if not found)
  type(t_string) :: res

! Local variables
  integer :: i
  type(t_string) :: local

! Initialise return string
  res = ' '

! Look for the requested attribute
  do i = 1, size(attr)
    local = attr(i)%get_name()
    if( adjustl(name) == local%adjustl() ) then
      res = attr(i)%get_value()
      exit
    end if
  end do

end function xml_attribute_value


! Getter for name
elemental function xml_attribute_get_name( this ) result(res)

! Calling object
  class(t_xml_attribute), intent(in) :: this

! Getter result
  type(t_string) :: res

! Return value (protect from not initialised name)
  if( this%name%len_trim() > 0 ) then
    res = this%name
  else
    res = string('')
  end if

end function xml_attribute_get_name


! Getter for value
elemental function xml_attribute_get_value( this ) result(res)

! Calling object
  class(t_xml_attribute), intent(in) :: this

! Getter result
  type(t_string) :: res

! Return value (protect from not initialised name)
  if( this%value%len_trim() > 0 ) then
    res = this%value
  else
    res = string('')
  end if

end function xml_attribute_get_value


! Setter for name
elemental subroutine xml_attribute_set_name( this, value )

! Calling object
  class(t_xml_attribute), intent(inout) :: this

! Value to set
  type(t_string), intent(in) :: value

! Set the value
  this%name = value

end subroutine xml_attribute_set_name


! Setter for name (character)
elemental subroutine xml_attribute_set_name_character( this, value )

! Calling object
  class(t_xml_attribute), intent(inout) :: this

! Value to set
  character(len=*), intent(in) :: value

! Set the value
  this%name = string(value)

end subroutine xml_attribute_set_name_character


! Setter for value
elemental subroutine xml_attribute_set_value( this, value )

! Calling object
  class(t_xml_attribute), intent(inout) :: this

! Value to set
  type(t_string), intent(in) :: value

! Set the value
  this%value = value

end subroutine xml_attribute_set_value


! Setter for value (character)
elemental subroutine xml_attribute_set_value_character( this, value )

! Calling object
  class(t_xml_attribute), intent(inout) :: this

! Value to set
  character(len=*), intent(in) :: value

! Set the value
  this%value = string(value)

end subroutine xml_attribute_set_value_character

end module m_xml_attribute
