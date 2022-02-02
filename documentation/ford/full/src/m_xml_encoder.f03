module m_xml_encoder

!------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>XML stream encoder
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
  public t_xml_encoder
  public xml_encoder

  public xml_encode_none
  public xml_encode_name
  public xml_encode_decimal
  public xml_encode_hexadecimal

  public xml_default_encoder

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

!> Encoding type
  integer, parameter :: xml_encode_none        = 0
  integer, parameter :: xml_encode_name        = 1
  integer, parameter :: xml_encode_decimal     = 2
  integer, parameter :: xml_encode_hexadecimal = 3

!> Characters for the encoding
  character, parameter :: xml_coding_lead  = '&'
  character, parameter :: xml_coding_trail = ';'
  character, parameter :: xml_coding_dec_prefix = '#'
  character(len=2), parameter :: xml_coding_hex_prefix = '#x'

!> The encoding table
  type, extends(t_object) :: t_xml_coding
    private
    character(len=2) :: dec  = ''
    character(len=2) :: hex  = ''
    character        :: char = ''
    character(len=4) :: name = ''
  end type t_xml_coding
  type(t_xml_coding), parameter, dimension(*) :: xml_coding = (/ &
    t_xml_coding( '34', '22', '&', 'amp ' ), &
    t_xml_coding( '38', '26', '"', 'quot' ), &
    t_xml_coding( '39', '27', "'", 'apos' ), &
    t_xml_coding( '60', '3C', '<', 'lt  ' ), &
    t_xml_coding( '62', '3E', '>', 'gt  ' ) /)

!> The encoding implementation
  type t_xml_code
    private
    character        :: char = ''
    character(len=8) :: code = ''
  end type t_xml_code


!> Attirbute type (associated to an XML tag)
  type, extends(t_object) :: t_xml_encoder
    private

!>     Encode type
      integer :: type = xml_encode_name

!>     The code mappings
      type(t_xml_code), dimension(size(xml_coding)) :: code = t_xml_code()

    contains

!     Encoding/decoding interfaces
      generic :: encode => xml_encoder_encode_string, &
                           xml_encoder_encode_character
      procedure, private :: xml_encoder_encode_string
      procedure, private :: xml_encoder_encode_character
      generic :: decode => xml_encoder_decode_string, &
                           xml_encoder_decode_character
      procedure, private :: xml_encoder_decode_string
      procedure, private :: xml_encoder_decode_character

  end type t_xml_encoder

!> Constructor interface
  interface xml_encoder
    module procedure xml_encoder_default
    module procedure xml_encoder_type
  end interface xml_encoder

!> Name encoder
  type(t_xml_code), dimension(5), parameter :: xml_code_name = (/ &
    t_xml_code( xml_coding(1)%char, xml_coding_lead//trim(xml_coding(1)%name)//xml_coding_trail ), &
    t_xml_code( xml_coding(2)%char, xml_coding_lead//trim(xml_coding(2)%name)//xml_coding_trail ), &
    t_xml_code( xml_coding(3)%char, xml_coding_lead//trim(xml_coding(3)%name)//xml_coding_trail ), &
    t_xml_code( xml_coding(4)%char, xml_coding_lead//trim(xml_coding(4)%name)//xml_coding_trail ), &
    t_xml_code( xml_coding(5)%char, xml_coding_lead//trim(xml_coding(5)%name)//xml_coding_trail ) /)

!> Decimal encoder
  type(t_xml_code), dimension(5), parameter :: xml_code_decimal = (/ &
    t_xml_code( xml_coding(1)%char, xml_coding_lead//xml_coding_dec_prefix//trim(xml_coding(1)%dec)//xml_coding_trail ), &
    t_xml_code( xml_coding(2)%char, xml_coding_lead//xml_coding_dec_prefix//trim(xml_coding(2)%dec)//xml_coding_trail ), &
    t_xml_code( xml_coding(3)%char, xml_coding_lead//xml_coding_dec_prefix//trim(xml_coding(3)%dec)//xml_coding_trail ), &
    t_xml_code( xml_coding(4)%char, xml_coding_lead//xml_coding_dec_prefix//trim(xml_coding(4)%dec)//xml_coding_trail ), &
    t_xml_code( xml_coding(5)%char, xml_coding_lead//xml_coding_dec_prefix//trim(xml_coding(5)%dec)//xml_coding_trail ) /)

!> HExadecimal encoder
  type(t_xml_code), dimension(5), parameter :: xml_code_hexadecimal = (/ &
    t_xml_code( xml_coding(1)%char, xml_coding_lead//xml_coding_hex_prefix//trim(xml_coding(1)%hex)//xml_coding_trail ), &
    t_xml_code( xml_coding(2)%char, xml_coding_lead//xml_coding_hex_prefix//trim(xml_coding(2)%hex)//xml_coding_trail ), &
    t_xml_code( xml_coding(3)%char, xml_coding_lead//xml_coding_hex_prefix//trim(xml_coding(3)%hex)//xml_coding_trail ), &
    t_xml_code( xml_coding(4)%char, xml_coding_lead//xml_coding_hex_prefix//trim(xml_coding(4)%hex)//xml_coding_trail ), &
    t_xml_code( xml_coding(5)%char, xml_coding_lead//xml_coding_hex_prefix//trim(xml_coding(5)%hex)//xml_coding_trail ) /)

!> Default encoder
  type(t_xml_encoder), parameter :: xml_default_encoder = t_xml_encoder( xml_encode_name, xml_code_name )

!- End of module variable declarations ----------------------------------------

contains

!> Constructor
elemental function xml_encoder_default () result(res)

! The returned encoder
  type(t_xml_encoder) :: res

! Construct with default type
  res = xml_default_encoder

end function xml_encoder_default


!> Constructor
elemental function xml_encoder_type( type ) result(res)

!> The encoding type
  integer, intent(in) :: type

!> The returned encoder
  type(t_xml_encoder) :: res

! Initialise the coders
  res%type = type

! Check the encoding to be applied
  select case( res%type )

!   No encoding
    case( xml_encode_none )

!   Name encoding
    case( xml_encode_name )
      res%code = xml_code_name

!   Decimal encoding
    case( xml_encode_decimal )
      res%code = xml_code_decimal

!   Hexadecimal encoding
    case( xml_encode_hexadecimal )
      res%code = xml_code_hexadecimal

  end select

end function xml_encoder_type


!> Encode a character string to be XML compliant
elemental function xml_encoder_encode_string( this, str ) result(res)

!> The encoder
  class(t_xml_encoder), intent(in) :: this

!> The input character string
  type(t_string), intent(in) :: str

!> The encoded string
  type(t_string) :: res

! Encode for all characters
  res = this%xml_encoder_encode_character( str%character() )

end function xml_encoder_encode_string


!> Encode a character string to be XML compliant
pure function xml_encoder_encode_character( this, str ) result(res)

!> The encoder
  class(t_xml_encoder), intent(in) :: this

!> The input character string
  character(len=*), intent(in) :: str

!> The encoded string
  character(len=:), allocatable :: res

! Local variables
  integer :: i

! Initialise
  res = str

! Check is encoding enabled
  if( this%type /= xml_encode_none ) then

!   Encode for all characters
    do i = 1, size(this%code)
      res = xml_encode_character( res, this%code(i) )
    end do

  end if

end function xml_encoder_encode_character


!> Encode one character
recursive pure function xml_encode_character( str, code ) result(res)

!> The input character string
  character(len=*), intent(in) :: str

!> The character to encode
  type(t_xml_code), intent(in) :: code

!> The encoded string
  character(len=:), allocatable :: res

! Local variables
  integer :: idx

! Find the character in the input string
  idx = index( str, code%char )
  if( idx > 0 ) then

!   Replace the character by its encoding
    res = str(1:idx-1) // &
          trim(code%code) // &
          xml_encode_character( str(idx+1:), code )

  else

!   No more to encode; exit reursion
    res = str

  end if

end function xml_encode_character


!> Decode a character string from XML compliant
elemental function xml_encoder_decode_string( this, str ) result(res)

!> The encoder
  class(t_xml_encoder), intent(in) :: this

!> The input character string
  type(t_string), intent(in) :: str

!> The decoded string
  type(t_string) :: res

! Decode for all characters
  res = this%xml_encoder_decode_character( str%character() )

end function xml_encoder_decode_string


!> Decode a character string from XML compliant
pure function xml_encoder_decode_character( this, str ) result(res)

!> The encoder
  class(t_xml_encoder), intent(in) :: this

!> The input character string
  character(len=*), intent(in) :: str

!> The decoded string
  character(len=:), allocatable :: res

! Local variables
  integer :: i

! Initialise
  res = str

! Check is encoding enabled
  if( this%type /= xml_encode_none ) then

!   Decode for all characters
    do i = 1, size(this%code)
      res = xml_decode_character( res, this%code(i) )
    end do

  end if

end function xml_encoder_decode_character


!> Decode one encoded string
recursive pure function xml_decode_character( str, code ) result(res)

!> The input character string
  character(len=*), intent(in) :: str

!> The character to decode
  type(t_xml_code), intent(in) :: code

!> The decoded string
  character(len=:), allocatable :: res

! Local variables
  integer :: idx, codelen

! Initialise
  codelen = len_trim(code%code)

! Find the encoded string in the input string
  idx = index( str, trim(code%code) )
  if( idx > 0 ) then

!   Recusrively replace the encoded string by its character
    res = str(1:idx-1) // &
          code%char // &
          xml_decode_character( str(idx+codelen:), code )

  else

!   No more to encode; exit recursion
    res = str

  end if

end function xml_decode_character

end module m_xml_encoder
