module m_util_convert

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Basic conversion functions and constants
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

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public character
  public double, real, integer, logical
  public complex, double_complex
  public hex
  public uppercase, lowercase
  public odd, even
  public character_to_bytes, bytes_to_character
  public character_to_array, array_to_character
  public bytes_to_bits_list, bits_list_to_bytes
  public bytes_to_hex, hex_to_bytes
  public split, join
  public change_endianness

!---End of public/private declarations------------------------------------------

  character(len=130), parameter, private :: sccs_info = &
    '$Id: $'

!---Declaration of module variables---------------------------------------------

! Offset to convert from uppercase to lowercase
  integer(kind=1), parameter :: index_AU = ichar('A', kind=1)
  integer(kind=1), parameter :: index_AL = ichar('a', kind=1)
  integer(kind=1), parameter :: index_ZU = ichar('Z', kind=1)
  integer(kind=1), parameter :: index_ZL = ichar('z', kind=1)
  integer(kind=1), parameter :: offset_U_to_L = index_AL - index_AU

! Generic conversion to character interface
  interface character
    module procedure integer1_to_character
    module procedure integer2_to_character
    module procedure integer_to_character
    module procedure integer8_to_character
    module procedure double_to_character
    module procedure real_to_character
    module procedure complex_to_character
    module procedure double_complex_to_character
    module procedure logical1_to_character
    module procedure logical2_to_character
    module procedure logical_to_character
  end interface character

! Generic interface from character to double
  interface double
    module procedure character_to_double
  end interface double

! Generic interface from character to real
  interface real
    module procedure character_to_real
  end interface real

! Generic interface from character to default integer
  interface integer
    module procedure character_to_integer
  end interface integer

! Generic interface from character to default complex
  interface complex
    module procedure character_to_complex
  end interface complex

! Generic interface from character to double complex
  interface double_complex
    module procedure character_to_double_complex
  end interface double_complex

! Generic interface from character to default logical
  interface logical
    module procedure character_to_logical
  end interface logical

! Generic interface from integer to hexadecimal string
  interface hex
    module procedure integer1_to_hex
    module procedure integer2_to_hex
    module procedure integer_to_hex
  end interface hex

! Interfaces for odd and even integer identification
  interface odd
    module procedure odd_integer1
    module procedure odd_integer2
    module procedure odd_integer
  end interface odd
  interface even
    module procedure even_integer1
    module procedure even_integer2
    module procedure even_integer
  end interface even

! Interfaces for character lowercase and uppercase
  interface lowercase
    module procedure character_lowercase
  end interface lowercase
  interface uppercase
    module procedure character_uppercase
  end interface uppercase

! Interface to split and join character strings
  interface split
    module procedure character_split
  end interface split
  interface join
    module procedure character_join
  end interface join

! Endiannes interface
  interface change_endianness
    module procedure change_endianness_integer2
    module procedure change_endianness_integer4
  end interface change_endianness

! Reference formats
  character(len=16), parameter :: sp_real_fmt = '(sp,g16.7e3)'
  character(len=16), parameter :: ss_real_fmt = '(ss,g16.7e3)'
  character(len=16), parameter :: sp_double_fmt = '(sp,g24.15e3)'
  character(len=16), parameter :: ss_double_fmt = '(ss,g24.15e3)'
  character(len=16), parameter :: sp_integer_fmt = '(sp,i0)'
  character(len=16), parameter :: ss_integer_fmt = '(ss,i0)'
  character(len=16), parameter :: sp_logical_fmt = '(sp,l1)'
  character(len=16), parameter :: ss_logical_fmt = '(ss,l1)'

! Sizes for some reference formated strings
  integer, parameter :: s_real_size = 16
  integer, parameter :: s_double_size = 24

!---End of declaration of module variables--------------------------------------

contains


! Convert a double complex to character string
elemental function complex_to_character( x, imaginary ) result(res)

! The double number
  complex(kind=4), intent(in) :: x

! The symbol for the imaginary unit (optional; default to 'i')
  character, optional, intent(in) :: imaginary

! The resulting string
  character(len=64) :: res

! Local variables
  real(kind=4) :: rx, ix
  character :: cunit

! Check imaginary unit symbol
  if( present(imaginary) ) then
    cunit = imaginary
  else
    cunit = 'i'
  end if

! Recover the complex parts (no imag for real(kind=8) in 2003 standard)
  rx =  real( x, kind=4 )
  ix = -real( cmplx(0.0_8, 1d0, kind=4) * x, kind=4 )

! Conver to string
  res = trim(real_to_character(rx))
  if( ix >= 0 ) then
    res = trim(res)//'+'//cunit
  else
    res = trim(res)//'-'//cunit
  end if
  res = trim(res)//trim(real_to_character(abs(ix)))

end function complex_to_character


! Convert a double complex to character string
elemental function double_complex_to_character( x, imaginary ) result(res)

! The double number
  complex(kind=8), intent(in) :: x

! The symbol for the imaginary unit (optional; default to 'i')
  character, optional, intent(in) :: imaginary

! The resulting string
  character(len=64) :: res

! Local variables
  real(kind=8) :: rx, ix
  character :: cunit

! Check imaginary unit symbol
  if( present(imaginary) ) then
    cunit = imaginary
  else
    cunit = 'i'
  end if

! Recover the complex parts (no imag for real(kind=8) in 2003 standard)
  rx =  real( x, kind=8 )
  ix = -real( cmplx(0.0_8, 1d0, kind=8) * x, kind=8 )

! Conver to string
  res = trim(double_to_character(rx))
  if( ix >= 0 ) then
    res = trim(res)//'+'//cunit
  else
    res = trim(res)//'-'//cunit
  end if
  res = trim(res)//trim(double_to_character(abs(ix)))

end function double_complex_to_character


! Convert a character string to double complex
elemental function character_to_double_complex( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting complex(kind=8)
  complex(kind=8) :: res

! Local variables
  integer :: ios
  real(kind=8) :: rx, ix
  integer :: i, j
  character(len=len(x)) :: local

! Look for all characters from the beginning that can be parsed
  local = adjustl(trim(x))
  i = verify( local, '01234567890-+.DEdeij' )
  if( i > 1 ) then
    j = scan( local, 'ij' )
    local = local(:i-1)
  else if( i == 1 ) then
    j = 1
    local = '0'
  end if

! Replace the imaginary unit
  if( j > 1 ) then
    local(j:j) = local(j-1:j-1)
    local(j-1:j-1) = ' '
  else if( j /= 0 ) then
    local(j:j) = ' '
  end if

! Convert to double
  rx = 0.0_8
  ix = 0.0_8
  read(local, *, iostat=ios) rx, ix
  if( ios > 0 ) then
    res = huge(0.0_8)
  else if( ios < 0 ) then
    if( j > 0 ) then
      res  = cmplx( 0.0_8, rx, kind=8 )
    else
      res  = cmplx( rx, 0.0_8, kind=8 )
    end if
  else
    res = cmplx( rx, ix, kind=8 )
  end if

end function character_to_double_complex


! Convert a character string to double complex
elemental function character_to_complex( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting complex(kind=4)
  complex :: res

! Local variables
  integer :: ios
  real :: rx, ix
  integer :: i, j
  character(len=len(x)) :: local

! Look for all characters from the beginning that can be parsed
  local = adjustl(trim(x))
  i = verify( local, '01234567890-+.DEdeij' )
  if( i > 1 ) then
    j = scan( local, 'ij' )
    local = local(:i-1)
  else if( i == 1 ) then
    j = 1
    local = '0'
  end if

! Replace the imaginary unit
  if( j > 1 ) then
    local(j:j) = local(j-1:j-1)
    local(j-1:j-1) = ' '
  else if( j /= 0 ) then
    local(j:j) = ' '
  end if

! Convert to real
  rx = 0.0
  ix = 0.0
  read(local, *, iostat=ios) rx, ix
  if( ios > 0 ) then
    res = huge(0.0)
  else if( ios < 0 ) then
    if( j > 0 ) then
      res  = cmplx( 0.0, rx, kind=4 )
    else
      res  = cmplx( rx, 0.0, kind=4 )
    end if
  else
    res = cmplx( rx, ix, kind=4 )
  end if

end function character_to_complex


! Convert a double to character string
elemental function double_to_character( x, sp, fmt ) result(res)

! The double number
  real(kind=8), intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer :: ios
  character(len=16) :: localfmt

! Build format
  localfmt = ss_double_fmt
  if( present(fmt) ) then
    localfmt = fmt
  else
    if( present(sp) ) then
      if( sp ) then
        localfmt = sp_double_fmt
      end if
    end if
  end if

! Convert to string
  write(res, localfmt, iostat=ios) x
  res = trim(res)

! If no format given then adjust to the left
  if( .not. present(fmt) ) then
    res = adjustl(res)
  end if

end function double_to_character


! Convert a real to character string
elemental function real_to_character( x, sp, fmt ) result(res)

! The double number
  real(kind=4), intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer :: ios
  character(len=16) :: localfmt

! Build format
  localfmt = ss_real_fmt
  if( present(fmt) ) then
    localfmt = fmt
  else
    if( present(sp) ) then
      if( sp ) then
        localfmt = sp_real_fmt
      end if
    end if
  end if

! Convert to string
  write(res, localfmt, iostat=ios) x
  res = trim(res)

! If no format given then adjust to the left
  if( .not. present(fmt) ) then
    res = adjustl(res)
  end if

end function real_to_character


! Convert a character string to double
elemental function character_to_double( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting real(kind=8)
  real(kind=8) :: res

! iostat
  integer :: ios

! Local variables
  integer :: i
  character(len=len(x)) :: local

! Look for all characters from the beginning that can be parsed
  local = adjustl(trim(x))
  i = verify( local, '01234567890-+.DEde' )
  if( i > 1 ) then
    local = local(:i-1)
  else if( i == 1 ) then
    local = '0'
  end if

! Convert to double
  read(local, *, iostat=ios) res
  if( ios /= 0 ) then
    res = huge(0.0_8)
  end if

end function character_to_double


! Convert a character string to default real
elemental function character_to_real( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting real(kind=8)
  real(kind=4) :: res

! iostat
  integer :: ios

! Local variables
  integer :: i
  character(len=len(x)) :: local

! Look for all characters from the beginning that can be parsed
  local = adjustl(trim(x))
  i = verify( local, '01234567890-+.DEde' )
  if( i > 1 ) then
    local = local(:i-1)
  else if( i == 1 ) then
    local = '0'
  end if

! Convert to double
  read(local, *, iostat=ios) res
  if( ios /= 0 ) then
    res = huge(0.0)
  end if

end function character_to_real


! Convert a kind=1 integer to character string
elemental function integer1_to_character( x, sp, fmt ) result(res)

! The double number
  integer(kind=1), intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer(kind=8) :: localx

! Convert to string
  localx = x
  res = character(localx, sp, fmt)

end function integer1_to_character

! Convert a kind=2 integer to character string
elemental function integer2_to_character( x, sp, fmt ) result(res)

! The double number
  integer(kind=2), intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer(kind=8) :: localx

! Convert to string
  localx = x
  res = character(localx, sp, fmt)

end function integer2_to_character

! Convert a kind=4 integer to character string
elemental function integer_to_character( x, sp, fmt ) result(res)

! The double number
  integer, intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer(kind=8) :: localx

! Convert to string
  localx = x
  res = character(localx, sp, fmt)

end function integer_to_character


! Convert a kind=8 integer to character string
elemental function integer8_to_character( x, sp, fmt ) result(res)

! The double number
  integer(kind=8), intent(in) :: x

! The flag to force the plus sign in positive values (optional; default .false.)
  logical, optional, intent(in) :: sp

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  integer :: ios
  character(len=16) :: localfmt

! Build format
  localfmt = ss_integer_fmt
  if( present(fmt) ) then
    localfmt = fmt
  else
    if( present(sp) ) then
      if( sp ) then
        localfmt = sp_integer_fmt
      end if
    end if
  end if

! Convert to string
  write(res, localfmt, iostat=ios) x
  res = trim(res)

! If no format given then adjust to the left
  if( .not. present(fmt) ) then
    res = adjustl(res)
  end if

end function integer8_to_character


! Convert a kind=1 integer to character string
elemental function logical1_to_character( x, fmt ) result(res)

! The double number
  logical(kind=1), intent(in) :: x

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  logical :: localx

! Convert to string
  localx = x
  res = trim(logical_to_character(localx, fmt))

! If no format given then adjust to the left
  if( .not. present(fmt) ) then
    res = adjustl(res)
  end if

end function logical1_to_character

! Convert a kind=2 integer to character string
elemental function logical2_to_character( x, fmt ) result(res)

! The double number
  logical(kind=2), intent(in) :: x

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=64) :: res

! Local variables
  logical :: localx

! Convert to string
  localx = x
  res = trim(logical_to_character(localx, fmt))

! If no format given then adjust to the left
  if( .not. present(fmt) ) then
    res = adjustl(res)
  end if

end function logical2_to_character

! Convert a kind=4 integer to character string
elemental function logical_to_character( x, fmt ) result(res)

! The double number
  logical, intent(in) :: x

! The format (optional)
  character(len=*), optional, intent(in) :: fmt

! The resulting string
  character(len=5) :: res

! Local variables
  integer :: idx
  character(len=5), parameter, dimension(0:1, 9) :: cvalues = reshape( &
    (/ 'no   ', 'yes  ', &
       'n    ', 'y    ', &
       'NO   ', 'YES  ', &
       'N    ', 'Y    ', &
       'false', 'true ', &
       'f    ', 't    ', &
       'FALSE', 'TRUE ', &
       'F    ', 'T    ', &
       '0    ', '1    ' /), (/ 2, 9 /) )

! Write the value
  if( present(fmt) ) then

!   Select the format
    select case( fmt )

!     yes/no lowercase
      case( 'yesno' )
        idx = 1

!     yes/no lowercase
      case( 'yn' )
        idx = 2

!     YES/NO uppercase
      case( 'YESNO' )
        idx = 3

!     YES/NO uppercase
      case( 'YN' )
        idx = 4

!     true/false lowercase
      case( 'truefalse' )
        idx = 5

!     true/false lowercase
      case( 'tf' )
        idx = 6

!     TRUE/FALSE uppercase
      case( 'TRUEFALSE' )
        idx = 7

!     TRUE/FALSE uppercase
      case( 'TF' )
        idx = 8

!     0/1
      case( '01' )
        idx = 9

      case default
        idx = 8

    end select

  else

!   Use default format
    idx = 8

  end if

! Select the format
  if( x ) then
    res = cvalues(1, idx)
  else
    res = cvalues(0, idx)
  end if

end function logical_to_character


! Convert a character string to integer
elemental function character_to_integer( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting integer
  integer :: res

! Convert to integer
  res = nint( character_to_double( x ) )

end function character_to_integer


! Convert a character string to logical
elemental function character_to_logical( x ) result(res)

! The character string
  character(len=*), intent(in) :: x

! The resulting integer
  logical :: res

! Local variables
  character(len=len(x)) :: y

! Check the start of the input looking for fortran constructs
  if( x(1:1) == '.' ) then
    y = x(2:)
  else
    y = x
  end if

! Verify first based on the first charater
  select case( y(1:1) )

!   Identify false
    case( '0', 'F', 'f', 'N', 'n' )
      res = .false.

!   Otherwise it is true
    case default
      res = .true.

  end select

end function character_to_logical


! Convert a kind=1 integer to hexadecimal representation
elemental function integer1_to_hex( x, lower ) result(res)

! The double number
  integer(kind=1), intent(in) :: x

! Flag to generate in lowercase
  logical, optional, intent(in) :: lower

! The resulting string
  character(len=2) :: res

! Convert to hexadecimal representation
  write( res, '(z2)' ) x

! Check for lowercase
  if( present(lower) ) then
    if( lower ) then
      res = lowercase(res)
    end if
  end if

end function integer1_to_hex

! Convert a kind=2 integer to hexadecimal representation
elemental function integer2_to_hex( x, lower ) result(res)

! The double number
  integer(kind=2), intent(in) :: x

! Flag to generate in lowercase
  logical, optional, intent(in) :: lower

! The resulting string
  character(len=4) :: res

! Convert to hexadecimal representation
  write( res, '(z4)' ) x

! Check for lowercase
  if( present(lower) ) then
    if( lower ) then
      res = lowercase(res)
    end if
  end if

end function integer2_to_hex

! Convert a kind=4 integer to hexadecimal representation
elemental function integer_to_hex( x, lower ) result(res)

! The double number
  integer, intent(in) :: x

! Flag to generate in lowercase
  logical, optional, intent(in) :: lower

! The resulting string
  character(len=8) :: res

! Convert to hexadecimal representation
  write( res, '(z8)' ) x

! Check for lowercase
  if( present(lower) ) then
    if( lower ) then
      res = lowercase(res)
    end if
  end if

end function integer_to_hex


! Convert a character string to lowercase
elemental function character_lowercase( s ) result(res)

! The input character string
  character(len=*), intent(in) :: s

! The converted character string
  character(len=len(s)) :: res

! Local variables
  integer :: ic, i

! Initialise result
  res = s

! Loop on all characters
  do i = 1, len(s)

!   Convert the uppercase letters only
    ic = ichar(s(i:i))
    if( ic >= index_AU ) then
      if( ic <= index_ZU ) then

!       Offset the ascii value to uppercase
        ic = ic + offset_U_to_L
        res(i:i) = char(ic)

      end if
    end if

  end do

end function character_lowercase


! Convert a character string to uppercase
elemental function character_uppercase( s ) result(res)

! The input character string
  character(len=*), intent(in) :: s

! The converted character string
  character(len=len(s)) :: res

! Local variables
  integer :: ic, i

! Initialise result
  res = s

! Loop on all characters
  do i = 1, len(s)

!   Convert the lowercase letters only
    ic = ichar(s(i:i))
    if( ic >= index_AL ) then
      if( ic <= index_ZL ) then

!       Offset the ascii value to uppercase
        ic = ic - offset_U_to_L
        res(i:i) = char(ic)

      end if
    end if

  end do

end function character_uppercase


! Return whether a number is odd (default integer)
elemental function odd_integer( i ) result(res)

! The integer
  integer, intent(in) :: i

! The odd result
  logical :: res

! Compute if value is odd
  res = ( btest( i, 0 ) )

end function odd_integer


! Return whether a number is odd (kind=2 integer)
elemental function odd_integer2( i ) result(res)

! The integer
  integer(kind=2), intent(in) :: i

! The odd result
  logical :: res

! Compute if value is odd
  res = ( btest( i, 0 ) )

end function odd_integer2


! Return whether a number is odd (kind=1 integer)
elemental function odd_integer1( i ) result(res)

! The integer
  integer(kind=1), intent(in) :: i

! The odd result
  logical :: res

! Compute if value is odd
  res = ( btest( i, 0 ) )

end function odd_integer1


! Return whether a number is even (default integer)
elemental function even_integer( i ) result(res)

! The integer
  integer, intent(in) :: i

! The even result
  logical :: res

! Compute if value is even
  res = ( .not. btest( i, 0 ) )

end function even_integer


! Return whether a number is even (kind=2 integer)
elemental function even_integer2( i ) result(res)

! The integer
  integer(kind=2), intent(in) :: i

! The even result
  logical :: res

! Compute if value is even
  res = ( .not. btest( i, 0 ) )

end function even_integer2


! Return whether a number is even (kind=1 integer)
elemental function even_integer1( i ) result(res)

! The integer
  integer(kind=1), intent(in) :: i

! The even result
  logical :: res

! Compute if value is even
  res = ( .not. btest( i, 0_1 ) )

end function even_integer1


! Convert character string into array of characters
pure function character_to_array( string ) result(res)

! The character string
  character(len=*), intent(in) :: string

! The buffer of bytes
  character, dimension(len(string)) :: res

! Convert
  res = transfer( string, res )

end function character_to_array


! Convert an array of character into character string
pure function array_to_character( buffer ) result(res)

! The buffer of bytes
  character, dimension(:), intent(in) :: buffer

! The character string
  character(len=size(buffer)) :: res

! Convert
  res = transfer( buffer, res )

end function array_to_character


! Convert character string into byte buffer
pure function character_to_bytes( string ) result(res)

! The character string
  character(len=*), intent(in) :: string

! The buffer of bytes
  integer(kind=1), dimension(len(string)) :: res

! Local variables
  character, dimension(len(string)) :: cpk

! Convert
  cpk = transfer( string, cpk )
  res = ichar( cpk, kind=1 )

end function character_to_bytes


! Convert byte buffer into character string
pure function bytes_to_character( buffer ) result(res)

! The buffer of bytes
  integer(kind=1), dimension(:), intent(in) :: buffer

! The character string
  character(len=size(buffer)) :: res


! Local variables
  character, dimension(size(buffer)) :: cpk

! Convert
  cpk = char( buffer )
  res = transfer( cpk, res )

end function bytes_to_character


! Convert character string into byte buffer
pure function hex_to_bytes( string ) result(res)

! The character string
  character(len=*), intent(in) :: string

! The buffer of bytes
  integer(kind=1), dimension(len(string)/2) :: res

! Local variables
  character(len=10) :: fmt

! Convert
  write( fmt, "('(', i0, 'z2)')" ) size(res)
  read( string, fmt ) res

end function hex_to_bytes


! Convert byte buffer into hexadecimal string representation
pure function bytes_to_hex( buffer, lower ) result(res)

! The buffer of bytes
  integer(kind=1), dimension(:), intent(in) :: buffer

! The flag to generate the string lowercase
  logical, optional, intent(in) :: lower

! The character string in hexadecimal
  character(len=2*size(buffer)) :: res

! Local variables
  character(len=10) :: fmt

! Convert
  write( fmt, "('(', i0, 'z2.2)')" ) size(buffer)
  write( res, fmt ) buffer

! Convert to lowercase
  if( present(lower) ) then
    if( lower ) then
      res = lowercase(res)
    end if
  end if

end function bytes_to_hex


! Convert a list of bits into a byte buffer
pure function bits_list_to_bytes( list ) result(res)

! The bits list
  integer(kind=1), dimension(:), intent(in) :: list

! The converted byte buffer
  integer(kind=1), dimension(size(list)/8) :: res

! Local variables
  integer :: i, j, k

! Loop on the output bytes
  res = 0_1
  do j = 1, size(res)

!   Assign the result fields according to the bits
    i = 8 * ( j - 1 ) + 1
    do k = 0, 7
      if( list(i+k) == 1 ) then
        res(j) = ibset( res(j), 7-k )
      end if
    end do

  end do

end function bits_list_to_bytes


! Convert a byte buffer into a list of bits
pure function bytes_to_bits_list( buffer ) result(res)

! The byte buffer to convert
  integer(kind=1), dimension(:), intent(in) :: buffer

! The result bits list
  integer(kind=1), dimension(8*size(buffer)) :: res

! Local variables
  integer :: i, j
  logical, dimension(8) :: bitset

! Loop on the entry bytes
  res = 0_1
  do j = 1, size(buffer)

!   Compute set bits for this byte
    bitset = btest( buffer(j), (/ 7, 6, 5, 4, 3, 2, 1, 0 /) )

!   Assign the result fields according to the bits
    i = 8 * ( j- 1 ) + 1
    where( bitset )
      res(i:i+7) = 1_1
    else where
      res(i:i+7) = 0_1
    end where

  end do

end function bytes_to_bits_list


! Compute the parameters to split a character string in tokens with a given separator character
pure subroutine character_split_count( chars, separator, ntokens, max_token_len, multiple )

! The character string to split
  character(len=*), intent(in) :: chars

! The separator character
  character, intent(in) :: separator

! The number of tokens
  integer, intent(out) :: ntokens

! The length of the longest token
  integer, intent(out) :: max_token_len

! Flag to treat multiple consecutive separators as one
  logical, optional, intent(in) :: multiple

! Local variables
  integer :: idx, jdx
  logical :: mult

! Initialise
  ntokens = 0
  max_token_len = 0

! Check multiple separators flag
  mult = .false.
  if( present(multiple) )then
    mult = multiple
  end if

! Loop looking for separators
  idx = 1
  do

!   Look for the next separator
    jdx = index( chars(idx:), separator )

!   Verify the computed index
    if( jdx == 0 ) then

!     Last token or no separator in input
      ntokens = ntokens + 1

!     Compute maximum token length
      max_token_len = max( max_token_len, len_trim(chars(idx:)) )

!     Exit loop
      exit

    else

!     Separator found
      ntokens = ntokens + 1

!     Compute maximum token length
      max_token_len = max( max_token_len, jdx )

!     Check if multiple separators are considered one
      if( mult ) then
        do while( chars(idx+jdx:idx+jdx) == separator )
          jdx = jdx + 1
        end do
      end if

!     Prepare index for next search
!     jdx always has the position of the last separator found
      idx = idx + jdx

    end if

  end do

end subroutine character_split_count


! Split a character string in tokens with a given separator character
pure subroutine character_split( chars, separator, tokens, multiple )

! The character string to split
  character(len=*), intent(in) :: chars

! The separator character
  character, intent(in) :: separator

! The list of tokens
  character(len=:), dimension(:), allocatable, intent(out) :: tokens

! Flag to treat multiple consecutive separators as one
  logical, optional, intent(in) :: multiple

! Local variables
  integer :: ntokens, itoken, ltoken
  integer :: idx, jdx
  logical :: mult

! Initialise
  ltoken = 0
  ntokens = 0

! Check multiple separators flag
  mult = .false.
  if( present(multiple) )then
    mult = multiple
  end if

! Initialise the token list parameters
  call character_split_count( chars, separator, ntokens, ltoken, multiple )

! Allocate output rray
  allocate( character(len=ltoken) :: tokens(ntokens) )

! Loop looking for separators
  idx = 1
  itoken = 0
  do

!   Look for the next separator
    jdx = index( chars(idx:), separator )

!   Verify the computed index
    if( jdx == 0 ) then

!     Last token or no separator in input
      itoken = itoken + 1

!     Store the token
      tokens(itoken) = chars(idx:)

!     Exit loop
      exit

    else

!     Separator found
      itoken = itoken + 1

!     Store the token
      tokens(itoken) = chars(idx:idx+jdx-2)

!     Check if multiple separators are considered one
      if( mult ) then
        do while( chars(idx+jdx:idx+jdx) == separator )
          jdx = jdx + 1
        end do
      end if

!     Prepare index for next search
!     jdx always has the position of the last separator found
      idx = idx + jdx

    end if

  end do

end subroutine character_split


! Join in a chracter string a list of tokens with a given joining character
pure function character_join( tokens, separator, token_trim ) result(res)

! The list of tokens
  character(len=*), dimension(:), intent(in) :: tokens

! The separator character
  character, intent(in) :: separator

! Flag to trim token while joining (defaults to .true.)
  logical, optional, intent(in) :: token_trim

! The resulting string
  character(len=:), allocatable :: res

! Local variables
  integer :: itoken
  logical :: do_trim

! Check trim flag
  do_trim = .true.
  if( present(token_trim) ) then
    do_trim = token_trim
  end if

! Initialise
  if( do_trim ) then
    res = trim(tokens(1))
  else
    res = tokens(1)
  end if

! Loop on the tokens
  do itoken = 2, size(tokens)
    if( do_trim ) then
      res = res // separator // trim(tokens(itoken))
    else
      res = res // separator // tokens(itoken)
    end if
  end do

end function character_join


! Change endianness for a four byte integer
elemental function change_endianness_integer4( i4 ) result(res)

! Input integer
  integer(kind=4), intent(in) :: i4

! Resulting integer
  integer(kind=4) :: res

! Local variables
  integer(kind=1), dimension(4) :: array

! Put the input integer in local storage
  array = transfer( i4, array )

! Reorder array bytes
  array = array(4:1:-1)

! Put the local storage in the result
  res = transfer( array, res )

end function change_endianness_integer4


! Change endianness for a four byte integer
elemental function change_endianness_integer2( i2 ) result(res)

! Input integer
  integer(kind=2), intent(in) :: i2

! Resulting integer
  integer(kind=2) :: res

! Local variables
  integer(kind=1), dimension(2) :: array

! Put the input integer in local storage
  array = transfer( i2, array )

! Reorder array bytes
  array = array(2:1:-1)

! Put the local storage in the result
  res = transfer( array, res )

end function change_endianness_integer2

end module m_util_convert
