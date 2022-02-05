module m_string

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Dynamic character string
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

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_string
  public string

  public len, len_trim, trim, adjustl, adjustr, strip, len_strip, &
         index, scan, verify, &
         lge, lgt, lle, llt

  public operator(+), character, match, replace, lowercase, uppercase

  !- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

! String type
  type, extends(t_object) :: t_string
    private

!     String buffer
      character(len=:), allocatable :: buffer

    contains

!     Initialisation status
      procedure :: is_initialised => string_is_initialised

!     Assignement operator
      generic :: assignment(=) => string_assign_from_char, &
!                                  string_assign_from_array, &
                                  string_assign_from_string, &
                                  char_alloc_assign_from_string
      procedure, private :: string_assign_from_char
!      procedure, private :: string_assign_from_array
      procedure, private :: string_assign_from_string
      procedure, private, pass(right) :: char_alloc_assign_from_string

!     Concatenation operators
      generic :: operator(+) => string_concat_string, &
                                string_concat_char, &
                                char_concat_string
      generic :: operator(//) => string_concat_string, &
                                 string_concat_char, &
                                 char_concat_string
      procedure, private :: string_concat_string
      procedure, private :: string_concat_char
      procedure, private, pass(right) :: char_concat_string

!     Tokenize functions
      procedure, private :: token_count => string_token_count
      procedure :: split => string_split
      generic :: join => string_join_char, string_join_string
      procedure, private :: string_join_char
      procedure, private :: string_join_string

!     Equality overriding
      procedure :: equals => string_equals

!     Equalityoperator
      generic :: operator(==) => string_equal_string, &
                                 string_equal_char, &
                                 char_equal_string
      procedure, private :: string_equal_string
      procedure, private :: string_equal_char
      procedure, private, pass(right) :: char_equal_string

!     Inequality operator
      generic :: operator(/=) => string_nonequal_string, &
                                 string_nonequal_char, &
                                 char_nonequal_string
      procedure, private :: string_nonequal_string
      procedure, private :: string_nonequal_char
      procedure, private, pass(right) :: char_nonequal_string

!     Comparison (greater than) operator
      generic :: operator(>) => string_greater_string, &
                                string_greater_char, &
                                char_greater_string
      procedure, private :: string_greater_string
      procedure, private :: string_greater_char
      procedure, private, pass(right) :: char_greater_string

!     Comparison (greater than or equal) operator
      generic :: operator(>=) => string_greater_equal_string, &
                                 string_greater_equal_char, &
                                 char_greater_equal_string
      procedure, private :: string_greater_equal_string
      procedure, private :: string_greater_equal_char
      procedure, private, pass(right) :: char_greater_equal_string

!     Comparison (less than) operator
      generic :: operator(<) => string_less_string, &
                                string_less_char, &
                                char_less_string
      procedure, private :: string_less_string
      procedure, private :: string_less_char
      procedure, private, pass(right) :: char_less_string

!     Comparison (less than or equal) operator
      generic :: operator(<=) => string_less_equal_string, &
                                 string_less_equal_char, &
                                 char_less_equal_string
      procedure, private :: string_less_equal_string
      procedure, private :: string_less_equal_char
      procedure, private, pass(right) :: char_less_equal_string

!     Conversion to intrinsic character
      generic :: character => string_to_char, &
                              string_to_char_selected
      procedure, private :: string_to_char
      procedure, private :: string_to_char_selected

!     String replacement
      procedure :: replace => string_replace_s

!     Aliases to make the type compatible with intrinsic character
      generic :: len => string_len
      procedure, private :: string_len
      generic :: len_trim => string_len_trim
      procedure, private :: string_len_trim
      generic :: trim => string_trim
      procedure, private :: string_trim
      generic :: adjustl => string_adjustl
      procedure, private :: string_adjustl
      generic :: adjustr => string_adjustr
      procedure, private :: string_adjustr
      generic :: index => string_index_c, &
                          string_index_s
      procedure, private :: string_index_c
      procedure, private :: string_index_s
      generic :: scan => string_scan_c, &
                         string_scan_s
      procedure, private :: string_scan_c
      procedure, private :: string_scan_s
      generic :: verify => string_verify_c, &
                           string_verify_s
      procedure, private :: string_verify_c
      procedure, private :: string_verify_s

!     Character intrinsic function extensions
      generic :: len_strip => string_len_strip
      procedure, private :: string_len_strip
      procedure :: strip => string_strip

!     Character compatibility interfaces
      generic :: lgt => string_greater_string, &
                        string_greater_char
      generic :: lge => string_greater_equal_string, &
                        string_greater_equal_char
      generic :: llt => string_less_string, &
                        string_less_char
      generic :: lle => string_less_equal_string, &
                        string_less_equal_char

!     Pattern matching with wildcards
      generic :: match => string_match_s
      procedure, private :: string_match_s

!     Lowercase and uppercase
      generic :: lowercase => string_lowercase
      procedure, private :: string_lowercase
      generic :: uppercase => string_uppercase
      procedure, private :: string_uppercase

!     Interface to array
      generic :: array => string_array, &
                          string_array_selected, &
                          string_array_selected_with_end
      procedure, private :: string_array
      procedure, private :: string_array_selected
      procedure, private :: string_array_selected_with_end

!     Read/write interafaces
      generic :: read => string_read
      procedure, private :: string_read
      generic :: write => string_write
      procedure, private :: string_write

!     Methods to make t_string quicksortable
      procedure :: less => string_less
      procedure :: assign => string_assign
      procedure :: string => string_to_char

  end type t_string

! Constructor interface
  interface string
    module procedure string_from_empty
    module procedure string_from_char
    module procedure string_from_char_selected
    module procedure string_from_array
    module procedure string_from_array_selected
    module procedure string_from_string
    module procedure string_from_string_selected
  end interface string

! Interfaces to allow character + character
  interface operator(+)
    module procedure char_concat_char
  end interface operator(+)

! The blank character
  character, parameter :: blank = ' '

! Wildcard for pattern matching
  character, parameter :: single_wildcard   = '?'
  character, parameter :: multiple_wildcard = '*'

! Interfaces for character intrinsic function extensions
  interface len_strip
    module procedure string_len_strip
  end interface len_strip
  interface strip
    module procedure string_strip
  end interface strip
  interface character
    module procedure string_to_char_static
    module procedure string_to_char_selected_static
  end interface character
  interface replace
    module procedure string_replace_c
    module procedure string_replace_s
  end interface replace
  interface match
    module procedure string_match_c
    module procedure string_match_s
  end interface match

! Aliases to make the type compatible with intrinsic character string
  interface len
    module procedure string_len
  end interface len
  interface len_trim
    module procedure string_len_trim
  end interface len_trim
  interface trim
    module procedure string_trim
  end interface trim
  interface adjustl
    module procedure string_adjustl
  end interface adjustl
  interface adjustr
    module procedure string_adjustr
  end interface adjustr
  interface index
    module procedure string_index_c
    module procedure string_index_s
  end interface index
  interface scan
    module procedure string_scan_c
    module procedure string_scan_s
  end interface scan
  interface verify
    module procedure string_verify_c
    module procedure string_verify_s
  end interface verify
  interface lgt
    module procedure string_greater_string
    module procedure string_greater_char
    module procedure char_greater_string
  end interface lgt
  interface lge
    module procedure string_greater_equal_string
    module procedure string_greater_equal_char
    module procedure char_greater_equal_string
  end interface lge
  interface llt
    module procedure string_less_string
    module procedure string_less_char
    module procedure char_less_string
  end interface llt
  interface lle
    module procedure string_less_equal_string
    module procedure string_less_equal_char
    module procedure char_less_equal_string
  end interface lle

! Additional convenience interfaces
  interface lowercase
    module procedure string_lowercase
  end interface lowercase
  interface uppercase
    module procedure string_uppercase
  end interface uppercase

!- End of module variable declarations ----------------------------------------

contains

! Constructor for empty string of given size
elemental function string_from_empty( n ) result(this)

! The string size in bytes
  integer, intent(in) :: n

! The string
  type(t_string) :: this

! Allocate the string
  allocate( character(len=n) :: this%buffer )

! Initialise to blanks
  this%buffer = blank

end function string_from_empty


! Constructor from character
elemental function string_from_char( c ) result(res)

! The character string to use as initialisation (optional)
  character(len=*), optional, intent(in) :: c

! The string
  type(t_string) :: res

! Check input character string
  if( present(c) ) then

!   Initialise from input
    res%buffer = c

  else

!   Initialisation by default

  end if

end function string_from_char


! Constructor from part of character
elemental function string_from_char_selected( c, start, end ) result(res)

! The character string to use as initialisation (optional)
  character(len=*), intent(in) :: c

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: res

! Local variables
  integer :: iend

! Select the indexes
  if( present(end) ) then
    iend = min( end, len(c) )
  else
    iend = len(c)
  end if

! Initialise from input
  res%buffer = c(start:iend)

end function string_from_char_selected


! Constructor from character array
pure function string_from_array( c ) result(res)

! The character string to use as initialisation (optional)
  character, dimension(:), intent(in) :: c

! The string
  type(t_string) :: res

! Initialise from input
  allocate( character(len=size(c)) :: res%buffer )
  res%buffer = transfer( c, res%buffer )

end function string_from_array


! Constructor from part of character array
pure function string_from_array_selected( c, start, end ) result(res)

! The character string to use as initialisation (optional)
  character, dimension(:), intent(in) :: c

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: res

! Local variables
  integer :: iend

! Select the indexes
  if( present(end) ) then
    iend = min( end, size(c) )
  else
    iend = size(c)
  end if

! Initialise from input
  allocate( character(iend-start+1) :: res%buffer )
  res%buffer = transfer( c(start:iend), res%buffer )

end function string_from_array_selected


! Constructor from string
elemental function string_from_string( s0 ) result(res)

! The character string to use as initialisation (optional)
  class(t_string), intent(in) :: s0

! The string
  type(t_string) :: res

! Initialise from input
  res%buffer = s0%buffer

end function string_from_string


! Constructor from part of string
elemental function string_from_string_selected( s0, start, end ) result(res)

! The character string to use as initialisation (optional)
  class(t_string), intent(in) :: s0

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: res

! Local variables
  integer :: iend

! Select the indexes
  if( present(end) ) then
    iend = min( end, len(s0%buffer) )
  else
    iend = len(s0%buffer)
  end if

! Initialise from input
  res%buffer = s0%buffer(start:iend)

end function string_from_string_selected


! Check initialisaiton status
elemental function string_is_initialised( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The initialisation status
  logical :: res

 ! Return initialisaiton status
   res = allocated( this%buffer )

end function string_is_initialised


! String length
elemental function string_len( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string length
  integer :: res

! Return the length
  res = len(this%buffer)

end function string_len


! String length (traling blanks removed)
elemental function string_len_trim( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string length
  integer :: res

! Return the trimmed length
   res = len_trim(this%buffer)

end function string_len_trim


! String length (traling leading and blanks removed)
elemental function string_len_strip( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string length
  integer :: res

! Compute length
  res = len_trim(adjustl(this%buffer))

end function string_len_strip


! Remove string traling blanks
elemental function string_trim( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  type(t_string) :: res

! Compute the trimmed string
  res%buffer = trim(this%buffer)

end function string_trim


! Remove string leading and traling blanks
elemental function string_strip( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  type(t_string) :: res

! Compute the stripped string
  res%buffer = trim(adjustl(this%buffer))

end function string_strip


! Performa a character replacement in a string
elemental function string_replace_s( this, c1, c2 ) result(res)

! The string
  class(t_string), intent(in) :: this

! The character to search for
  character, intent(in) :: c1

! The character to use as replacement
  character, intent(in) :: c2

! The output string
  type(t_string) :: res

! Make the replacement
  res%buffer = replace( this%buffer, c1, c2 )

end function string_replace_s


! Performa a character replacement in a character string
elemental function string_replace_c( this, c1, c2 ) result(res)

! The string
  character(len=*), intent(in) :: this

! The character to search for
  character, intent(in) :: c1

! The character to use as replacement
  character, intent(in) :: c2

! The output string
  character(len=len(this)) :: res

! Local variables
  integer :: ic

! Make the replacement
  res = this
  do ic = 1, len(res)
    if( res(ic:ic) == c1 ) then
      res(ic:ic) = c2
    end if
  end do

end function string_replace_c


! Left justify string contents
elemental function string_adjustl( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  type(t_string) :: res

! Compute the left justified string
  res%buffer = adjustl(this%buffer)

end function string_adjustl


! Right justify string contents
elemental function string_adjustr( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  type(t_string) :: res

! Compute the right justified string
  res%buffer = adjustr(this%buffer)

end function string_adjustr


! Get the position of a substring in a string
elemental function string_index_s( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  type(t_string), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = index( this%buffer, subs%buffer, back )

end function string_index_s


! Get the position of a substring in a string
elemental function string_index_c( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  character(len=*), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = index( this%buffer, subs, back )

end function string_index_c


! Get the position of a character in substring in a string
elemental function string_scan_s( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  type(t_string), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = scan( this%buffer, subs%buffer, back )

end function string_scan_s


! Get the position of a character in substring in a string
elemental function string_scan_c( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  character(len=*), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = scan( this%buffer, subs, back )

end function string_scan_c


! Get the position of a character in substring in a string
elemental function string_verify_s( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  type(t_string), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = verify( this%buffer, subs%buffer, back )

end function string_verify_s


! Get the position of a character in substring in a string
elemental function string_verify_c( this, subs, back ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  character(len=*), intent(in) :: subs

! The search direction
  logical, optional, intent(in) :: back

! The character position
  integer :: res

! Compute the position
  res = verify( this%buffer, subs, back )

end function string_verify_c


! Return the string as character
pure function string_to_char( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  character(len=:), allocatable :: res

! Return the character string
  allocate( res, source=this%buffer )

end function string_to_char


! Return the string as character
! intel-bug
! Used as static function; the type bound procedure generates
! a SIGSEGV if the passed first object is class instead of type
pure function string_to_char_static( object ) result(res)

! The string
  type(t_string), intent(in) :: object

! The resulting character string
  character(len=:), allocatable :: res

! Return the character string
  res = object%character()

end function string_to_char_static


! Return the string as character with selected bounds
pure function string_to_char_selected( this, start, end ) result(res)

! The string
  class(t_string), intent(in) :: this

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The resulting character string
  character(len=:), allocatable :: res

! Local variables
  integer :: iend

! Select the indexes
  if( present(end) ) then
    iend = min( end, len(this%buffer) )
  else
    iend = len(this%buffer)
  end if

! Return the character string
  allocate( res, source=this%buffer(start:iend) )

end function string_to_char_selected


! Return the string as character with selected bounds
! intel-bug
! Used as static function; the type bound procedure generates
! a SIGSEGV if the passed first object is class instead of type
pure function string_to_char_selected_static( object, start, end ) result(res)

! The string
  type(t_string), intent(in) :: object

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The resulting character string
  character(len=:), allocatable :: res

! Return the character string
  res = object%character( start, end )

end function string_to_char_selected_static


! Return the string all in lowercase
elemental function string_lowercase( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting lowercase string
  type(t_string) :: res

! Convert to lowercase
  res%buffer = lowercase( this%buffer )

end function string_lowercase


! Return the string all in uppercase
elemental function string_uppercase( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting uppercase string
  type(t_string) :: res

! Convert to uppercase
  res%buffer = uppercase( this%buffer )

end function string_uppercase


! Return the string as a character array
pure function string_array( this ) result(res)

! The string
  class(t_string), intent(in) :: this

! The resulting character string
  character, dimension(len(this%buffer)) :: res

! Return the character string
  res = transfer( this%buffer, res )

end function string_array


! Return the string as a character array with selected bounds
pure function string_array_selected( this, start ) result(res)

! The string
  class(t_string), intent(in) :: this

! The first character to select
  integer, intent(in) :: start

! The resulting character string
  character, dimension(len(this%buffer)-start+1) :: res

! Local variables
  integer :: iend

! Check input allocation
  if( allocated(this%buffer) ) then

!   Select the indexes
    iend = len(this%buffer)

!   Return the character string
    res = transfer( this%buffer(start:iend), res )

  end if

end function string_array_selected


! Return the string as a character array with selected bounds
pure function string_array_selected_with_end( this, start, end ) result(res)

! The string
  class(t_string), intent(in) :: this

! The first character to select
  integer, intent(in) :: start

! The last character to select
  integer, intent(in) :: end

! The resulting character string
  character, dimension(end-start+1) :: res

! Local variables
  integer :: iend

! Check allocation
  if( allocated(this%buffer) ) then

!   Select the indexes
    iend = min( end, len(this%buffer) )

!   Return the character string
    res = transfer( this%buffer(start:iend), res )

  end if

end function string_array_selected_with_end


! Match a character string against a pattern with wildcards
elemental function string_match_s( this, pattern ) result(res)

! The string
  class(t_string), intent(in) :: this

! The string searched
  type(t_string), intent(in) :: pattern

! The matching result
  logical :: res

! Compute the match using the character implementation
  res = match( this%buffer, pattern%buffer )

end function string_match_s


! Auxiliary function to compress the pattern for the wildcard pattern matching.
! All consecutive occurrences of one or more question marks ('?') and
! asterisks ('*') are sorted and compressed.
! Reference: http://fortranwiki.org/fortran/show/match_wild
pure function string_pattern_compress( pattern ) result(res)

! The string pattern
  character(len=*), intent(in) :: pattern

! The compressed pattern
  character(len=:), allocatable :: res

! Local variables
  character(len=len(pattern)) :: pattern2
  integer :: p, p2
  integer :: n_single, n_multi

! Initialise
  pattern2 = ''
  p = 1 ! current position in input pattern
  p2 = 0 ! current position in generated pattern
  n_single = 0
  n_multi = 0

! Main loop in pattern
  do

!   Check the pattern character
    select case( pattern(p:p) )

!     Single character wildcard
      case( single_wildcard )
        n_single = n_single + 1

!     Multiple character wildcard
      case( multiple_wildcard )
        n_multi = n_multi + 1

!     Any gneric character
      case default

!       Check single wildcard first
        if( n_single > 0 ) then
          pattern2 = pattern2(:p2) // repeat(single_wildcard,n_single)
          p2 = p2 + n_single
        end if

!       Check multiple wildcard (add only one)
        if( n_multi > 0 ) then
          pattern2 = pattern2(:p2) // multiple_wildcard
          p2 = p2 + n_multi
        end if

!       Append the character
        pattern2 = pattern2(:p2) // pattern(p:p)
        p2 = p2 + 1

!       Reset wildcard counters
        n_single = 0
        n_multi = 0

    end select

!   Iterate
    p = p + 1
    if( p > len(pattern) ) exit

  end do

! Check single wildcard first
  if( n_single > 0 ) then
    pattern2 = pattern2(:p2) // repeat(single_wildcard,n_single)
    p2 = p2 + n_single
  end if

! Check multiple wildcard (add only one)
  if( n_multi > 0 ) then
    pattern2 = pattern2(:p2) // multiple_wildcard
  end if

! Return the pattern
  allocate( res, source=pattern2 )

end function string_pattern_compress


! Match a character string against a pattern with wildcards
! Reference: http://fortranwiki.org/fortran/show/match_wild
elemental function string_match_c( input, pattern ) result(res)

! The string
  character(len=*), intent(in) :: input

! The string pattern
  character(len=*), intent(in) :: pattern

! The character position
  logical :: res

! Local variables
  character(len=:), allocatable :: cpattern
  integer :: p, p1, plast
  integer :: s
  integer :: n

! Compress the input pattern
  allocate( cpattern, source=string_pattern_compress( pattern ) )

! Initialise
  res = .false.
  s = 1
  p = 1
  plast = 0

! Main loop in the pattern characters
  do

!   Check the pattern character
    select case( cpattern(p:p) )

!     Single character wildcard
      case( single_wildcard )
        plast = p

!       Accept any char in string
        s = s + 1

!     Multiple character wildcard
      case( multiple_wildcard )
        plast = p

!       Check if at the end of the pattern
        if( p == len(pattern) ) then

!         The rest of the input matches
          res = .true.
          exit

        else

!         Look for the character following the wildcard in the string
!         Because pattern has been compressed and in this parto of the
!         if/then/else the next character is a general character
          p1 = p + 1
          n = index( input(s:), cpattern(p1:p1) )
          if( n == 0 ) then

!           Character not found; match not possible
            exit

          else

!           Next character matches
            s = n + s - 1

          end if

        end if

!     Any gneric character
      case default

!       Check match against pattern
        if( cpattern(p:p) == input(s:s) ) then

        else

!         Non-match
!         Fail to match one character in the pattern but may be found later.
!         Return pattern index to the last found wildcard
!         If there was no prior wildcard, then return .false.
          if( plast > 0 ) then
            p = plast
          else
            exit
          end if

      endif

!     Iterate in the input string
      s = s + 1

    end select

!   Iterate
    p = p + 1

!   Check pattern pointer position
    if( p > len(cpattern) ) then

!     Check input pointer position
      if( s > len(input) ) then

!       End of both pattern and input
        res = .TRUE.
        exit

      else

!       End of pattern only
        exit

      endif

!     IF (s > lens .AND. p2 == lenp) THEN
!       IF(pattern2(p2:p2) == "*") THEN
!! "*" at end of pattern2 represents an empty string
!         match_wild = .TRUE.
!         EXIT
!       ENDIF
!     ENDIF

    else

!     Check input pointer position
      if( s > len(input) ) then

!       End of input only
        exit

      endif

    end if

  enddo

end function string_match_c


! Read a string from an open unit
subroutine string_read( this, unit, iostat, format )

! The string
  class(t_string), intent(out) :: this

! The open file to read from
  integer, intent(in) :: unit

! The read condition status
  integer, optional, intent(out) :: iostat

! The read format (optional)
  character(len=*), optional, intent(in) :: format

! Local storage
  character(len=1024) :: local

! Check format
  if( present(format) ) then
    read( unit, format, iostat=iostat ) local
    this%buffer = local
  else
    read( unit, '(A1024)', iostat=iostat ) local
    this%buffer = trim(local)
  end if

end subroutine string_read


! Write in ASCII
subroutine string_write( this, unit, advance )

! The vector
  class(t_string), intent(in) :: this

! The open file to write the element to
  integer, intent(in) :: unit

! Write a new line after the vector (true by default)
  character(len=*), optional, intent(in) :: advance

! Write the string
  write( unit, '(A)', advance='no' ) this%buffer

! Check for newline at the end
  if( present(advance) ) then
    if( advance == 'YES' ) then
      write(unit,*)
    end if
  else
    write(unit,*)
  end if

end subroutine string_write


! Assign operator (string from char)
elemental subroutine string_assign_from_char( left, right )

! The target string
  class(t_string), intent(inout) :: left

! The source string
  character(len=*), intent(in) :: right

! Copy memory
  left%buffer = right

end subroutine string_assign_from_char


!! Assign operator (string from char array)
!pure subroutine string_assign_from_array( left, right )
!
!! The target string
!  class(t_string), intent(out) :: left
!
!! The source string
!  character, dimension(:), intent(in) :: right
!
!! Copy memory
!  left%buffer = transfer( right, left%buffer )
!
!end subroutine string_assign_from_array


! Assign operator (char from string)
elemental subroutine string_assign_from_string( left, right )

! The target string
  class(t_string), intent(inout) :: left

! The source string
  type(t_string), intent(in) :: right

  integer :: n

  n = len(right%buffer)

! Copy memory
  if(allocated(left%buffer)) then
    deallocate(left%buffer)
  end if
  if(allocated(right%buffer)) then
    allocate( character(len=len(right%buffer)) :: left%buffer )
    left%buffer = right%buffer
  end if

end subroutine string_assign_from_string


! Assign operator (char from string)
pure subroutine char_alloc_assign_from_string( left, right )

! The target string
  character(len=:), allocatable, intent(out) :: left

! The source string
  class(t_string), intent(in) :: right

! Copy memory
  left = right%buffer

end subroutine char_alloc_assign_from_string


! Concatenation operations
elemental function string_concat_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The resulting string
  type(t_string) :: res

! Concatenate strings
  res%buffer = left%buffer // right%buffer

end function string_concat_string


! Concatenation operations
elemental function string_concat_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The resulting string
  type(t_string) :: res

! Concatenate strings
  res%buffer = left%buffer // right

end function string_concat_char


! Concatenation operations
elemental function char_concat_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The resulting string
  type(t_string) :: res

! Concatenate strings
  res%buffer = left // right%buffer

end function char_concat_string


! Concatenation operations
elemental function char_concat_char( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The resulting string
  type(t_string) :: res

! Concatenate strings
  res%buffer = left // right

end function char_concat_char


! Compute the number of tokens in a string
pure function string_token_count( this, separator, multiple ) result(res)

! The string to tokenize
  class(t_string), intent(in) :: this

! The separator character
  character, intent(in) :: separator

! Flag to consider consecutive separators as one (default to .false.)
  logical, optional, intent(in) :: multiple

! The number of tokens
  integer :: res

! Local variables
  logical :: mult
  integer :: consecutive
  integer :: i, n

! Initialise the number of tokens
! Minimum one, even if the string is empty
  res = 1

! Compute the number of tokens
  n = len(this%buffer)
  do i = 1, n
    if( this%buffer(i:i) == separator ) then
      res = res + 1
    end if
  end do

! Check if multiple separators are to be considered as one
  if( present(multiple) ) then
    mult = multiple
    if( mult ) then

!     Look for conscutive separators
      consecutive = 0
      do i = 1, n-1
        if( this%buffer(i:i) == separator ) then
          if( this%buffer(i+1:i+1) == separator ) then
            consecutive = consecutive + 1
          end if
        end if
      end do

!     Remove the multiple separators
      res = res - consecutive

    end if
  end if

end function string_token_count


! Split a chracter string into token separated by a given character
pure subroutine string_split( this, separator, tokens, multiple )

! The string to split
  class(t_string), intent(in) :: this

! The separator character
  character, intent(in) :: separator

! The list of tokens
  type(t_string), dimension(:), allocatable, intent(out) :: tokens

! Flag to consider consecutive separators as one (default to .false.)
  logical, optional, intent(in) :: multiple

! Local variables
  integer :: ntokens, itoken
  integer :: idx, jdx, kdx
  logical :: mult

! Check if multiple separators are to be considered as one
  mult = .false.
  if( present(multiple) ) then
    mult = multiple
  end if

! Compute number of tokens
  ntokens = this%token_count( separator, multiple )

! Allocate result
  allocate( tokens(ntokens) )

! Initialise character buffer indexing
  idx = 1

! Loop on the expected tokens
  do itoken = 1, ntokens

!   Look for the separator
    jdx = index( this%buffer(idx:), separator )

!   Process the return index
    if( jdx == 0 ) then

!     Separator not found; last token or no separator in input
      if( idx <= len(this%buffer) ) then
        tokens(itoken) = this%buffer(idx:)
      else
        tokens(itoken) = string('')
      end if

    else

!     Separator found
      kdx = idx + jdx - 2
      if( kdx > 0  ) then
        tokens(itoken) = this%buffer(idx:kdx)
      else
        tokens(itoken) = string('')
      end if

!     If multiple separators are condired as 1, find start of next token
      if( mult ) then
        if( idx+jdx < len(this%buffer) ) then
          do while( this%buffer(idx+jdx:idx+jdx) == separator )
            jdx = jdx + 1
          end do
        end if
      end if

!     Reset the search index
!     jdx stops in the last appearence of the separator
      idx = idx + jdx

    end if

  end do

end subroutine string_split


! Build a string from tokens using the given characters for binding
pure subroutine string_join_char( this, tokens, separator )

! The calling string object
  class(t_string), intent(inout) :: this

! The list of tokens
  type(t_string), dimension(:), intent(in) :: tokens

! The separator character
  character(len=*), intent(in) :: separator

! Local variables
  integer :: itoken

! Initialise
  this%buffer = tokens(1)%buffer

! Loop on the tokens
  do itoken = 2, size(tokens)
    this%buffer = this%buffer // separator // tokens(itoken)%buffer
  end do

end subroutine string_join_char


! Build a string from tokens using the given string for binding
pure subroutine string_join_string( this, tokens, separator )

! The calling string object
  class(t_string), intent(inout) :: this

! The list of tokens
  type(t_string), dimension(:), intent(in) :: tokens

! The separator character
  type(t_string), intent(in) :: separator

! Call the character interface
  call this%join( tokens, separator%character() )
  
end subroutine string_join_string


! General object comparison override
elemental function string_equals( this, other ) result(res)

! The left string
  class(t_string), intent(in) :: this

! The right string
  class(t_object), intent(in) :: other

! Comparison result
  logical :: res

! Compute equality
  select type(other)
    type is(t_string)
      res = ( this%buffer == other%buffer )
    class default
      res = this%t_object%equals(other)
  end select

end function string_equals


! Equality comparison operator (string == string)
elemental function string_equal_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = left%equals( right )

end function string_equal_string


! Equality comparison operator (string == character)
elemental function string_equal_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = ( left%buffer == right )

end function string_equal_char


! Equality comparison operator (character == string)
elemental function char_equal_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = ( left == right%buffer )

end function char_equal_string


! Inequality comparison operator (string /= string)
elemental function string_nonequal_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = .not. left%equals( right )

end function string_nonequal_string


! Inequality comparison operator (string /= character)
elemental function string_nonequal_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = ( left%buffer /= right )

end function string_nonequal_char


! Inequality comparison operator (character /= string)
elemental function char_nonequal_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute equality
  res = ( left /= right%buffer )

end function char_nonequal_string


! Comparison operator 'string > string'
elemental function string_greater_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer > right%buffer )

end function string_greater_string


! Comparison operator 'string > character'
elemental function string_greater_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer > right )

end function string_greater_char


! Comparison operator 'character > string'
elemental function char_greater_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left > right%buffer )

end function char_greater_string


! Comparison operator 'string >= string'
elemental function string_greater_equal_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer >= right%buffer )

end function string_greater_equal_string


! Comparison operator 'string >= character'
elemental function string_greater_equal_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer >= right )

end function string_greater_equal_char


! Comparison operator 'character >= string'
elemental function char_greater_equal_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left >= right%buffer )

end function char_greater_equal_string


! Comparison operator 'string < string'
elemental function string_less_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer < right%buffer )

end function string_less_string


! Comparison operator 'string < character'
elemental function string_less_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer < right )

end function string_less_char


! Comparison operator 'character < string'
elemental function char_less_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left < right%buffer )

end function char_less_string


! Comparison operator 'string <= string'
elemental function string_less_equal_string( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  type(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer <= right%buffer )

end function string_less_equal_string


! Comparison operator 'string <= character'
elemental function string_less_equal_char( left, right ) result(res)

! The left string
  class(t_string), intent(in) :: left

! The right string
  character(len=*), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left%buffer <= right )

end function string_less_equal_char


! Comparison operator 'character <= string'
elemental function char_less_equal_string( left, right ) result(res)

! The left string
  character(len=*), intent(in) :: left

! The right string
  class(t_string), intent(in) :: right

! The comparison result
  logical :: res

! Compute comparison
  res = ( left <= right%buffer )

end function char_less_equal_string


! Comparison function for quicksort
pure function string_less( left, right ) result(res)

! Calling object
  class(t_string), intent(in) :: left

! Other comparison object
  class(t_string), intent(in) :: right

! Comparison result
  logical :: res

! Do the comparison
  res = ( left%buffer < right%buffer )

end function string_less


! Assignment for quicksort
pure subroutine string_assign( left, right )

! Calling object
  class(t_string), intent(inout) :: left

! Other comparison object
  class(t_string), intent(in) :: right

! Do the comparison
  left%buffer = right%buffer

end subroutine string_assign

end module m_string
