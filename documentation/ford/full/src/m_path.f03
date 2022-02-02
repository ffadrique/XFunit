module m_path

!------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>Module for the implementation of path management
!>             The extension from string holds the path of the file or directory
!>             Both forward and backward separator slashes are managed seamlessly
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

  use m_string

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_path, path
  public path_unix_slash, path_windows_slash

  public path_directory, path_file_name, path_name, path_is_absolute
  public path_extension, path_change_extension, path_has_extension
  public path_exists, path_normalise
  public path_temp_path, path_temp_file_name

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

!> Path separation symbols
  character, parameter :: path_unix_slash = '/'
  character, parameter :: path_windows_slash = '\'
  character, parameter :: dot = '.'
  character, parameter :: colon = ':'

!> The path class
!> The extension from string holds the path of the file or directory
  type, extends(t_string) :: t_path
    private

    contains

!>     Directroy of the file path in the path (no trailing path separator)
      procedure :: directory => path_directory_method

!>     File name of the file path in the path (with extension)
      procedure :: file_name => path_file_name_method

!>     File name of the file path in the path (without extension)
      procedure :: name => path_name_method

!>     Check if the path is an absolute path representation
      procedure :: is_absolute => path_is_absolute_method

!>     File extension of the file in the path
      procedure :: extension => path_extension_method

!>     Determines whether a path includes a file name extension
      procedure :: has_extension => path_has_extension_method

!     Change file extension of the file in the path
      generic :: change_extension => path_change_extension_string_method, &
                                     path_change_extension_character_method
      procedure, private :: path_change_extension_string_method
      procedure, private :: path_change_extension_character_method

!>     Check if path exists
      procedure :: exists => path_exists_method

!>     Normalise the path
      procedure :: normalise => path_normalise_method

!>     Return the path as a string
      procedure :: to_string => path_to_string

  end type t_path

!> Constructor interface
  interface path
    module procedure path_default
    module procedure path_character
    module procedure path_string
    module procedure path_array_character
    module procedure path_array_string
  end interface path

!> Static interface for path exists
  interface path_exists
    module procedure path_exists_string_static
    module procedure path_exists_character_static
  end interface path_exists

!> Static interface for directory extrraction
  interface path_directory
    module procedure path_directory_string_static
    module procedure path_directory_character_static
  end interface path_directory

!> Static interface for file name (with extension)
  interface path_file_name
    module procedure path_file_name_string_static
    module procedure path_file_name_character_static
  end interface path_file_name

!> Static interface for name (without extension)
  interface path_name
    module procedure path_name_string_static
    module procedure path_name_character_static
  end interface path_name

!> Static interface to check absolute path representation
  interface path_is_absolute
    module procedure path_is_absolute_string_static
    module procedure path_is_absolute_character_static
  end interface path_is_absolute

!> Static interface for extension
  interface path_extension
    module procedure path_extension_string_static
    module procedure path_extension_character_static
  end interface path_extension

!> Determines whether a path includes a file name extension
  interface path_has_extension
    module procedure path_has_extension_string_static
    module procedure path_has_extension_character_static
  end interface path_has_extension

!> Static interface for change extension
  interface path_change_extension
    module procedure path_change_extension_string_static
    module procedure path_change_extension_character_static
  end interface path_change_extension

!> Static interface for normalise
  interface path_normalise
    module procedure path_normalise_string_static
    module procedure path_normalise_character_static
  end interface path_normalise

!> Static interface for temp_path
  interface path_temp_path
    module procedure path_temp_path_static
  end interface path_temp_path

!> Static interface for temp_path
  interface path_temp_file_name
    module procedure path_temp_file_name
  end interface path_temp_file_name

!- End of module variable declarations ----------------------------------------

contains

!> Constructor
elemental function path_default() result(res)

! The path
  type(t_path) :: res

! Default to current (.)
  res = dot

end function path_default


!> Constructor from character
elemental function path_character( path ) result(res)

!> The file name
  character(len=*), intent(in) :: path

!> The path
  type(t_path) :: res

! Initialise result
  res%t_string = string(trim(path))

! Normalise the path
  call res%normalise()

end function path_character


!> Constructor from string
elemental function path_string( path ) result(res)

!> The file name (polymorphic allows passing also t_path)
  class(t_string), intent(in) :: path

!> The path
  type(t_path) :: res

! Invoke the character interface
  res = path_character( path%character() )

end function path_string


!> Constructor from array of strings
pure function path_array_character( tokens ) result(res)

!> The array of strings
  character(len=*), dimension(:), intent(in) :: tokens

!> The path
  type(t_path) :: res

! Local variables
  integer :: i

! Use the string interface
  res = path( [ (string(trim(tokens(i))), i = 1, size(tokens) ) ] )

end function path_array_character


!> Constructor from array of strings
pure function path_array_string( tokens ) result(res)

!> The array of strings (polymorphic allows passing also t_path)
  class(t_string), dimension(:), intent(in) :: tokens

!> The path
  type(t_path) :: res

! Join the array of strings
  call res%join( tokens, path_unix_slash )

! Normalise the path
  call res%normalise()

end function path_array_string


!> Check if file in handler exists
function path_exists_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The status flag
  logical :: res

! Call the static method
  res = path_exists( this%t_string )

end function path_exists_method


!> Check if file in handler exists (string interface)
function path_exists_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The status flag
  logical :: res

! Call the character interface
  res = path_exists( path%character() )

end function path_exists_string_static


!> Check if file in handler exists (character interface)
function path_exists_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The status flag
  logical :: res

! Check if the file exists
  inquire( file=trim(path), exist=res )

end function path_exists_character_static


!> Get the directroy (folder of file)
elemental function path_directory_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The file path (without last slash)
  type(t_string) :: res

! Call the static function
  res = path_directory( this%t_string )

end function path_directory_method


!> Get the directroy (folder of file; string interface)
elemental function path_directory_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The file path (without last slash)
  type(t_string) :: res

! Call the character interface
  res = string( path_directory( path%character() ) )

end function path_directory_string_static


!> Get the directroy (folder of file; character interface)
elemental function path_directory_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The file path (without last slash)
  character(len=len(path)) :: res

! Local variables
  integer :: islash

! Locate the last slash in the full path
  islash = max( index( path, path_unix_slash, back=.true. ), &
                index( path, path_windows_slash, back=.true. ) )

! Set the path
  select case( islash )
    case(0)
      res = ''
    case default
      res = path(:islash-1)
  end select

end function path_directory_character_static


!> Get the file name (includes extension)
elemental function path_file_name_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The file path (without last slash)
  type(t_string) :: res

! Call the static function
  res = path_file_name( this%t_string )

end function path_file_name_method


!> Get the file name (includes extension; string interface)
elemental function path_file_name_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The file path (without last slash)
  type(t_string) :: res

! Call the character interface
  res = string( path_file_name( path%character() ) )

end function path_file_name_string_static


!> Get the file name (includes extension; character interface)
elemental function path_file_name_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The file path (without last slash)
  character(len=len(path)) :: res

! Local variables
  integer :: islash

! Locate the last slash in the full path
  islash = max( index( path, path_unix_slash, back=.true. ), &
                index( path, path_windows_slash, back=.true. ) )

! Set the path
  select case( islash )
    case(0)
      res = path
    case default
      res = path(islash+1:)
  end select

end function path_file_name_character_static


!> Get the file name (without extension)
elemental function path_name_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The file path (without last slash)
  type(t_string) :: res

! Call the static function
  res = path_name( this%t_string )

end function path_name_method


!> Get the file name (without extension; string interface)
elemental function path_name_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The file path (without last slash)
  type(t_string) :: res

! Call the character interface
  res = path_name( path%character() )

end function path_name_string_static


!> Get the file name (without extension; character interface)
elemental function path_name_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The file path (without last slash)
  character(len=len(path)) :: res

! Local variables
  integer :: idot
  character(len=:), allocatable :: local

! Initialise local variables
  local = path_file_name( path )

! Look for the last dot in the file name
  idot = index( local, dot, back=.true. )

! Set the rootname
  select case( idot )
    case(0)
      res = local
    case default
      res = local(:idot-1)
  end select

end function path_name_character_static


!> Check if path is an absolute path representation
elemental function path_is_absolute_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The absolute path flag
  logical :: res

! Call the static function
  res = path_is_absolute( this%t_string )

end function path_is_absolute_method


!> Check if path is an absolute path representation (string interface)
elemental function path_is_absolute_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The absolute path flag
  logical :: res

! Call the character interface
  res = path_is_absolute( path%character() )

end function path_is_absolute_string_static


!> Check if path is an absolute path representation (character interface)
elemental function path_is_absolute_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The absolute path flag
  logical :: res

! Local variables
  character(len=2) :: root

! Initialise root
  root = path(1:2)

! Check the condition for absolute path
  res = ( root(1:1) == path_windows_slash ) .or. &
        ( root(1:1) == path_unix_slash ) .or. &
        ( root(2:2) == colon )

end function path_is_absolute_character_static


!> Get the file extension
elemental function path_extension_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The file path (without last slash)
  type(t_string) :: res

! Call the static function
  res = path_extension( this%t_string )

end function path_extension_method


!> Get the file extension (string interface)
elemental function path_extension_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The file path (without last slash)
  type(t_string) :: res

! Call the character interface
  res = string( path_extension( path%character() ) )

end function path_extension_string_static


!> Get the file extension (character interface)
elemental function path_extension_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The file path (without last slash)
  character(len=len(path)) :: res

! Local variables
  integer :: idot
  character(len=:), allocatable :: local

! Initialise local variables
  local = path_file_name( path )

! Look for the last dot in the file name
  idot = index( local, dot, back=.true. )

! Set the extension
  select case( idot )
    case(0)
      res = ''
    case default
      res = local(idot+1:)
  end select

end function path_extension_character_static


!> Determines whether a path includes a file name extension
elemental function path_has_extension_method( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> Has extension flag)
  logical :: res

! Call the static function
  res = path_has_extension( this%t_string )

end function path_has_extension_method


!> Determines whether a path includes a file name extension (string interface)
elemental function path_has_extension_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> Has extension flag)
  logical :: res

! Call the character interface
  res = path_has_extension( path%character() )

end function path_has_extension_string_static


!> Determines whether a path includes a file name extension (character interface)
elemental function path_has_extension_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> Has extension flag)
  logical :: res

! Local variables
  integer :: idot
  type(t_string) :: local

! Initialise local variables
  local = path_file_name( path )

! Look for the last dot in the file name
  idot = local%index( dot, back=.true. )

! Check the extension presence
  res = ( idot > 0 )

end function path_has_extension_character_static


!> Change path extension (string interface)
!> Takes the characters after the last dot provided in the nex extension.
!> If there are more than one dot, all leading characters prior to the vey last dot are ignored
elemental subroutine path_change_extension_string_method( this, extension )

!> The path
  class(t_path), intent(inout) :: this

!> The new extension
  type(t_string), intent(in) :: extension

! Call the static function
  this%t_string = path_change_extension( this%t_string, extension )

end subroutine path_change_extension_string_method


!> Change path extension (character interface)
!> Takes the characters after the last dot provided in the nex extension.
!> If there are more than one dot, all leading characters prior to the vey last dot are ignored
elemental subroutine path_change_extension_character_method( this, extension )

!> The path
  class(t_path), intent(inout) :: this

!> The new extension
  character(len=*), intent(in) :: extension

! Call the static function
  this%t_string = path_change_extension( this%t_string, string(extension) )

end subroutine path_change_extension_character_method


!> Change path extension (string interface)
!> Takes the characters after the last dot provided in the nex extension.
!> If there are more than one dot, all leading characters prior to the vey last dot are ignored
elemental function path_change_extension_string_static( path, extension ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The new extension
  type(t_string), intent(in) :: extension

!> The path with the modified extension)
  type(t_string) :: res

! Call the character interface
  res = path_change_extension( path%character(), extension%character() )

end function path_change_extension_string_static


!> Change path extension (character interface)
!> Takes the characters after the last dot provided in the nex extension.
!> If there are more than one dot, all leading characters prior to the vey last dot are ignored
elemental function path_change_extension_character_static( path, extension ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The new extension
  character(len=*), intent(in) :: extension

!> The path with the modified extension)
  character(len=len(path)+len(extension)) :: res

! Local variables
  integer :: idot
  character(len=:), allocatable :: ext, local

! Check if the provided extension has a dot
  idot = index( extension, dot, back=.true. )
  select case( idot )
    case(0)
      ext = extension
    case default
      ext = extension(idot+1:)
    end select

! Look for the last dot in the path
  local = path_file_name( path )
  idot = index( local, dot, back=.true. )

! Reconstruct the path with the new extension
  select case( idot )
    case(0)
      res = trim(path) // dot // ext
    case default
      idot = index( path, dot, back=.true. )
      res = path(:idot) // ext
    end select

end function path_change_extension_character_static


!> Normalise a file path
elemental subroutine path_normalise_method( this )

!> The path
  class(t_path), intent(inout) :: this

! Call the static function
  this%t_string = path_normalise( this%t_string )

end subroutine path_normalise_method


!> Normalise a file path (string interface)
elemental function path_normalise_string_static( path ) result(res)

!> The path
  type(t_string), intent(in) :: path

!> The normalised path
  type(t_string) :: res

! Call the character interface
  res = string( path_normalise( path%character() ) )

end function path_normalise_string_static


!> Normalise a file path (character interface)
elemental function path_normalise_character_static( path ) result(res)

!> The path
  character(len=*), intent(in) :: path

!> The normalised path
  type(t_string) :: res

! Local variables
  type(t_string), dimension(:), allocatable :: tokenlist, packed_tokenlist
  character(len=:), allocatable :: ctoken
  integer :: ntoken
  integer :: i, j
  logical :: absolute
  type(t_string) :: local

! Initialise
  local = string( adjustl(path) )

! Normalise slashes to unix like slash
  local = local%replace( path_windows_slash, path_unix_slash )

! Check absolute path
! This also works in Windows because if the start is x: then it would be considered as relative
! and normalisation will take the x: as the first relative token
  absolute = ( local%character(1,1) == path_unix_slash )

! Tokenize the path
  call local%split( path_unix_slash, tokenlist, .true. )

! Look for the first token that is not '..' or '.'
  do j = 1, size(tokenlist)

!   Check the token
    ctoken = tokenlist(j)%character()
    select case( ctoken )

!     Nullify self token; if not first token
      case( '.' )
        if( j /= 1 ) tokenlist(j) = ''

!     Skip token reference to parent level
      case( '..' )

!     Default is a non '.' or '..' token; exit loop
      case default
        exit

    end select

  end do

! Process the tokens
  do i = j, size(tokenlist)

!   Check for tokens requiring specific action
    select case( tokenlist(i)%character() )

!     Check for self token
      case( '.' )

!       Nullify current token
        tokenlist(i) = ''

!     Check for reference to parent level
      case( '..' )

!       Nullify the previous and current token
        tokenlist(i-1) = ''
        tokenlist(i) = ''

    end select

  end do

! Reconstruct the path from the tokens (protect from no tokens)
  ntoken = count(tokenlist /= '')
  if( ntoken > 0 ) then
!    allocate( packed_tokenlist, source=pack( tokenlist, tokenlist /= '' ) )
    call local%join( tokenlist, path_unix_slash )
  else
    local = ''
  end if

! Reconstruct the path including the absolute condition
  if( absolute ) then
    res = path_unix_slash // local%character()
  else
    res = local%character()
  end if

end function path_normalise_character_static


!> Get the path of the system temporary folder (defined by env(TMP))
function path_temp_path_static() result(res)

! System temporary path (empty if not defined)
  type(t_string) :: res

! Local variables
  character(len=:), allocatable :: tmpenv
  integer :: tmpenvlen

! Get the value of the environment varialble TMP
  call get_environment_variable( 'TMP', length=tmpenvlen )
  allocate( character(len=tmpenvlen) :: tmpenv )
  call get_environment_variable( 'TMP', value=tmpenv )

! Return the path
  res = string(tmpenv)

end function path_temp_path_static


!> Returns the full path of a unique named temporary file
function path_temp_file_name() result(res)

! Full path to the temporary file
  type(t_string) :: res

! Local variables
  type(t_string) :: tmppath
  real, dimension(10) :: rands
  character(len=size(rands)) :: tmpfilename
  integer :: i
  integer :: idx
  logical, save :: first = .true.

! Initialise the random number generator (only once to avoid same name if too fast consecutive calls)
  if( first ) then
    call random_seed()
    first = .false.
  end if

! Get random number for the generation of the unique name (first must be a letter)
  call random_number( rands(1) )
  idx = int( 25 * rands(1) + 65, kind=4 )
  tmpfilename(1:1) = char(idx)
  do i = 2, size(rands)
    call random_number( rands(i) )
    idx = int( 25 * rands(i) + 65, kind=4 )
    tmpfilename(i:i) = char(idx)
  end do

! Get the temporary path
  tmppath = path_temp_path()

! Get the full path for the temporary file name
  call res%join( [ tmppath, string(tmpfilename//'.tmp') ], path_unix_slash )

end function path_temp_file_name


!> Get the full file path as a string
elemental function path_to_string( this ) result(res)

!> The path
  class(t_path), intent(in) :: this

!> The file path (without last slash)
  type(t_string) :: res

! Set the full file path
  res = this%t_string

end function path_to_string

end module m_path
