module m_xfunit_assertion_files

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests file comparison assertion
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

  use, intrinsic :: iso_fortran_env

  use m_string
  use m_file_handler
  use m_util_convert

  use m_xfunit_assertion
  use m_xml
  use m_msg

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_xfunit_assertion_files
  public xfunit_assertion_files

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! The file comparison assertion type
  type, extends(t_xfunit_assertion) :: t_xfunit_assertion_files
    private

!     Actual asserted value
      type(t_string) :: actual

!     Expected value
      type(t_string) :: expected

    contains

!     Assignment
      procedure :: xfunit_assertion_assign => xfunit_assertion_files_assign

!     Serialization interface (overrides abstract base; same name required)
      procedure :: write_xml => xfunit_assertion_files_write_xml
      procedure :: write => xfunit_assertion_files_write

  end type t_xfunit_assertion_files


!---End of declaration of module variables--------------------------------------

contains

! Constructor for files comparison assertion
function xfunit_assertion_files( name, actual, expected, binary ) result(res)

! The assertion name
  character(len=*), intent(in)  :: name

! The assertion actual value (the filename)
  character(len=*), intent(in)  :: actual

! The assertion expected value (the filename)
  character(len=*), intent(in)  :: expected

! Implement a binary comparison
  logical, optional, intent(in) :: binary

! The returned assertion
  type(t_xfunit_assertion_files) :: res

! Local variables
  integer :: status
  type(t_msg) :: amsg
  type(t_file_handler) :: actual_handler, expected_handler
  logical :: bin

! Initialise
  status = xfunit_assertion_is_pass
  if( present(binary) ) then
    bin = binary
  else
    bin = .false.
  end if

! Initialise actual file handler
  actual_handler = file_handler( actual )
  if( .true. .or. actual_handler%exists() ) then

!   Initialise expected file handler
    expected_handler = file_handler( expected )
    if( .true. .or. expected_handler%exists() ) then

!     Evaluate the assertion
      if( bin ) then

!       Implement binary comparison
        call xfunit_assertion_files_compare_binary( actual_handler, expected_handler, amsg )

      else

!       Implement text comparison
        call xfunit_assertion_files_compare_text( actual_handler, expected_handler, amsg )

      end if

    else

!     Store error
      amsg = msg( 1, 'Cannot find expected file: '//trim(expected), &
                     'm_xfunit_assertion_files', &
                     'xfunit_assertion_files_character' )

    end if

  else

!   Store error
    amsg = msg( 2, 'Cannot find actual file: '//trim(actual), &
                   'm_xfunit_assertion_files', &
                   'xfunit_assertion_files_character' )

  end if

! Process assertion status
  if( amsg%get_code() /= 0 ) then
    call res%set_error( amsg )
    status = xfunit_assertion_is_fail
  end if

! Store the assertion information
  res%t_xfunit_assertion = xfunit_assertion( name, xfunit_assertion_files_index, status )
  res%actual   = actual
  res%expected = expected

end function xfunit_assertion_files


! Compare two text files
subroutine xfunit_assertion_files_compare_text( ahandler, ehandler, amsg )

! The handler to the actual file
  type(t_file_handler), intent(inout) :: ahandler

! The handler to the expected file
  type(t_file_handler), intent(inout) :: ehandler

! Error handling
  type(t_msg) :: amsg

! Buffers
  character(len=8*1024) :: arec, erec

! Local variables
  character(len=:), allocatable :: fname
  integer :: aunit, eunit
  integer :: aios, eios
  character(len=:), allocatable :: fmt

! Open the actual file
  call ahandler%open( write=.false. )
  if( ahandler%is_open() ) then

!   Open the expected file
    call ehandler%open( write=.false. )
    if( ehandler%is_open() ) then

!     Get fortran logical units
      aunit = ahandler%get_unit()
      eunit = ehandler%get_unit()

!     Build read format
      fmt = '(a' // trim(character(len(arec))) // ')'

!     Loop reading records in the buffer
      do

!       Read actual file
        read( aunit, fmt, advance='no', iostat=aios ) arec
        read( eunit, fmt, advance='no', iostat=eios ) erec

!       Check the read buffers
        if( arec /= erec ) then

!         Generate error message and exit
          amsg = msg( 3, 'Files do not compare', 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_text' )
          exit

        end if

!       Terminate the loop on end of file
        if( aios == iostat_end .or. eios == iostat_end ) exit

      end do

!     Close the expected file
      call ehandler%close()

    else

!     Report error on actual file opne
      fname = trim(ehandler%get_file_path())
      amsg = msg( 2, 'Cannot open expected file: ' // fname, 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_text' )

    end if

!   Close the actual file
    call ahandler%close()

  else

!   Report error on actual file opne
    fname = trim(ahandler%get_file_path())
    amsg = msg( 1, 'Cannot open actual file: ' // fname, 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_text' )

  end if

end subroutine xfunit_assertion_files_compare_text


! Compare two binary files
subroutine xfunit_assertion_files_compare_binary( ahandler, ehandler, amsg )

! The handler to the actual file
  type(t_file_handler), intent(inout) :: ahandler

! The handler to the expected file
  type(t_file_handler), intent(inout) :: ehandler

! Error handling
  type(t_msg) :: amsg

! Buffers
  integer(kind=1), dimension(8*1024) :: arec, erec

! Local variables
  character(len=:), allocatable :: fname
  integer :: aunit, eunit
  integer :: aios, eios

! Initialise
  arec = 0_1
  erec = 0_1

! Open the actual file
  call ahandler%open( write=.false., stream=.true., binary=.true. )
  if( ahandler%is_open() ) then

!   Open the expected file
    call ehandler%open( write=.false., stream=.true., binary=.true. )
    if( ehandler%is_open() ) then

!     Get fortran logical units
      aunit = ahandler%get_unit()
      eunit = ehandler%get_unit()

!     Loop reading records in the buffer
      do

!       Read actual file
        read( aunit, iostat=aios ) arec
        read( eunit, iostat=eios ) erec

!       Check the read buffers
        if( any( arec /= erec ) ) then

!         Generate error message and exit
          amsg = msg( 3, 'Files do not compare', 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_binary' )
          exit

        end if

!       Terminate the loop on end of file
        if( aios == iostat_end .or. eios == iostat_end ) exit

      end do

!     Close the expected file
      call ehandler%close()

    else

!     Report error on actual file opne
      fname = trim(ehandler%get_file_path())
      amsg = msg( 2, 'Cannot open expected file: ' // fname, 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_binary' )

    end if

!   Close the actual file
    call ahandler%close()

  else

!   Report error on actual file opne
    fname = trim(ahandler%get_file_path())
    amsg = msg( 1, 'Cannot open actual file: ' // fname, 'm_xfunit_assertion_files', 'xfunit_assertion_files_compare_binary' )

  end if

end subroutine xfunit_assertion_files_compare_binary


! Assignment
elemental subroutine xfunit_assertion_files_assign( this, other )

! The target assertion
  class(t_xfunit_assertion_files), intent(inout) :: this

! The source assertion
  class(t_xfunit_assertion), intent(in) :: other

! Cast the other object
  select type(other)
    type is(t_xfunit_assertion_files)

!     Assign elements
      this%t_xfunit_assertion = other%t_xfunit_assertion
      this%actual = other%actual
      this%expected = other%expected

  end select

end subroutine xfunit_assertion_files_assign


! Serialize in XML
subroutine xfunit_assertion_files_write_xml( this, xml )

! The assertion
  class(t_xfunit_assertion_files), intent(in) :: this

! The XML context structure
  type(t_xml_writer), intent(inout) :: xml

! Local variables
  character(len=256) :: fname
  integer :: idx

! Serialize start tag
  call this%write_xml_start_tag( xml )

! Add details
  fname = trim(character(this%actual))
  idx = index( fname, '/', back=.true. )
  if( idx > 0 ) then
    fname = fname(idx+1:)
  end if
  call xml%write_terminal( 'actual', trim(fname) )
  fname = trim(character(this%expected))
  idx = index( fname, '/', back=.true. )
  if( idx > 0 ) then
    fname = fname(idx+1:)
  end if
  call xml%write_terminal( 'expected', trim(fname) )

! Serializa end tag
  call this%write_xml_end_tag( xml )

end subroutine xfunit_assertion_files_write_xml


! Serialize in plain text
subroutine xfunit_assertion_files_write( this, unit )

! The assertion
  class(t_xfunit_assertion_files), intent(in) :: this

! The open fortran unit
  integer, intent(in) :: unit

! Write the assertion header
  call this%write_header( unit )

! Write the assertion details
  write( unit, '(2x,a,1x,a)' ) 'actual:  ', trim(character(this%actual))
  write( unit, '(2x,a,1x,a)' ) 'expected:', trim(character(this%expected))

! Write the assertion footer
  call this%write_footer( unit )

end subroutine xfunit_assertion_files_write

end module m_xfunit_assertion_files

