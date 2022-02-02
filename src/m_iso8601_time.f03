module m_iso8601_time

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Reference : ISO_8601-2004_E
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : ISO8601 time format
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

  use m_object

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_iso8601_time
  public iso8601_time
  public iso8601_time_now

  public iso8601_hhmmss
  public iso8601_hhmm
  public iso8601_hh
  public iso8601_hhmmss_s
  public iso8601_hhmmss_ss
  public iso8601_hhmmss_sss
  public iso8601_hhmmss_sss_sss
  public iso8601_hhmmss_sss_sss_sss
  public iso8601_zone_none
  public iso8601_zone_hhmm
  public iso8601_zone_hh

  public iso8601_time_base_format, iso8601_time_extended_format

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

! ISO8601 time zone format designators
  integer(kind=1), parameter :: iso8601_zone_none = 0_1
  integer(kind=1), parameter :: iso8601_zone_hhmm = 1_1
  integer(kind=1), parameter :: iso8601_zone_hh   = 2_1

! Time zone designators
  character, parameter :: utc = 'Z'

! ISO8601 format designators (*** for future use ***)
  character, parameter :: time_period_designator = 'P'
  character, parameter :: recurring_period_designator = 'R'

! ISO6801 format separators (*** for future use ***)
  character, parameter :: time_separator = ':'
  character, parameter :: solidus_separator = '/'

! Base/Extended format
  integer, parameter :: iso8601_time_base_format     = 1
  integer, parameter :: iso8601_time_extended_format = 2

! ISO8601 time format designators
! The values shall be multiples of 1 to allow combination with the time desingators and allow
! date/time designatos to be n00+m and make unique combinations of date and time designators
  integer, parameter :: iso8601_hhmmss             =  1
  integer, parameter :: iso8601_hhmm               =  2
  integer, parameter :: iso8601_hh                 =  3
  integer, parameter :: iso8601_hhmmss_s           =  4
  integer, parameter :: iso8601_hhmmss_ss          =  5
  integer, parameter :: iso8601_hhmmss_sss         =  6
  integer, parameter :: iso8601_hhmmss_sss_sss     =  7
  integer, parameter :: iso8601_hhmmss_sss_sss_sss =  8
  character(len=*), parameter, dimension(2,8) :: time_format_base = reshape( [ &
    '3i2.2                      ', 'i2.2,2(":",i2.2)           ', &
    '2i2.2                      ', 'i2.2,":",i2.2              ', &
    'i2.2                       ', 'i2.2                       ', &
    '3i2.2,".",i1.1             ', 'i2.2,2(":",i2.2),".",i1.1  ', &
    '3i2.2,".",i2.2             ', 'i2.2,2(":",i2.2),".",i2.2  ', &
    '3i2.2,".",i3.3             ', 'i2.2,2(":",i2.2),".",i3.3  ', &
    '3i2.2,".",2i3.3            ', 'i2.2,2(":",i2.2),".",2i3.3 ', &
    '3i2.2,".",3i3.3            ', 'i2.2,2(":",i2.2),".",3i3.3 ' ], &
    [ 2, 8 ] )
  integer :: i
  character(len=*), parameter, dimension(2,8) :: time_format = reshape( [ &
    ( '(' // time_format_base(1,i) // ')', '(' // time_format_base(2,i) // ')', i=1, 8 ) ], &
    [ 2, 8 ] )

! Default length for character representation
  integer, parameter :: time_length = 32

! The iso8601 structure
  type, extends(t_object) :: t_iso8601_time
    private

!     Hour of the day
      integer(kind=1) :: hour = 0_1

!     Minute of the hour
      integer(kind=1) :: minute = 0_1

!     Second of the minute
      integer(kind=1) :: second = 0_1

!     Milllisecond of the second
      integer(kind=2) :: millisecond = 0_2

!     Microsecond of the milllisecond
      integer(kind=2) :: microsecond = 0_2

!     Nanosecond of the microsecond
      integer(kind=2) :: nanosecond = 0_2

!     World time zone
      character(len=6) :: time_zone = utc

!     World time zone designator
      integer(kind=1) :: time_zone_designator = iso8601_zone_none

    contains

!     Formatting interfaces
      generic :: to_string => iso8601_time_to_string
      procedure, private :: iso8601_time_to_string

!     Access function interfaces
      generic :: get_hour => iso8601_time_get_hour
      procedure, private :: iso8601_time_get_hour
      generic :: set_hour => iso8601_time_set_hour, &
                             iso8601_time_set_hour_default
      procedure, private :: iso8601_time_set_hour
      procedure, private :: iso8601_time_set_hour_default
      generic :: get_minute => iso8601_time_get_minute
      procedure, private :: iso8601_time_get_minute
      generic :: set_minute => iso8601_time_set_minute, &
                               iso8601_time_set_minute_default
      procedure, private :: iso8601_time_set_minute
      procedure, private :: iso8601_time_set_minute_default
      generic :: get_second => iso8601_time_get_second
      procedure, private :: iso8601_time_get_second
      generic :: set_second => iso8601_time_set_second, &
                               iso8601_time_set_second_default
      procedure, private :: iso8601_time_set_second
      procedure, private :: iso8601_time_set_second_default
      generic :: get_millisecond => iso8601_time_get_millisecond
      procedure, private :: iso8601_time_get_millisecond
      generic :: set_millisecond => iso8601_time_set_millisecond, &
                                    iso8601_time_set_millisecond_default
      procedure, private :: iso8601_time_set_millisecond
      procedure, private :: iso8601_time_set_millisecond_default
      generic :: get_microsecond => iso8601_time_get_microsecond
      procedure, private :: iso8601_time_get_microsecond
      generic :: set_microsecond => iso8601_time_set_microsecond, &
                                    iso8601_time_set_microsecond_default
      procedure, private :: iso8601_time_set_microsecond
      procedure, private :: iso8601_time_set_microsecond_default
      generic :: get_nanosecond => iso8601_time_get_nanosecond
      procedure, private :: iso8601_time_get_nanosecond
      generic :: set_nanosecond => iso8601_time_set_nanosecond, &
                                   iso8601_time_set_nanosecond_default
      procedure, private :: iso8601_time_set_nanosecond
      procedure, private :: iso8601_time_set_nanosecond_default
      generic :: get_time_zone => iso8601_time_get_time_zone
      procedure, private :: iso8601_time_get_time_zone
      generic :: set_time_zone => iso8601_time_set_time_zone
      procedure, private :: iso8601_time_set_time_zone

!     Private functions
      procedure, private :: iso8601_time_zone_to_string

  end type t_iso8601_time

! Constructor interface
  interface iso8601_time
    module procedure iso8601_time_null
    module procedure iso8601_time_specific
    module procedure iso8601_time_default
  end interface iso8601_time

!- End of module variable declarations ----------------------------------------

contains

! Default time constructor
pure function iso8601_time_null() result(res)

! The constructed structure
  type(t_iso8601_time) :: res

! Initialise result
  res = t_iso8601_time()

end function iso8601_time_null


! Time constructor
pure function iso8601_time_specific( hour, minute, second, &
                                     millisecond, microsecond, nanosecond, &
                                     time_zone ) result(res)

! The hour
  integer(kind=1), intent(in) :: hour

! The minute
  integer(kind=1), intent(in) :: minute

! The second
  integer(kind=1), intent(in) :: second

! The millisecond
  integer(kind=2), optional, intent(in) :: millisecond

! The microsecond
  integer(kind=2), optional, intent(in) :: microsecond

! The nanosecond
  integer(kind=2), optional, intent(in) :: nanosecond

! The time zone
  character(len=*), optional, intent(in) :: time_zone

! The constructed structure
  type(t_iso8601_time) :: res

! Initialise the hour
  res%hour = hour

! Initialise the minute
  res%minute = minute

! Initialise the second
  res%second = second

! Initialise the millisecond
  if( present( millisecond ) ) then
    res%millisecond = millisecond

!   Initialise the microsecond
    if( present( microsecond ) ) then
      res%microsecond = microsecond

!     Initialise the nanosecond
      if( present( nanosecond ) ) then
        res%nanosecond = nanosecond
      end if

    end if

  end if

! Initialise the time zone
  if( present( time_zone ) ) then
    res%time_zone = time_zone
    select case(len_trim(time_zone))
      case(:2)
        res%time_zone_designator = iso8601_zone_none
      case(3)
        res%time_zone_designator = iso8601_zone_hh
      case default
        res%time_zone_designator = iso8601_zone_hhmm
    end select
  end if

end function iso8601_time_specific


! Time constructor with default integers
pure function iso8601_time_default( hour, minute, second, &
                                    millisecond, microsecond, nanosecond, &
                                    time_zone ) result(res)

! The hour
  integer, intent(in) :: hour

! The minute
  integer, intent(in) :: minute

! The second
  integer, intent(in) :: second

! The millisecond
  integer, optional, intent(in) :: millisecond

! The microsecond
  integer, optional, intent(in) :: microsecond

! The nanosecond
  integer, optional, intent(in) :: nanosecond

! The time zone
  character(len=*), optional, intent(in) :: time_zone

! The constructed structure
  type(t_iso8601_time) :: res

! Initialise the hour
  res%hour = int(hour,1)

! Initialise the minute
  res%minute = int(minute,1)

! Initialise the second
  res%second = int(second,1)

! Initialise the millisecond
  if( present( millisecond ) ) then
    res%millisecond = int(millisecond,2)

!   Initialise the microsecond
    if( present( microsecond ) ) then
      res%microsecond = int(microsecond,2)

!     Initialise the nanosecond
      if( present( nanosecond ) ) then
        res%nanosecond = int(nanosecond,2)
      end if

    end if

  end if

! Initialise the time zone
  if( present( time_zone ) ) then
    res%time_zone = time_zone
    select case(len_trim(time_zone))
      case(:2)
        res%time_zone_designator = iso8601_zone_none
      case(3)
        res%time_zone_designator = iso8601_zone_hh
      case default
        res%time_zone_designator = iso8601_zone_hhmm
    end select
  end if

end function iso8601_time_default


! Initialise from machine clock
function iso8601_time_now() result(res)

! The constructed structure
  type(t_iso8601_time) :: res

! Local variables
  integer, dimension(8) :: values
  character(len=5) :: zone

! Get the machine time
  call date_and_time( zone=zone, values=values )

! Assign the core return information
  res%hour        = int(values(5),1)
  res%minute      = int(values(6),1)
  res%second      = int(values(7),1)
  res%millisecond = int(values(8),2)

! Assign ancillary return information
  res%time_zone = zone
  res%time_zone_designator = iso8601_zone_hhmm

end function iso8601_time_now


! Format a time
elemental function iso8601_time_to_string( this, format, size, zone ) result(res)

! The ISO8601 structure
  class(t_iso8601_time), intent(in) :: this

! The format flag (optional, default to iso8601_hhmmss)
  integer, optional, intent(in) :: format

! The flag to for base/extended format (optional, default to extended_format)
  integer, optional, intent(in) :: size

! The flag to show the time zone (optional, default to .true.)
  logical, optional, intent(in) :: zone

! The resulting string
  character(len=time_length) :: res

! Local variables
  character(len=32) :: local
  integer :: the_format
  integer :: the_size
  integer :: fmt_idx
  character(len=:), allocatable :: tz

! Check inputs
  if( present(format) ) then
    the_format = format
  else
    the_format = iso8601_hhmmss
  end if
  if( present(size) ) then
    the_size = size
  else
    the_size = iso8601_time_extended_format
  end if


! Generate the output
  fmt_idx = the_format
  select case( fmt_idx )

!   Basic hh:mm:ss
    case( iso8601_hhmmss )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second

!   To milliseconds
    case( iso8601_hhmmss_s )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second, &
             nint( this%millisecond / 100.0 )
    case( iso8601_hhmmss_ss )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second, &
             nint( this%millisecond / 10.0 )
    case( iso8601_hhmmss_sss )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second, &
             this%millisecond

!   To microseconds
    case( iso8601_hhmmss_sss_sss )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second, &
             this%millisecond, this%microsecond

!   To nanoseconds
    case( iso8601_hhmmss_sss_sss_sss )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute, this%second, &
             this%millisecond, this%microsecond, this%nanosecond

!   Simplified hh:mm
    case( iso8601_hhmm )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour, this%minute

!   Minimum hh
    case( iso8601_hh )
      write( local, time_format( the_size, fmt_idx ) ) &
             this%hour

  end select

! Append the zone
  if( present(zone) ) then

!   User selection
    if( zone ) then
      tz = this%iso8601_time_zone_to_string( the_size )
    else
      tz = ''
    end if

  else

!   Default to zone appended
    tz = this%iso8601_time_zone_to_string( the_size )

  end if

! Return the formatted string
  res = trim(local) // trim(tz)

end function iso8601_time_to_string


! Build the zone string
pure function iso8601_time_zone_to_string( this, size ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! Format for the zone string generation
  integer, intent(in) :: size

! The resulting zone string
  character(len=:), allocatable :: res

! Select the configured zone type
  select case( this%time_zone_designator )

!   No specific zone
    case(iso8601_zone_none)
      res = utc

!   Reduce accuracy zone
    case(iso8601_zone_hh)
      res = this%time_zone(:3)

!   Full accuracy zone
    case(iso8601_zone_hhmm)
      if( size == iso8601_time_extended_format ) then
        if( index( this%time_zone, ":" ) == 0 ) then
          res = this%time_zone(:3) // ":" // this%time_zone(4:5)
        else
          res = this%time_zone
        end if
      else
        if( index( this%time_zone, ":" ) /= 0 ) then
          res = this%time_zone(:3) // this%time_zone(5:6)
        else
          res = this%time_zone
        end if
      end if

  end select

end function iso8601_time_zone_to_string


! Get attribute hour
elemental function iso8601_time_get_hour( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%hour

end function iso8601_time_get_hour


! Set attribute hour
elemental subroutine iso8601_time_set_hour( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  this%hour = value

end subroutine iso8601_time_set_hour


! Set attribute hour (from default integer)
elemental subroutine iso8601_time_set_hour_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%hour = int(value,1)

end subroutine iso8601_time_set_hour_default


! Get attribute minute
elemental function iso8601_time_get_minute( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%minute

end function iso8601_time_get_minute


! Set attribute minute
elemental subroutine iso8601_time_set_minute( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  this%minute = value

end subroutine iso8601_time_set_minute


! Set attribute minute (from default integer)
elemental subroutine iso8601_time_set_minute_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%minute = int(value,1)

end subroutine iso8601_time_set_minute_default

! Get attribute second
elemental function iso8601_time_get_second( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%second

end function iso8601_time_get_second

! Set attribute second
elemental subroutine iso8601_time_set_second( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  this%second = value

end subroutine iso8601_time_set_second
elemental subroutine iso8601_time_set_second_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%second = int(value,1)

end subroutine iso8601_time_set_second_default

! Get attribute millisecond
elemental function iso8601_time_get_millisecond( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%millisecond

end function iso8601_time_get_millisecond

! Set attribute millisecond
elemental subroutine iso8601_time_set_millisecond( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  this%millisecond = value

end subroutine iso8601_time_set_millisecond
elemental subroutine iso8601_time_set_millisecond_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%millisecond = int(value,2)

end subroutine iso8601_time_set_millisecond_default

! Get attribute microsecond
elemental function iso8601_time_get_microsecond( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%microsecond

end function iso8601_time_get_microsecond

! Set attribute microsecond
elemental subroutine iso8601_time_set_microsecond( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  this%microsecond = value

end subroutine iso8601_time_set_microsecond
elemental subroutine iso8601_time_set_microsecond_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%microsecond = int(value,2)

end subroutine iso8601_time_set_microsecond_default

! Get attribute nanosecond
elemental function iso8601_time_get_nanosecond( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%nanosecond

end function iso8601_time_get_nanosecond

! Set attribute nanosecond
elemental subroutine iso8601_time_set_nanosecond( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  this%nanosecond = value

end subroutine iso8601_time_set_nanosecond
elemental subroutine iso8601_time_set_nanosecond_default( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%nanosecond = int(value,2)

end subroutine iso8601_time_set_nanosecond_default

! Get attribute time_zone
elemental function iso8601_time_get_time_zone( this ) result(res)

! The data structure
  class(t_iso8601_time), intent(in) :: this

! The parameter value to be returned
  character(len=6) :: res

! Return the value
  res = this%time_zone

end function iso8601_time_get_time_zone

! Set attribute time_zone
elemental subroutine iso8601_time_set_time_zone( this, value )

! The data structure
  class(t_iso8601_time), intent(inout) :: this

! The parameter value to set
  character(len=*), intent(in) :: value

! Set the value
  this%time_zone = value

end subroutine iso8601_time_set_time_zone

end module m_iso8601_time
