module m_iso8601_date_time

!------------------------------------------------------------------------------
! Copyright : 2021, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Reference : ISO_8601-2004_E
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
!>ISO8601 date/time format
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

  use m_iso8601_date
  use m_iso8601_time

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_iso8601_date_time
  public iso8601_date_time
  public iso8601_date_time_now

  public iso8601_YYYYMMDDhhmmss
  public iso8601_YYYYMMDDhhmm
  public iso8601_YYYYMMDDhhmmss_s
  public iso8601_YYYYMMDDhhmmss_ss
  public iso8601_YYYYMMDDhhmmss_sss
  public iso8601_YYYYMMDDhhmmss_sss_sss
  public iso8601_YYYYMMDDhhmmss_sss_sss_sss
  public iso8601_YYYYDDDhhmmss
  public iso8601_YYYYDDDhhmm
  public iso8601_YYYYDDDhhmmss_s
  public iso8601_YYYYDDDhhmmss_ss
  public iso8601_YYYYDDDhhmmss_sss
  public iso8601_YYYYDDDhhmmss_sss_sss
  public iso8601_YYYYDDDhhmmss_sss_sss_sss
  public iso8601_YYYYWwwDhhmmss
  public iso8601_YYYYWwwDhhmm
  public iso8601_YYYYWwwDhhmmss_s
  public iso8601_YYYYWwwDhhmmss_ss
  public iso8601_YYYYWwwDhhmmss_sss
  public iso8601_YYYYWwwDhhmmss_sss_sss
  public iso8601_YYYYWwwDhhmmss_sss_sss_sss

  public iso8601_extended_format, iso8601_base_format

  public iso8601_date_time_is_leap_year

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

!> ISO8601 format designators
  character, parameter :: time_designator = 'T'

!> Base/Extended format
  integer, parameter :: iso8601_base_format     = 1
  integer, parameter :: iso8601_extended_format = 2

!> ISO8601 calendar format designators
  integer, parameter :: iso8601_YYYYMMDDhhmmss             = iso8601_YYYYMMDD + iso8601_hhmmss
  integer, parameter :: iso8601_YYYYMMDDhhmm               = iso8601_YYYYMMDD + iso8601_hhmm
  integer, parameter :: iso8601_YYYYMMDDhh                 = iso8601_YYYYMMDD + iso8601_hh
  integer, parameter :: iso8601_YYYYMMDDhhmmss_s           = iso8601_YYYYMMDD + iso8601_hhmmss_s
  integer, parameter :: iso8601_YYYYMMDDhhmmss_ss          = iso8601_YYYYMMDD + iso8601_hhmmss_ss
  integer, parameter :: iso8601_YYYYMMDDhhmmss_sss         = iso8601_YYYYMMDD + iso8601_hhmmss_sss
  integer, parameter :: iso8601_YYYYMMDDhhmmss_sss_sss     = iso8601_YYYYMMDD + iso8601_hhmmss_sss_sss
  integer, parameter :: iso8601_YYYYMMDDhhmmss_sss_sss_sss = iso8601_YYYYMMDD + iso8601_hhmmss_sss_sss
  integer, parameter :: iso8601_YYYYDDDhhmmss              = iso8601_YYYYDDD  + iso8601_hhmmss
  integer, parameter :: iso8601_YYYYDDDhhmm                = iso8601_YYYYDDD  + iso8601_hhmm
  integer, parameter :: iso8601_YYYYDDDhh                  = iso8601_YYYYDDD  + iso8601_hh
  integer, parameter :: iso8601_YYYYDDDhhmmss_s            = iso8601_YYYYDDD  + iso8601_hhmmss_s
  integer, parameter :: iso8601_YYYYDDDhhmmss_ss           = iso8601_YYYYDDD  + iso8601_hhmmss_ss
  integer, parameter :: iso8601_YYYYDDDhhmmss_sss          = iso8601_YYYYDDD  + iso8601_hhmmss_sss
  integer, parameter :: iso8601_YYYYDDDhhmmss_sss_sss      = iso8601_YYYYDDD  + iso8601_hhmmss_sss_sss
  integer, parameter :: iso8601_YYYYDDDhhmmss_sss_sss_sss  = iso8601_YYYYDDD  + iso8601_hhmmss_sss_sss_sss
  integer, parameter :: iso8601_YYYYWwwDhhmmss             = iso8601_YYYYWwwD + iso8601_hhmmss
  integer, parameter :: iso8601_YYYYWwwDhhmm               = iso8601_YYYYWwwD + iso8601_hhmm
  integer, parameter :: iso8601_YYYYWwwDhh                 = iso8601_YYYYWwwD + iso8601_hh
  integer, parameter :: iso8601_YYYYWwwDhhmmss_s           = iso8601_YYYYWwwD + iso8601_hhmmss_s
  integer, parameter :: iso8601_YYYYWwwDhhmmss_ss          = iso8601_YYYYWwwD + iso8601_hhmmss_ss
  integer, parameter :: iso8601_YYYYWwwDhhmmss_sss         = iso8601_YYYYWwwD + iso8601_hhmmss_sss
  integer, parameter :: iso8601_YYYYWwwDhhmmss_sss_sss     = iso8601_YYYYWwwD + iso8601_hhmmss_sss_sss
  integer, parameter :: iso8601_YYYYWwwDhhmmss_sss_sss_sss = iso8601_YYYYWwwD + iso8601_hhmmss_sss_sss_sss

!> Default length for character representation
  integer, parameter :: date_time_length = 48

!> The iso8601 date/time structure
  type, extends(t_object) :: t_iso8601_date_time
    private

!>     The date structure
      type(t_iso8601_date) :: date

!>     The time structure
      type(t_iso8601_time) :: time

    contains

!     Formatting interfaces
      generic :: to_string => iso8601_date_time_to_string
      procedure, private :: iso8601_date_time_to_string

!     Access function interfaces
      generic :: get_year => iso8601_date_time_get_year
      procedure, private :: iso8601_date_time_get_year
      generic :: set_year => iso8601_date_time_set_year, &
                             iso8601_date_time_set_year_default
      procedure, private :: iso8601_date_time_set_year
      procedure, private :: iso8601_date_time_set_year_default
      generic :: get_month => iso8601_date_time_get_month
      procedure, private :: iso8601_date_time_get_month
      generic :: set_month => iso8601_date_time_set_month, &
                              iso8601_date_time_set_month_default
      procedure, private :: iso8601_date_time_set_month
      procedure, private :: iso8601_date_time_set_month_default
      generic :: get_day => iso8601_date_time_get_day
      procedure, private :: iso8601_date_time_get_day
      generic :: set_day => iso8601_date_time_set_day, &
                            iso8601_date_time_set_day_default
      procedure, private :: iso8601_date_time_set_day
      procedure, private :: iso8601_date_time_set_day_default
      generic :: get_hour => iso8601_date_time_get_hour
      procedure, private :: iso8601_date_time_get_hour
      generic :: set_hour => iso8601_date_time_set_hour, &
                             iso8601_date_time_set_hour_default
      procedure, private :: iso8601_date_time_set_hour
      procedure, private :: iso8601_date_time_set_hour_default
      generic :: get_minute => iso8601_date_time_get_minute
      procedure, private :: iso8601_date_time_get_minute
      generic :: set_minute => iso8601_date_time_set_minute, &
                               iso8601_date_time_set_minute_default
      procedure, private :: iso8601_date_time_set_minute
      procedure, private :: iso8601_date_time_set_minute_default
      generic :: get_second => iso8601_date_time_get_second
      procedure, private :: iso8601_date_time_get_second
      generic :: set_second => iso8601_date_time_set_second, &
                               iso8601_date_time_set_second_default
      procedure, private :: iso8601_date_time_set_second
      procedure, private :: iso8601_date_time_set_second_default
      generic :: get_millisecond => iso8601_date_time_get_millisecond
      procedure, private :: iso8601_date_time_get_millisecond
      generic :: set_millisecond => iso8601_date_time_set_millisecond, &
                                    iso8601_date_time_set_millisecond_default
      procedure, private :: iso8601_date_time_set_millisecond
      procedure, private :: iso8601_date_time_set_millisecond_default
      generic :: get_microsecond => iso8601_date_time_get_microsecond
      procedure, private :: iso8601_date_time_get_microsecond
      generic :: set_microsecond => iso8601_date_time_set_microsecond, &
                                    iso8601_date_time_set_microsecond_default
      procedure, private :: iso8601_date_time_set_microsecond
      procedure, private :: iso8601_date_time_set_microsecond_default
      generic :: get_nanosecond => iso8601_date_time_get_nanosecond
      procedure, private :: iso8601_date_time_get_nanosecond
      generic :: set_nanosecond => iso8601_date_time_set_nanosecond, &
                                   iso8601_date_time_set_nanosecond_default
      procedure, private :: iso8601_date_time_set_nanosecond
      procedure, private :: iso8601_date_time_set_nanosecond_default
      generic :: get_time_zone => iso8601_date_time_get_time_zone
      procedure, private :: iso8601_date_time_get_time_zone
      generic :: set_time_zone => iso8601_date_time_set_time_zone
      procedure, private :: iso8601_date_time_set_time_zone

      procedure :: is_leap_year => iso8601_date_time_is_leap_year

  end type t_iso8601_date_time

!> Constructor interface
  interface iso8601_date_time
    module procedure iso8601_date_time_generic
    module procedure iso8601_date_time_specific
    module procedure iso8601_date_time_default
  end interface iso8601_date_time

!- End of module variable declarations ----------------------------------------

contains

!> Generic date time constructor from date/time structures
pure function iso8601_date_time_generic( date, time ) result(res)

!> The date structure
  type(t_iso8601_date), intent(in) :: date

!> The time structure
  type(t_iso8601_time), optional, intent(in) :: time

!> The constructed structure
  type(t_iso8601_date_time) :: res

! Initialise the date structure
  res%date = date

! Initialise the time structure
  if( present(time) ) then
    res%time = time
  end if

end function iso8601_date_time_generic


!> Generic date/time constructor from details
pure function iso8601_date_time_specific( year, month, day, &
                                          hour, minute, second, &
                                          millisecond, microsecond, nanosecond, &
                                          time_zone ) result(res)

!> The year
  integer(kind=2), intent(in) :: year

!> The month
  integer(kind=1), intent(in) :: month

!> The day
  integer(kind=1), intent(in) :: day

! The hour
  integer(kind=1), optional, intent(in) :: hour

! The minute
  integer(kind=1), optional, intent(in) :: minute

! The second
  integer(kind=1), optional, intent(in) :: second

! The millisecond
  integer(kind=2), optional, intent(in) :: millisecond

! The microsecond
  integer(kind=2), optional, intent(in) :: microsecond

! The nanosecond
  integer(kind=2), optional, intent(in) :: nanosecond

! The time zone
  character(len=*), optional, intent(in) :: time_zone

! The constructed structure
  type(t_iso8601_date_time) :: res

! Local variables
  integer(kind=1) :: h, m, s


! Initialise the date structure
  res%date = iso8601_date( year, month, day )

! Check optionals
  if( present(hour) ) then
    h = hour
  else
    h = 0_1
  end if
  if( present(minute) ) then
    m = minute
  else
    m = 0_1
  end if
  if( present(second) ) then
    s = second
  else
    s = 0_1
  end if

! Initialise the time structure
  res%time = iso8601_time( h, m, s, &
                           millisecond, microsecond, nanosecond, &
                           time_zone )

end function iso8601_date_time_specific


!> Generic date/time constructor with default integers
pure function iso8601_date_time_default( year, month, day, &
                                         hour, minute, second, &
                                         millisecond, microsecond, nanosecond, &
                                         time_zone ) result(res)

!> The year
  integer, intent(in) :: year

!> The month
  integer, intent(in) :: month

!> The day
  integer, intent(in) :: day

! The hour
  integer, optional, intent(in) :: hour

! The minute
  integer, optional, intent(in) :: minute

! The second
  integer, optional, intent(in) :: second

! The millisecond
  integer, optional, intent(in) :: millisecond

! The microsecond
  integer, optional, intent(in) :: microsecond

! The nanosecond
  integer, optional, intent(in) :: nanosecond

! The time zone
  character(len=*), optional, intent(in) :: time_zone

! The constructed structure
  type(t_iso8601_date_time) :: res

! Local variables
  integer :: h, m, s


! Initialise the date structure
  res%date = iso8601_date( year, month, day )

! Check optionals
  if( present(hour) ) then
    h = hour
  else
    h = 0
  end if
  if( present(minute) ) then
    m = minute
  else
    m = 0
  end if
  if( present(second) ) then
    s = second
  else
    s = 0
  end if

! Initialise the time structure
  res%time = iso8601_time( h, m, s, &
                           millisecond, microsecond, nanosecond, &
                           time_zone )

end function iso8601_date_time_default


!> Initialise from machine clock
function iso8601_date_time_now() result(res)

! The constructed structure
  type(t_iso8601_date_time) :: res

! Local variables
  integer, dimension(8) :: values
  character(len=5) :: zone

! Get the machine time
  call date_and_time( zone=zone, values=values )

! Initialise the date structure
  res%date = iso8601_date( values(1), values(2), values(3) )

! Initialise the time structure
  res%time = iso8601_time( values(5), values(6), values(7), &
                           values(8), 0, 0, &
                           zone )

end function iso8601_date_time_now


!> Format a date/time
elemental function iso8601_date_time_to_string( this, format, size, sign, zone ) result(res)

!> The ISO8601 structure
  class(t_iso8601_date_time), intent(in) :: this

!> The format flag (optiona, default to iso8601_YYYYMMDDhhmmss)
  integer, optional, intent(in) :: format

!> The flag to for base/extended format (default to extended_format)
  integer, optional, intent(in) :: size

!> The flag to include the sign before the date (default to .false.)
  logical, optional, intent(in) :: sign

!> The flag to include the time zone (default to .false.)
  logical, optional, intent(in) :: zone

!> The resulting string
  character(len=date_time_length) :: res

! Local variables
  character(len=32) :: local_date, local_time
  integer :: the_format
  integer :: fmt_date_idx, fmt_time_idx

! Check inputs
  if( present(format) ) then
    the_format = format
  else
    the_format = iso8601_YYYYMMDDhhmmss
  end if

! Compute indexes for date and time
  fmt_date_idx = 100 * ( the_format / 100 )
  fmt_time_idx = the_format - fmt_date_idx

! to_string the date
  local_date = this%date%to_string( fmt_date_idx, size, sign )

! to_string the time
  local_time = this%time%to_string( fmt_time_idx, size, zone )

! to_string the date/time
  res = trim(local_date) // time_designator // trim(local_time)

end function iso8601_date_time_to_string


!> Check for leap year (type dound)
pure function iso8601_date_time_is_leap_year( this ) result(res)

!> Calling object
  class(t_iso8601_date_time), intent(in) :: this

!> Flag for leap year
  logical :: res

! Compute from static function
  res = this%date%is_leap_year()

end function iso8601_date_time_is_leap_year


!> Get attribute year
elemental function iso8601_date_time_get_year( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%date%get_year()

end function iso8601_date_time_get_year


!> Set attribute year
elemental subroutine iso8601_date_time_set_year( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  call this%date%set_year( value )

end subroutine iso8601_date_time_set_year


!> Set attribute year (from default integer)
elemental subroutine iso8601_date_time_set_year_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%date%set_year( int(value,2) )

end subroutine iso8601_date_time_set_year_default


!> Get attribute month
elemental function iso8601_date_time_get_month( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%date%get_month()

end function iso8601_date_time_get_month


!> Set attribute month
elemental subroutine iso8601_date_time_set_month( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  call this%date%set_month( value )

end subroutine iso8601_date_time_set_month


!> Set attribute month (from default integer)
elemental subroutine iso8601_date_time_set_month_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%date%set_month( int(value,1) )

end subroutine iso8601_date_time_set_month_default


!> Get attribute day
elemental function iso8601_date_time_get_day( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%date%get_day()

end function iso8601_date_time_get_day


!> Set attribute day
elemental subroutine iso8601_date_time_set_day( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  call this%date%set_day( value )

end subroutine iso8601_date_time_set_day


!> Set attribute day (from default integer)
elemental subroutine iso8601_date_time_set_day_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%date%set_day( int(value,1) )

end subroutine iso8601_date_time_set_day_default


!> Get attribute hour
elemental function iso8601_date_time_get_hour( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%time%get_hour()

end function iso8601_date_time_get_hour


!> Set attribute hour
elemental subroutine iso8601_date_time_set_hour( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  call this%time%set_hour( value )

end subroutine iso8601_date_time_set_hour


!> Set attribute hour (from default integer)
elemental subroutine iso8601_date_time_set_hour_default( this, value )


!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_hour( int(value,1) )

end subroutine iso8601_date_time_set_hour_default


!> Get attribute minute
elemental function iso8601_date_time_get_minute( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%time%get_minute()

end function iso8601_date_time_get_minute


!> Set attribute minute
elemental subroutine iso8601_date_time_set_minute( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  call this%time%set_minute( value )

end subroutine iso8601_date_time_set_minute


!> Set attribute minute (from default integer)
elemental subroutine iso8601_date_time_set_minute_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_minute( int(value,1) )

end subroutine iso8601_date_time_set_minute_default


!> Get attribute second
elemental function iso8601_date_time_get_second( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%time%get_second()

end function iso8601_date_time_get_second


!> Set attribute second
elemental subroutine iso8601_date_time_set_second( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  call this%time%set_second( value )

end subroutine iso8601_date_time_set_second


!> Set attribute second (from default integer)
elemental subroutine iso8601_date_time_set_second_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_second( int(value,1) )

end subroutine iso8601_date_time_set_second_default


!> Get attribute millisecond
elemental function iso8601_date_time_get_millisecond( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%time%get_millisecond()

end function iso8601_date_time_get_millisecond


!> Set attribute millisecond
elemental subroutine iso8601_date_time_set_millisecond( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  call this%time%set_millisecond( value )

end subroutine iso8601_date_time_set_millisecond


!> Set attribute millisecond (from default integer)
elemental subroutine iso8601_date_time_set_millisecond_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_millisecond( int(value,2) )

end subroutine iso8601_date_time_set_millisecond_default


!> Get attribute microsecond
elemental function iso8601_date_time_get_microsecond( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%time%get_microsecond()

end function iso8601_date_time_get_microsecond


!> Set attribute microsecond
elemental subroutine iso8601_date_time_set_microsecond( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  call this%time%set_microsecond( value )

end subroutine iso8601_date_time_set_microsecond


!> Set attribute microsecond (from default integer)
elemental subroutine iso8601_date_time_set_microsecond_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_microsecond( int(value,2) )

end subroutine iso8601_date_time_set_microsecond_default


!> Get attribute nanosecond
elemental function iso8601_date_time_get_nanosecond( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%time%get_nanosecond()

end function iso8601_date_time_get_nanosecond


!> Set attribute nanosecond
elemental subroutine iso8601_date_time_set_nanosecond( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  call this%time%set_nanosecond( value )

end subroutine iso8601_date_time_set_nanosecond


!> Set attribute nanosecond (from default integer)
elemental subroutine iso8601_date_time_set_nanosecond_default( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  integer, intent(in) :: value

! Set the value
  call this%time%set_nanosecond( int(value,2) )

end subroutine iso8601_date_time_set_nanosecond_default


!> Get attribute time_zone
elemental function iso8601_date_time_get_time_zone( this ) result(res)

!> The data structure
  class(t_iso8601_date_time), intent(in) :: this

!> The parameter value to be returned
  character(len=6) :: res

! Return the value
  res = this%time%get_time_zone()

end function iso8601_date_time_get_time_zone


!> Set attribute time_zone
elemental subroutine iso8601_date_time_set_time_zone( this, value )

!> The data structure
  class(t_iso8601_date_time), intent(inout) :: this

!> The parameter value to set
  character(len=*), intent(in) :: value

! Set the value
  call this%time%set_time_zone( value )

end subroutine iso8601_date_time_set_time_zone

end module m_iso8601_date_time
