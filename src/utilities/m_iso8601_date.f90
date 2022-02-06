module m_iso8601_date

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Reference : ISO_8601-2004_E
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : ISO8601 date format
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
  public t_iso8601_date
  public iso8601_date

  public iso8601_date_now
  public iso8601_YYYYMMDD
  public iso8601_YYYYMM
  public iso8601_YYYY
  public iso8601_YY
  public iso8601_YYYYDDD
  public iso8601_YYYYWwwD
  public iso8601_YYYYWww

  public iso8601_date_extended_format, iso8601_date_base_format

  public iso8601_date_is_leap_year, iso8601_date_days_in_month

!- End of Public/Private declarations -----------------------------------------

  character(len=130), parameter, private :: sccs_info = &
     '$Id: $'

!- Start of module variable declarations --------------------------------------

! ISO6801 format separators
  character, parameter :: date_separator = '-'

! Base/Extended format
  integer, parameter :: iso8601_date_base_format     = 1
  integer, parameter :: iso8601_date_extended_format = 2

! ISO8601 date format designators
! The values shall be multiple of 100 to allow combination with the time desingators and allow
! date/time designatos to be n00+m and make unique combinations of date and time designators
  integer, parameter :: iso8601_YYYYMMDD =  100
  integer, parameter :: iso8601_YYYYMM   =  200
  integer, parameter :: iso8601_YYYY     =  300
  integer, parameter :: iso8601_YY       =  400
  integer, parameter :: iso8601_YYYYDDD  =  500
  integer, parameter :: iso8601_YYYYWwwD =  600
  integer, parameter :: iso8601_YYYYWww  =  700
  character(len=*), parameter, dimension(2,7) :: date_format_base = reshape( (/ &
    'i4.4,2i2.2             ', 'i4.4,2("-",i2.2)       ', &
    'i4.4,i2.2              ', 'i4.4,"-",i2.2          ', &
    'i4.4                   ', 'i4.4                   ', &
    'i2.2                   ', 'i2.2                   ', &
    'i4.4,i3.3              ', 'i4.4,"-",i3.3)         ', &
    'i4.4,"W",i2.2,i1.1     ', 'i4.4,"-W",i2.2,"-",i1.1', &
    'i4.4,"W",i2.2          ', 'i4.4,"-W",i2.2         ' /), &
    (/ 2, 7 /) )
  integer :: i
  character(len=*), parameter, dimension(2,7) :: date_format = reshape( (/ &
    ( '(' // date_format_base(1,i) // ')', '(' // date_format_base(2,i) // ')', i=1, 7 ) /), &
    (/ 2, 7 /) )

! Default length for character representation
  integer, parameter :: date_length = 16

! Days in a month
  integer(kind=1), dimension(12), parameter :: days_in_a_month = &
      (/ 31_1, 28_1, 31_1, 30_1, 31_1, 30_1, 31_1, 31_1, 30_1, 31_1, 30_1, 31_1 /)

! The iso8601 structure
  type, extends(t_object) :: t_iso8601_date
    private

!     Calendar year
      integer(kind=2) :: year = 0_2

!     Calendar month
      integer(kind=1) :: month = 0_1

!     Calendar day
      integer(kind=1) :: day = 0_1

    contains

!     Formatting interfaces
      generic :: to_string => iso8601_date_to_string
      procedure, private :: iso8601_date_to_string

!     Access function interfaces
      generic :: get_year => iso8601_date_get_year
      procedure, private :: iso8601_date_get_year
      generic :: set_year => iso8601_date_set_year, &
                             iso8601_date_set_year_default
      procedure, private :: iso8601_date_set_year
      procedure, private :: iso8601_date_set_year_default
      generic :: get_month => iso8601_date_get_month
      procedure, private :: iso8601_date_get_month
      generic :: set_month => iso8601_date_set_month, &
                              iso8601_date_set_month_default
      procedure, private :: iso8601_date_set_month
      procedure, private :: iso8601_date_set_month_default
      generic :: get_day => iso8601_date_get_day
      procedure, private :: iso8601_date_get_day
      generic :: set_day => iso8601_date_set_day, &
                            iso8601_date_set_day_default
      procedure, private :: iso8601_date_set_day
      procedure, private :: iso8601_date_set_day_default

!     Calculate the day of the year
      procedure :: day_of_year => iso8601_date_day_of_year

!     Calculate the week of the year
      procedure :: week_of_year => iso8601_date_week_of_year

!     Calculate the day of the week
      procedure :: day_of_week => iso8601_date_day_of_week

!     Return the number of days in the month
      procedure :: days_in_month => iso8601_date_days_in_month_this

!     Check if it is a leap year
      procedure :: is_leap_year => iso8601_date_is_leap_year_this

!     Auxiliary function for day counting
      procedure, private :: mjd => iso8601_date_mjd

  end type t_iso8601_date

! Constructor interface
  interface iso8601_date
    module procedure iso8601_date_specific
    module procedure iso8601_date_default
  end interface iso8601_date

! Interface for static functions
  interface iso8601_date_is_leap_year
    module procedure iso8601_date_is_leap_year_static
    module procedure iso8601_date_is_leap_year_default
  end interface iso8601_date_is_leap_year
  interface iso8601_date_days_in_month
    module procedure iso8601_date_days_in_month_static
    module procedure iso8601_date_days_in_month_default
  end interface iso8601_date_days_in_month

!- End of module variable declarations ----------------------------------------

contains

! Date constructor
elemental function iso8601_date_specific( year, month, day ) result(res)

! The year
  integer(kind=2), intent(in) :: year

! The month
  integer(kind=1), intent(in) :: month

! The day
  integer(kind=1), intent(in) :: day

! The constructed structure
  type(t_iso8601_date) :: res

! Initialise the year
  res%year = year

! Initialise the month
  res%month = month

! Initialise the day
  res%day = day

end function iso8601_date_specific


! Date constructor with default integers
elemental function iso8601_date_default( year, month, day ) result(res)

! The year
  integer, intent(in) :: year

! The month
  integer, intent(in) :: month

! The day
  integer, intent(in) :: day

! The constructed structure
  type(t_iso8601_date) :: res

! Initialise the year
  res%year = int(year,2)

! Initialise the month
  res%month = int(month,1)

! Initialise the day
  res%day = int(day,1)

end function iso8601_date_default


! Initialise from machine clock
function iso8601_date_now() result(res)

! The constructed structure
  type(t_iso8601_date) :: res

! Local variables
  integer, dimension(8) :: values
  character(len=5) :: zone

! Get the machine time
  call date_and_time( zone=zone, values=values )

! Assign the core return information
  res%year = int(values(1),2)
  res%month = int(values(2),1)
  res%day = int(values(3),1)

end function iso8601_date_now


! Format a date
elemental function iso8601_date_to_string( this, format, size, sign ) result(res)

! The ISO8601 structure
  class(t_iso8601_date), intent(in) :: this

! The format flag (optiona, default to iso8601_YYYYMMDD)
  integer, optional, intent(in) :: format

! The flag to for base/extended format (default to extended_format)
  integer, optional, intent(in) :: size

! The flag to include the sign before the date (default to .false.)
  logical, optional, intent(in) :: sign

! The resulting string
  character(len=date_length) :: res

! Local variables
  character(len=32) :: local
  integer :: the_format
  integer :: the_size
  character :: the_sign
  integer :: fmt_idx

! Check inputs
  if( present(format) ) then
    the_format = format
  else
    the_format = iso8601_YYYYMMDD
  end if
  if( present(size) ) then
    the_size = size
  else
    the_size = iso8601_date_extended_format
  end if
  the_sign = ''
  if( present(sign) ) then
    if( sign ) then
      if( this%year >= 0 ) then
        the_sign = '+'
      else
        the_sign = '-'
      end if
    end if
  end if

! Generate the output
  fmt_idx = the_format / 100
  select case( the_format )

!   Format: YYYY/MM/DD
    case( iso8601_YYYYMMDD )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year), this%month, this%day

!   Format: YYYY/MM
    case( iso8601_YYYYMM )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year), this%month

!   Format: YYYY
    case( iso8601_YYYY )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year)

!   Format: YY
    case( iso8601_YY )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year - ( this%year / 100 ) * 100 )

!   Format: YYYY/DoY
    case( iso8601_YYYYDDD )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year), this%day_of_year()

!   Format: YYYY/WoY/DoW
    case( iso8601_YYYYWwwD )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year), this%week_of_year(), this%day_of_week()

!   Format: YYYY/WoY
    case( iso8601_YYYYWww )
      write( local, date_format(the_size,fmt_idx) ) &
             abs(this%year), this%week_of_year()

  end select

! Prepend the sign
  local = trim(the_sign) // local

! Return the formatted string
  res = trim( local )

end function iso8601_date_to_string


! Access functions
! Get attribute year
elemental function iso8601_date_get_year( this ) result(res)

! The data structure
  class(t_iso8601_date), intent(in) :: this

! The parameter value to be returned
  integer(kind=2) :: res

! Return the value
  res = this%year

end function iso8601_date_get_year


! Set attribute year
elemental subroutine iso8601_date_set_year( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer(kind=2), intent(in) :: value

! Set the value
  this%year = value

end subroutine iso8601_date_set_year


! Set attribute year (from default integer)
elemental subroutine iso8601_date_set_year_default( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%year = int(value,2)

end subroutine iso8601_date_set_year_default


! Get attribute month
elemental function iso8601_date_get_month( this ) result(res)

! The data structure
  class(t_iso8601_date), intent(in) :: this

! The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%month

end function iso8601_date_get_month


! Set attribute month
elemental subroutine iso8601_date_set_month( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  this%month = value

end subroutine iso8601_date_set_month


! Set attribute month (from default integer)
elemental subroutine iso8601_date_set_month_default( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%month = int(value,1)

end subroutine iso8601_date_set_month_default


! Get attribute day
elemental function iso8601_date_get_day( this ) result(res)

! The data structure
  class(t_iso8601_date), intent(in) :: this

! The parameter value to be returned
  integer(kind=1) :: res

! Return the value
  res = this%day

end function iso8601_date_get_day


! Set attribute day
elemental subroutine iso8601_date_set_day( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer(kind=1), intent(in) :: value

! Set the value
  this%day = value

end subroutine iso8601_date_set_day


! Set attribute day (from default integer)
elemental subroutine iso8601_date_set_day_default( this, value )

! The data structure
  class(t_iso8601_date), intent(inout) :: this

! The parameter value to set
  integer, intent(in) :: value

! Set the value
  this%day = int(value,1)

end subroutine iso8601_date_set_day_default


! Check for leap year (type dound)
pure function iso8601_date_is_leap_year_this( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Flag for leap year
  logical :: res

! Compute from static function
  res = iso8601_date_is_leap_year_static(this%year)

end function iso8601_date_is_leap_year_this


! Check for leap year (static)
pure function iso8601_date_is_leap_year_static( year ) result(res)

! Year to test
  integer(kind=2), intent(in) :: year

! Flag for leap year
  logical :: res

! Check modulus 400; is leap year
  if( mod(year,400_2) == 0 ) then
    res = .true.

! Check modulus 100; is not leap year
  else if( mod(year,100_2) == 0 ) then
    res = .false.

! Check modulus 4; is leap year
  else if( mod(year,4_2) == 0 ) then
    res = .true.

! Default is not leap year
  else
    res = .false.

  end if

end function iso8601_date_is_leap_year_static


! Check for leap year (static; default integer)
pure function iso8601_date_is_leap_year_default( year ) result(res)

! Year to test
  integer, intent(in) :: year

! Flag for leap year
  logical :: res

! Compute from the base function
  res = iso8601_date_is_leap_year_static( int(year,kind=2) )

end function iso8601_date_is_leap_year_default


! Get days in month (type dound)
pure function iso8601_date_days_in_month_this( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Days in month
  integer :: res

! Compute from static function
  res = iso8601_date_days_in_month_static( this%month, this%year )

end function iso8601_date_days_in_month_this


! Get the days in month (static)
pure function iso8601_date_days_in_month_static( month, year ) result(res)

! Input month
  integer(kind=1), intent(in) :: month

! Input year (to check for leap year; optional; default no leap year)
  integer(kind=2), optional, intent(in) :: year

! Days in month
  integer :: res

! Local variables
  logical :: isleap

! Check input range
  if( month > 0 .and. month <= 12 ) then

!   Basic computation of the number of days
    res = days_in_a_month(month)

!   Check for leap year and month
    if( month == 2 ) then

!     Check if leap year is to be taken into account
      if( present(year) ) then
        isleap = iso8601_date_is_leap_year_static(year)
      else
        isleap = .false.
      end if
      if( isleap ) res = res + 1

    end if

  else
    res = 0
  end if

end function iso8601_date_days_in_month_static


! Get days in month (static; default integer)
pure function iso8601_date_days_in_month_default( month, year ) result(res)

! Input month
  integer, intent(in) :: month

! Input year (to check for leap year; optional; default no leap year)
  integer, optional, intent(in) :: year

! Days in month
  integer :: res

! Compute with base function
  if( present(year) ) then
    res = iso8601_date_days_in_month_static( int(month,kind=1), int(year,kind=2) )
  else
    res = iso8601_date_days_in_month_static( int(month,kind=1) )
  end if

end function iso8601_date_days_in_month_default


! Get day of the year
pure function iso8601_date_day_of_year( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Day of the year
  integer :: res

! Local variables
  integer(kind=1) :: month

! Get the last complete month in the date
  month = this%month - 1_1

! Get the number of days from the date
  res = int( sum( days_in_a_month(:month) ), 4) + int(this%day,4)

! Check for leap years
  if( this%month > 2 ) then
    if( this%is_leap_year() ) then
      res = res + 1
    end if
  end if

end function iso8601_date_day_of_year


! Get week of the year
! ISO-8601: 1st week of year contains first Thursday of year
!           Calendar week starts on Monday
pure function iso8601_date_week_of_year( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Day of the year
  integer :: res

! Local variables
  type(t_iso8601_date) :: jan_first
  integer :: jan_first_dow
  integer :: first_week

! Compute the week day for January first of the input date year
  jan_first = iso8601_date( this%year, 1_1, 1_1 )
  jan_first_dow = jan_first%day_of_week()

! Compute the first week of the year according to ISO-8601
  if( jan_first_dow <= 4 ) then
    first_week = 1
  else
    first_week = 0
  end if

! Compute the week of the year
  res = ( ( this%day_of_year() + jan_first_dow - 2 ) / 7 ) + first_week

end function iso8601_date_week_of_year


! Get day of the week
! ISO-8601: Calendar week starts on Monday
pure function iso8601_date_day_of_week( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Day of the year
  integer :: res

! Local variables
  integer :: mjd

! Compute MJD (reference 2000-01-01)
  mjd = this%mjd()

! Compute the week day (2000-01-01 is Saturday=6)
  if( mjd > 0 ) then
    res = mod( mjd + 5, 7 ) + 1
  else
    res = mod( mod( mjd + 5, 7 ) + 8, 7 )
  end if

end  function iso8601_date_day_of_week


! Compute integer part of mjd2000 from year, month, day
pure function iso8601_date_mjd( this ) result(res)

! Calling object
  class(t_iso8601_date), intent(in) :: this

! Resulting MJD with reference to 2000-01-01
  integer :: res

! Local variables
  integer :: mjd2000
  integer   :: jj, l
  integer, parameter :: user_time_mjd1950 = -18262    ! 1950-01-01 in MJD2000

! Code from ESA's orbit library ORBLIB
  jj      = (14 - this%month)/12
  l       = this%year - 1900 * (this%year / 1900) - jj
  mjd2000 = this%day - 18234 + (1461 * l) / 4 +       &
            (367 * (this%month - 2 + jj * 12)) / 12
! End of code from ESA's orbit library ORBLIB

! Set reference to 2000-01-01
  res = mjd2000 + user_time_mjd1950

end function iso8601_date_mjd

end module m_iso8601_date
