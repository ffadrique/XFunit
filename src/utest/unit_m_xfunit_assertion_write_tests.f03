module unit_m_xfunit_assertion_write_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_write
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

  use m_xfunit
  use m_messages

  use m_xfunit_unit
  use m_xfunit_assertion

  use m_string
  use m_xml
  use m_file_handler

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_write_before_001
  public unit_m_xfunit_assertion_write_test_001
  public unit_m_xfunit_assertion_write_after_001

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_write'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! Variables to support file comparison assertion
  type(t_file_handler), allocatable, save :: fhandler
  type(t_xml_writer), allocatable, save :: xwriter
  type(t_string), save :: factual, fexpected

! Type to be used as input for the class(*) assertions
  type t_test
    integer :: i = 0
    real :: r = 0.0
    character(len=10) :: c
  end type t_test


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_write_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the handlers for file comparison assertion
  fexpected = trim(manager%get_unit_ref_dir()) // '/unit_m_xfunit_assertion_write_001.ref'
  factual = 'unit_m_xfunit_assertion_write_001.out'

! Open the temporary file for file comparison assertion
  allocate( fhandler, source=file_handler( factual ) )
  call fhandler%open( write=.true. )
  if( fhandler%is_open() ) then

!   Initialise XML handler
    allocate( xwriter, source=xml_writer( fhandler ) )

  else
    call ut%error( fhandler%get_iostat() )
  end if

end subroutine unit_m_xfunit_assertion_write_before_001

! ############################################################################

subroutine unit_m_xfunit_assertion_write_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer :: i
  integer, parameter :: n = 4
  integer, parameter, dimension(n) :: ia = [ 3, 4, 5, 6 ]
  real, parameter, dimension(n) :: ra = [ 3.0, 4.0, 5.0, 6.0 ]
  complex, parameter, dimension(n) :: xa = [ cmplx(3.0,-3.0), cmplx(4.0,-4.0), cmplx(5.0,-5.0), cmplx(6.0,-6.0) ]
  character(len=*), parameter, dimension(n) :: ca = [ 'Lorem ipsum dolor sit amet, ', &
                                                      'consectetur adipiscing elit.', &
                                                      'Aliquam non consequat ex,   ', &
                                                      'efficitur vestibulum mauris.' ]
  type(t_string), dimension(n) :: sa
  logical, dimension(n) :: la = [ .true., .true., .true., .true. ]
  type(t_test), parameter, dimension(n) :: ta = &
             [ t_test( 1, 1.0, 'One'), &
               t_test( 2, 2.0, 'Two'), &
               t_test( 3, 3.0, 'Three'), &
               t_test( 4, 4.0, 'Four') ]
  integer, parameter, dimension(2,2) :: im = reshape( ia, [ n/2, n/2 ] )
  real, parameter, dimension(2,2) :: rm = reshape( ra, [ n/2, n/2 ] )
  type(t_test), parameter, dimension(2,2) :: tm = reshape( ta, [ n/2, n/2 ] )


! Reset error handling structures
  call msg%reset_error()

! Initialise
  sa = [ ( string( ca(i) ), i = 1, n ) ]

! Class assertion
  call ut%assert_equal( 'Class equal', ta(1), ta(1), test_equal, test_serialize )

! Integer assertions
  call ut%assert_equal( 'Integer equal', ia(1), ia(1) )
  call ut%assert_between( 'Integer between', ia(3), ia(1), ia(n) )
  call ut%assert_greater( 'Integer greater', ia(n), ia(1) )
  call ut%assert_less( 'Integer less', ia(1), ia(n) )

! Real assertions
  call ut%assert_equal( 'Real equal', ra(1), ra(1) )
  call ut%assert_between( 'Real between', ra(3), ra(1), ra(n) )
  call ut%assert_greater( 'Real greater', ra(n), ra(1) )
  call ut%assert_less( 'Real less', ra(1), ra(n) )

! Complex assertions
  call ut%assert_equal( 'Complex equal', xa(1), xa(1) )

! Logical assertions
  call ut%assert_true( 'Logical equal', la(1) )
  call ut%assert_false( 'Logical between', .not. la(1) )

! Character assertions
  call ut%assert_equal( 'Character equal', ca(1), ca(1) )
  call ut%assert_equal( 'Character equal (nocase)', ca(1), ca(1), ignorecase=.true. )
  call ut%assert_equal( 'Character equal (global)', ca(1), 'Lorem*', matching=xfunit_assertion_character_match_global )

! String assertions
  call ut%assert_equal( 'String equal', sa(1), sa(1) )
  call ut%assert_equal( 'String equal (nocase)', sa(1), sa(1), ignorecase=.true. )
  call ut%assert_equal( 'String equal (global)', sa(1), string('Lorem*'), matching=xfunit_assertion_character_match_global )



! Class array assertion
  call ut%assert_equal( 'Class array equal', ta, ta, test_equal, test_serialize )

! Integer array assertions
  call ut%assert_equal( 'Integer array equal', ia, ia )
  call ut%assert_between( 'Integer array between', ia, ia(1), ia(n) )
  call ut%assert_greater( 'Integer array greater', ia, ia(1) )
  call ut%assert_less( 'Integer array less', ia, ia(n) )

! Real array assertions
  call ut%assert_equal( 'Real array equal', ra, ra )
  call ut%assert_between( 'Real array between', ra, ra(1), ra(n) )
  call ut%assert_greater( 'Real array greater', ra, ra(1) )
  call ut%assert_less( 'Real array less', ra, ra(n) )

! Complex array assertions
  call ut%assert_equal( 'Complex array equal', xa, xa )

! Logical array assertions
  call ut%assert_true( 'Logical array equal', la )
  call ut%assert_false( 'Logical array between', .not. la )

! Character array assertions
  call ut%assert_equal( 'Character array equal', ca, ca )
  call ut%assert_equal( 'Character array equal (nocase)', ca, ca, ignorecase=.true. )
  call ut%assert_equal( 'Character array equal (global)', ca, '*i*', matching=xfunit_assertion_character_match_global )

! String array assertions
  call ut%assert_equal( 'String array equal', sa, sa )
  call ut%assert_equal( 'String array equal (nocase)', sa, sa, ignorecase=.true. )
  call ut%assert_equal( 'String array equal (global)', sa, string('*i*'), matching=xfunit_assertion_character_match_global )




! Class matrix assertion
  call ut%assert_equal( 'Class matrix equal', tm, tm, test_equal, test_serialize )

! Integer matrix assertions
  call ut%assert_equal( 'Integer matrix equal', im, im )
  !call ut%assert_between( 'Integer matrix between', ia, ia(1), ia(n) )
  !call ut%assert_greater( 'Integer matrix greater', ia, ia(1) )
  !call ut%assert_less( 'Integer matrix less', ia, ia(n) )

! Real matrix assertions
  call ut%assert_equal( 'Real matrix equal', ra, ra )
  !call ut%assert_between( 'Real matrix between', ra, ra(1), ra(n) )
  !call ut%assert_greater( 'Real matrix greater', ra, ra(1) )
  !call ut%assert_less( 'Real matrix less', ra, ra(n) )


! Write unit test in text format
  call ut%write( fhandler%get_unit(), .false. )

! Flush and close the temporary file before comparing
  call fhandler%flush()
  call fhandler%close()

! Implement the file comparison assertion
  call ut%assert_compare_files( 'm_xfunit_assertion_write_001', factual, fexpected )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_write_test_001

! ############################################################################

subroutine unit_m_xfunit_assertion_write_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Terminate handler structures
  deallocate( xwriter )
  deallocate( fhandler )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_write_after_001

! ############################################################################
! # Auxiliary functions for object comparison in assertion ###################
! # Follows interface in m_xfunit_assertion_class ############################
! ############################################################################

pure function test_equal( left, right ) result(res)
  class(*), intent(in) :: left
  class(*), intent(in) :: right
  logical :: res
  select type( left )
    type is( t_test )
      select type( right )
        type is( t_test )
          res = left%i == right%i
      end select
  end select
end function test_equal

pure function test_serialize( this ) result(res)
  class(*), intent(in) :: this
  character(len=:), allocatable :: res
  select type( this )
    type is( t_test )
      res = trim(character(this%i)) // ': ' // trim(character(this%r)) // ' (' // trim(this%c) // ')'
  end select
end function test_serialize

end module unit_m_xfunit_assertion_write_tests
