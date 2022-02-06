module unit_m_xfunit_assertion_files_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_xfunit_assertion_files
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

  use m_string
  use m_xfunit
  use m_messages

  use m_xfunit_assertion_files

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_xfunit_assertion_files_test_001

  public unit_m_xfunit_assertion_files_test_002

  public unit_m_xfunit_assertion_files_test_003

  public unit_m_xfunit_assertion_files_test_004

  public unit_m_xfunit_assertion_files_test_005

  public unit_m_xfunit_assertion_files_test_006

  public manager, suite
  
!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'xfunit'
  character(len=*), parameter :: module = 'm_xfunit_assertion_files'
  
  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  integer :: status
  character(len=:), allocatable :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_test_expected.dat'
  
! Compare files with status (default text mode)
  call ut%assert_compare_files( 'Default text (character interface)', factual, fexpected )
  
! Compare files (default text mode)
  call ut%assert_compare_files( 'Default text (character interface) with status', factual, fexpected, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Compare files with status (forced text mode)
  call ut%assert_compare_files( 'Foced text (character interface)', factual, fexpected, binary=.false. )
  
! Compare files (forced text mode)
  call ut%assert_compare_files( 'Foced text (character interface) with status', factual, fexpected, binary=.false., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_files_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  integer :: status
  character(len=:), allocatable :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_test_expected.dat'
  
! Compare files with status (forced binary mode)
  call ut%assert_compare_files( 'Foced binary (character interface)', factual, fexpected, binary=.true. )
  
! Compare files (forced binary mode)
  call ut%assert_compare_files( 'Foced binary (character interface) with status', factual, fexpected, binary=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_files_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  integer :: status
  type(t_string) :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_test_expected.dat'
  
! Compare files with status (default text mode)
  call ut%assert_compare_files( 'Default text (character interface)', factual, fexpected )
  
! Compare files (default text mode)
  call ut%assert_compare_files( 'Default text (character interface) with status', factual, fexpected, status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Compare files with status (forced text mode)
  call ut%assert_compare_files( 'Foced text (character interface)', factual, fexpected, binary=.false. )
  
! Compare files (forced text mode)
  call ut%assert_compare_files( 'Foced text (character interface) with status', factual, fexpected, binary=.false., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_m_xfunit_assertion_files_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  integer :: status
  type(t_string) :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_test_expected.dat'
  
! Compare files with status (forced binary mode)
  call ut%assert_compare_files( 'Foced binary (character interface)', factual, fexpected, binary=.true. )
  
! Compare files (forced binary mode)
  call ut%assert_compare_files( 'Foced binary (character interface) with status', factual, fexpected, binary=.true., status=status )
  call ut%assert_equal( 'Status', status, xfunit_assertion_is_pass )  

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_files_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  type(t_string) :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_performance_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_performance_test_expected.dat'
  
! Compare files with status (forced binary mode)
  call ut%assert_compare_files( 'Performance text', factual, fexpected, binary=.false. )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_files_test_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_xfunit_assertion_files_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_xfunit_assertion_files) :: object

! Local variables
  type(t_string) :: factual, fexpected

! Reset error handling structures
  call msg%reset_error()

! Initialise
  factual = trim(manager%get_unit_data_dir()) // '/xfunit_performance_test_actual.dat'
  fexpected = trim(manager%get_unit_data_dir()) // '/xfunit_performance_test_expected.dat'
 
! Compare files with status (forced binary mode)
  call ut%assert_compare_files( 'Performance binary', factual, fexpected, binary=.true. )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_xfunit_assertion_files_test_006

end module unit_m_xfunit_assertion_files_tests
