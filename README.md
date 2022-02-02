# XFunit
XFunit is an object oriented framework to implement unit testing in native Fortran. It uses object-oriented features of Fortran 2018.

## Unit testing with XFunit
Unit testing with XFunit is structured in suites and units. A test suite typically define all test to be implemented for a Fortran module. The tests in the suite are structured in unit test (units) conceived to test specific aspects of the functionality in the module,  Within each unit, a number of assertions are made, i.e. to test whether a specified condition is satisfied or not, e.g. the output of a function against expected value.

## The XFunit types
XFunit provides the `t_xfunit_unit` derived type to host the method that implements the  assertions within the unit test case. Objects of this type also keep track of how many assertions are implemented in the test, how many have been executed and how many have passed and failed.

XFunit provides the `t_xfunit_suite`derived type to host all configured unit tests. Objects of this type keep track of the number of implemented unit tests, how many have been executed and how many have passed or failed.

XFunit provides the `t_xfunit_manager` derived type to manage the execution of the test suite, the configuration of the execution environment and the generation of the test execution report.

## Writing unit tests with XFunit
### Using XFunit and declaring suites and unit tests
To use XFunit the test module shall include the `use m_xfunit` statement. Then test suites and unit tests can be declared in he test module

```fortran
use m_xfunit

type(t_xfunit_suite) :: suite
type(t_xfunit_unit) :: ut
```
### Initialising the test suite
The process of initialisation of the test module starts with the initialisation of the suite object. The constructor provided by XFunit has mandatory arguments for the package being tested, the source code where the tested module is contained and the suite description (annotation).
```fortran
  suite = xfunit_suite( package='foo_package', &
                        source='foo.f90', &
                        annotation='My description of foo' )
```

### Initialising the units tests
Unit tests can then be created and added to the test suite. The constructor provided by XFunit has mandatory arguments for the test name, the type/class for which the test is defined, the method to execute the unit test and the test description (annotation).
```fortran
  ut = xfunit_unit( name='bar', &
                    classname='bar_type', &
                    executer=bar_test, &
                    annotation='My description of bar' )
  call suite%add_unit_test( ut )
```
There is no limit in the number of tests that a suite can host.

### Implementing the units tests
The unit tests are implemented in Fortran subroutines that comply with the interface defined in XFunit. The unit test subroutine receives the unit test object, which is used to invoke the assertions that implement the actual tests.
```fortran
subroutine bar_test(ut)
    class(t_xfunit_unit), intent(inout) :: ut

    call ut%assert_equal( x, 3.0 )
    ...
end subroutine bar_test
```

### Assertions
Within a test case subroutine, assertions are made invoking one of the `assert` methods of the `t_xfunit_unit` object. XFunit provides a variety of overloaded methods:
- `assert_equal()`: Equality for real, integer, complex, logical and character. This includes comparison of arrays with respect to another array or a single value.
- `assert_between()`: Value within a range for real and integer. This includes comparison of arrays with respect to two boundary arrays or with respect to single boundary values.
- `assert_greater()` and `assert_less()`: Value greater than and value less than for real and integer. This includes comparison of arrays with respect to another boundary array (greater or less) or with respect to a single boundary value (greater or less).
- `assert_true()` and `assert_false()`: Assert the logical argument provided to the function with respect to `.true.` or `.false.`.
- `assert_files()`: File comparison assertion, either as binary (data stream) or text (line by line)
- `assert_class()`: Object comparison assertion. An object comparison function compatible with the `assignment(=)` operator must be provided. Optionally, a function returning the object serialisation as a character string can be provided to allow the text representation of the compared objects in the tes report.
- `assert_pass()` and `assert_fail()`: Pass and fail assertions that generate directly pass or fail status of the assertion without actual comparison.

All `real` and `complex` functions are overloaded for `kind=4,8`
All `integer` functions are overloaded for `kind=1,2,4,8`
All `logical` functions  are overloaded for `kind=1,2,4`
The derived type `t_string` is also supported as a native type in the same configuration as `character`.
For floating point equality assertions a threshold is used in the comparison. This threshold is automatically computed from the values of the asserted values. I can be overwritten passing to the assertion call the optional argument `threshold`.
```fortran
call ut%assert_equal( 'Test with tolerance', x, 3.0, threshold=0.001 )
```
All overloads permit the direct retrieval of the assertion status in the invocation of the assertion by providing the optional integer argument `status`. 
```fortran
call ut%assert_equal( 'Test with tolerance', x, 3.0, threshold=0.001, status=stat )
```
The status argument can take the values `xfunit_assertion_is_none` (status undefined), `xfunit_assertion_is_fail` or `xfunit_assertion_is_pass`.

### Setup and teardown routines
Each suite may optionally contain special subroutines called to be executed before (setup) and after (teardown) the execution of the unit tests within the suite.
```fortran
  suite = xfunit_suite( package='foo', &
                        source='foo.f90', &
                        before=execute_before_foo_tests, &
                        after=execute_after_foo_tests, &
                        annotation='My description of foo' )
```
The setup subroutine is called before any tests are run and the teardown routine is called after all tests are finished. These subroutines can be used to initialise and finalise global variables or other settings for the tests. They also take the unit suite object as an argument.
Analogously, each unit test may optionally contain dedicated setup and teardown routines.
```fortran
  ut = xfunit_unit( name='bar', &
                    classname='bar_type', &
                    executer=bar_test, &
                    before=bar_test_before, &
                    after=bar_test_after, &
                    annotation='My description of bar' )
  call suite%add_unit_test( ut )
```
The setup subroutine is called before the unit test is run and the teardown routine is called after the unit test is finished. These subroutines can be used to initialise and finalise global variables or other settings for the unit test. They also take the unit test object as an argument.

### Test initialisation and execution
XFunit provides the derived type `t_xfunit_manager` to set up the test execution environment, to execute the test and to generate the test execution report.
The manager can be initialised with the general constructor that allows the definition of the root directory for the test execution, the data directory where data files are to be fetched (e.g. model data for the tests), the reference directory where file comparison reference files are to be fetched and the output directory where JUnit files with the text execution report are to be placed.
```fortran
! Constructor (generic)
function xfunit_manager( fname, &
                         xfunit_root_dir, &
                         unit_data_dir, unit_ref_dir, unit_jxml_dir ) result(res)

! The output file name (default to module.jxml or module.junit.xml)
  character(len=*), intent(in) :: fname

! The path to the root directory where the tests are implemented (default to .)
  character(len=*), optional, intent(in) :: xfunit_root_dir

! The path to the unit data (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_data_dir

! The path to the unit test reference (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_ref_dir

! The path to the unit test jxml (default to xfunit_root_dir)
  character(len=*), optional, intent(in) :: unit_jxml_dir

...
```

A convenience constructor `xfunit_manager_eclipse()` is also provided that implements a predefined directory structure starting from the root directory
```fortran
! Constructor (unit testing contained in source code folder tree, similar to Eclipse projects)
! Structure:
!   datadir = xfunit_root_dir / data
!   refdir  = xfunit_root_dir / reference
!   jxmldir = xfunit_root_dir
! This is consistent with the integration in eclipse:
!   xfunit_root_dir := workspace / src / <package> / utest
function xfunit_manager_eclipse( module, xfunit_root_dir ) result(res)

! The name of the module being tested
  character(len=*), intent(in) :: module

! The path to the root directory where the tests are implemented (default to .)
  character(len=*), optional, intent(in) :: xfunit_root_dir

...
```
The test sequnece can be implemented according to the following example:
```fortran
! Declarations
  type(t_xfunit_suite) :: suite
  type(t_xfunit_ut) :: ut
  character(len=256) :: xfunit_root_dir

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  manager = xfunit_manager_eclipse( 'foo', xfunit_root_dir )

! Initialise test suite
  suite = xfunit_suite( package='foobar', &
                        source='foo.f03', &
                        annotation='My description for suite foo' )

! Create test
  ut = xfunit_unit( name='bar', &
                    classname='bar_type', &
                    executer=bar_test, &
                    annotation='My description of bar' )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )
```

### Controlling the report generation
XFunit generates a test execution report in XML compatible with JUnit and that can be ingested by continuous integration tools like Hudson or Jenkins.
Two modes can be selected for the generation of the report: strict compatible with JUnit XML schema or detailed (default) that extends the JUnit default with information on the unit test and assertion execution. The selection is done in the construction of the `t_xfunit_manager` object.
```fortran
  manager = xfunit_manager_eclipse( 'foo', xfunit_root_dir, junit_strict=.true. )
```

By default, the `t_xfunit_manager` object captures the standard output and standard error and writes them to the `<sytem-out>` and `<system-err>` sections of the JUnit file respetively. This capture can be overriden during the `t_xfunit_manager` object construction.
```fortran
  manager = xfunit_manager_eclipse( 'foo', xfunit_root_dir, free_out=.true. )
```

## Building and testing XFunit
XFunit has been tested with Intel Fortran 19 (or higher) and gfortran 9.4 (or higher).
The test provided along with the XFunit libraries are written using XFunit, hence being the XFunit autotest.

### Windows
XFunit is provided with a Visual Studio 2019 configured solution that allows building and testing the entire suite. The solution provides a C# project that integrates the unit test in Fortran with the unit test detection feature of Visual Studio. This allows the execution of all unit tests from the Test Explorer menu.

Create the environment variable `XFUNIT_ROOT_DIR=$(ProjectDir)..` for each unit testing project to store the test output (.jxml file) in the `utest` directory. This configuration depends on the windows user in file `unit_m_xfunit_<functionname>.vfproj.<windows user>.user`

### Linux
XFunit is provided with `gmake` makefiles to build and test the entire suite. 
To build the XFunit library and use modules files  execute the following command in the `src` directory
```make
gmake libs
```
To build the XFunit library, use modules files and unit tests execute the following command in the `src` directory
```make
gmake all
```
To execute the unit tests  execute the following command in the `src` or the `utest` directory
```make
gmake units
```
The default compiler is `gfortran` but can be overridden from the command line to use Intel Fortran
```make
gmake F90COMP=ifort
```
The ifort or gfortran commands must be in the execution path.
Alternatively, the makefile can be edited to provide the full path to the compiler.
Compiler options are automatically selected for the active compiler; only Debug configuration is provided in the make files.

## Documentation
This readme page is the main user documentation. In addition, documentation generated with FORD and Doxygen can be found in the `documentation` directory. Two sets of documentation are provided
- API: limited to the modules and derived types necessary to build units tests using XFunit (`t_xfunit`, `t_xfunit_manager`, `t_xfunit_suite`, `t_xfunit_unit`, `t_xfunit_assertion`)
- Full: contains documentation for the complete XFunit library, including all utility modules.

## Licensing
XFunit is open-source software, licensed under the GNU Lesser General Public License (LGPL).

