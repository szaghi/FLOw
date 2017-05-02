!< FLOw test.

program flow_test_primitive_compressible_mul
!< FLOw test.

use flow, only : primitive_compressible
use penf, only : I_P, R_P
use vecfor, only : vector

implicit none
type(primitive_compressible) :: primitive1     !< A primitive object.
type(primitive_compressible) :: primitive2     !< A primitive object.
type(primitive_compressible) :: primitive3     !< A primitive object.
type(primitive_compressible) :: primitive4     !< A primitive object.
type(vector)                 :: velocity       !< A vector object.
logical                      :: test_passed(5) !< List of passed tests.

test_passed = .false.

velocity = 1._R_P
primitive1 = primitive_compressible(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
velocity = 2._R_P
primitive2 = primitive_compressible(density=1._R_P, velocity=velocity, pressure=1._R_P)

primitive3 = primitive_compressible(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
primitive4 = primitive1 * primitive2
test_passed(1) = primitive3 == primitive4
print "(A,F6.3)",       'density   => 0.125  * 1 = ', primitive4%density
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2 = ', primitive4%velocity
print "(A,F6.3)",       'pressure  => 1      * 1 = ', primitive4%pressure
call print_error(test='primitive3 = primitive1 * primitive2', is_test_passed=test_passed(1))

velocity = 2._R_P
primitive3 = primitive_compressible(density=0.25_R_P, velocity=velocity, pressure=2._R_P)

primitive4 = 2._R_P * primitive1
test_passed(2) = primitive3 == primitive4
print "(A)", ''
print "(A,F6.3)",       'density   => 2.0 * 0.125  = ', primitive4%density
print "(A,3(F6.3,1X))", 'velocity  => 2.0 * 1      = ', primitive4%velocity
print "(A,F6.3)",       'pressure  => 2.0 * 1      = ', primitive4%pressure
call print_error(test='primitive3 = 2.0 * primitive1', is_test_passed=test_passed(2))

primitive4 = primitive1 * 2._R_P
test_passed(3) = primitive3 == primitive4
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2.0 = ', primitive4%density
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2.0 = ', primitive4%velocity
print "(A,F6.3)",       'pressure  => 1      * 2.0 = ', primitive4%pressure
call print_error(test='primitive3 = primitive1 * 2.0', is_test_passed=test_passed(3))

primitive3 = 2_I_P * primitive1
test_passed(4) = primitive3 == primitive4
print "(A)", ''
print "(A,F6.3)",       'density   => 2 * 0.125  = ', primitive4%density
print "(A,3(F6.3,1X))", 'velocity  => 2 * 1      = ', primitive4%velocity
print "(A,F6.3)",       'pressure  => 2 * 1      = ', primitive4%pressure
call print_error(test='primitive3 = 2 * primitive1', is_test_passed=test_passed(4))

primitive3 = primitive1 * 2_I_P
test_passed(5) = primitive3 == primitive4
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2 = ', primitive4%density
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2 = ', primitive4%velocity
print "(A,F6.3)",       'pressure  => 1      * 2 = ', primitive4%pressure
call print_error(test='primitive3 = primitive1 * 2', is_test_passed=test_passed(5))

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
contains
  subroutine print_error(test, is_test_passed)
  !< Print error message if test is failed.
  character(*), intent(in) :: test           !< Test description.
  logical,      intent(in) :: is_test_passed !< Test result.
  if (.not.is_test_passed) then
    print*, '"'//test//'" failed!'
    print*, 'primitive1:'
    print "(A,F6.3)",       '  density   = ', primitive1%density
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive1%velocity
    print "(A,F6.3)",       '  pressure  = ', primitive1%pressure
    print*, 'primitive2:'
    print "(A,F6.3)",       '  density   = ', primitive2%density
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive2%velocity
    print "(A,F6.3)",       '  pressure  = ', primitive2%pressure
    print*, 'primitive3:'
    print "(A,F6.3)",       '  density   = ', primitive3%density
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive3%velocity
    print "(A,F6.3)",       '  pressure  = ', primitive3%pressure
  endif
  endsubroutine print_error
endprogram flow_test_primitive_compressible_mul
