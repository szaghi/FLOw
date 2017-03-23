!< FLOw test.

program flow_test_primitive_compressible_div
!< FLOw test.

use flow, only : primitive_compressible
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
type(primitive_compressible) :: primitive1     !< A primitive object.
type(primitive_compressible) :: primitive2     !< A primitive object.
type(primitive_compressible) :: primitive3     !< A primitive object.
type(vector)                 :: velocity       !< A vector object.
logical                      :: test_passed(3) !< List of passed tests.

test_passed = .false.

velocity = 1._R8P
primitive1 = primitive_compressible(density=0.125_R8P, velocity=velocity, pressure=1._R8P)
velocity = 2._R8P
primitive2 = primitive_compressible(density=1._R8P, velocity=velocity, pressure=1._R8P)

primitive3 = primitive1 / primitive2
test_passed(1) = primitive3 == (primitive1 / primitive2)
print "(A,F6.3)",       'density   => 0.125  / 1   = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2   = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 1   = ', primitive3%pressure
call print_error(test='primitive3 = primitive1 / primitive2', is_test_passed=test_passed(1))

primitive3 = primitive1 / 2._R8P
test_passed(2) = primitive3 == (primitive1 / 2._R8P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  / 2.0 = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2.0 = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 2.0 = ', primitive3%pressure
call print_error(test='primitive3 = primitive1 / 2.0', is_test_passed=test_passed(2))

primitive3 = primitive1 / 2_I4P
test_passed(3) = primitive3 == (primitive1 / 2_I4P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  / 2 = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2 = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 2 = ', primitive3%pressure
call print_error(test='primitive3 = primitive1 / 2', is_test_passed=test_passed(3))

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
endprogram flow_test_primitive_compressible_div
