!< FLOw test.
program flow_test_primitive_mul
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1           !< A primitive object.
type(primitive_object) :: primitive2           !< A primitive object.
type(primitive_object) :: primitive3           !< A primitive object.
type(field_scalar)     :: partial_densities(2) !< A scalar field object.
logical                :: test_passed(7)       !< List of passed tests.

test_passed = .false.

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P
partial_densities(1) = 0.125_R_P / 2
partial_densities(2) = 0.125_R_P / 2
primitive1%partial_densities = partial_densities

primitive2%density = 1._R_P
primitive2%velocity = 2._R_P
primitive2%pressure = 1._R_P
partial_densities(1) = 1._R_P / 2
partial_densities(2) = 1._R_P / 2
primitive2%partial_densities = partial_densities

primitive3 = primitive1 * primitive2
test_passed(1) = primitive3 == (primitive1 * primitive2)
print "(A,F6.3)",       'density   => 0.125  * 1 = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2 = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 1      * 1 = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 0.0625 * 0.5 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
call print_error(test='primitive3 = primitive1 * primitive2', is_test_passed=test_passed(1))

primitive3 = 2._R_P * primitive1
test_passed(2) = primitive3 == (2._R_P * primitive1)
print "(A)", ''
print "(A,F6.3)",       'density   => 2.0 * 0.125  = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 2.0 * 1      = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 2.0 * 1      = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 2.0 * 0.0625 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
call print_error(test='primitive3 = 2.0 * primitive1', is_test_passed=test_passed(2))

primitive3 = primitive1 * 2._R_P
test_passed(3) = primitive3 == (primitive1 * 2._R_P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2.0 = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2.0 = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 1      * 2.0 = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 0.0625 * 2.0 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
call print_error(test='primitive3 = primitive1 * 2.0', is_test_passed=test_passed(3))

primitive3%velocity = primitive1%density * primitive2%velocity
test_passed(4) = primitive3%velocity == (primitive2%velocity * 0.125_R_P)
print "(A)", ''
print "(A,3(F6.3,1X))", 'density * velocity => 0.125 * 2 = ', primitive3%velocity%field
call print_error(test='primitive3%velocity = primitive1%density * primitive2%velocity', is_test_passed=test_passed(4))

primitive3%velocity = primitive2%velocity * primitive1%density
test_passed(5) = primitive3%velocity == (primitive2%velocity * 0.125_R_P)
print "(A)", ''
print "(A,3(F6.3,1X))", 'velocity * density => 2 * 0.125 = ', primitive3%velocity%field
call print_error(test='primitive3%velocity = primitive2%velocity * primitive1%density', is_test_passed=test_passed(5))

primitive3 = 2_I_P * primitive1
test_passed(6) = primitive3 == (2_I_P * primitive1)
print "(A)", ''
print "(A,F6.3)",       'density   => 2 * 0.125  = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 2 * 1      = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 2 * 1      = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 2 * 0.0625 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
call print_error(test='primitive3 = 2 * primitive1', is_test_passed=test_passed(6))

primitive3 = primitive1 * 2_I_P
test_passed(7) = primitive3 == (primitive1 * 2_I_P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2 = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 1      * 2 = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 1      * 2 = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 0.0625 * 2 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
call print_error(test='primitive3 = primitive1 * 2', is_test_passed=test_passed(7))

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
contains
  subroutine print_error(test, is_test_passed)
  !< Print error message if test is failed.
  character(*), intent(in) :: test           !< Test description.
  logical,      intent(in) :: is_test_passed !< Test result.
  if (.not.is_test_passed) then
    print*, '"'//test//'" failed!'
    print*, 'primitive1:'
    print "(A,F6.3)",       '  density   = ', primitive1%density%field
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive1%velocity%field
    print "(A,F6.3)",       '  pressure  = ', primitive1%pressure%field
    print "(A,2(F7.4,1X))", '  densities = ', primitive1%partial_densities(1)%field, primitive1%partial_densities(2)%field
    print*, 'primitive2:'
    print "(A,F6.3)",       '  density   = ', primitive2%density%field
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive2%velocity%field
    print "(A,F6.3)",       '  pressure  = ', primitive2%pressure%field
    print "(A,2(F7.4,1X))", '  densities = ', primitive2%partial_densities(1)%field, primitive2%partial_densities(2)%field
    print*, 'primitive3:'
    print "(A,F6.3)",       '  density   = ', primitive3%density%field
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive3%velocity%field
    print "(A,F6.3)",       '  pressure  = ', primitive3%pressure%field
    print "(A,2(F7.4,1X))", '  densities = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field
  endif
  endsubroutine print_error
endprogram flow_test_primitive_mul
