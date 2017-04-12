!< FLOw test.

program flow_test_primitive_compressible_multispecie_div
!< FLOw test.

use flow, only : primitive_compressible_multispecie
use penf, only : I_P, R_P
use vecfor, only : vector

implicit none
type(primitive_compressible_multispecie) :: primitive1     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive2     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive3     !< A primitive object.
type(vector)                             :: velocity       !< A vector object.
logical                                  :: test_passed(3) !< List of passed tests.

test_passed = .false.

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=0.125_R_P,  &
                                                velocity=velocity,  &
                                                pressure=1._R_P,    &
                                                partial_densities=[0.125_R_P / 2,  0.125_R_P / 2])
velocity = 2._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P,  &
                                                velocity=velocity,  &
                                                pressure=1._R_P,    &
                                                partial_densities=[1._R_P / 2,  1._R_P / 2])

primitive3 = primitive1 / primitive2
test_passed(1) = primitive3 == (primitive1 / primitive2)
print "(A,F6.3)",       'density   => 0.125  / 1   = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2   = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 1   = ', primitive3%pressure
print "(A,2(F7.4,1X))", 'densities => 0.0625 / 0.5 = ', primitive3%partial_densities(1), primitive3%partial_densities(2)
call print_error(test='primitive3 = primitive1 / primitive2', is_test_passed=test_passed(1))

primitive3 = primitive1 / 2._R_P
test_passed(2) = primitive3 == (primitive1 / 2._R_P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  / 2.0 = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2.0 = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 2.0 = ', primitive3%pressure
print "(A,2(F7.4,1X))", 'densities => 0.0625 / 2.0 = ', primitive3%partial_densities(1), primitive3%partial_densities(2)
call print_error(test='primitive3 = primitive1 / 2.0', is_test_passed=test_passed(2))

primitive3 = primitive1 / 2_I_P
test_passed(3) = primitive3 == (primitive1 / 2_I_P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  / 2 = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      / 2 = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      / 2 = ', primitive3%pressure
print "(A,2(F7.4,1X))", 'densities => 0.0625 / 2 = ', primitive3%partial_densities(1), primitive3%partial_densities(2)
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
    print "(A,2(F7.4,1X))", '  densities = ', primitive1%partial_densities(1), primitive1%partial_densities(2)
    print*, 'primitive2:'
    print "(A,F6.3)",       '  density   = ', primitive2%density
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive2%velocity
    print "(A,F6.3)",       '  pressure  = ', primitive2%pressure
    print "(A,2(F7.4,1X))", '  densities = ', primitive2%partial_densities(1), primitive2%partial_densities(2)
    print*, 'primitive3:'
    print "(A,F6.3)",       '  density   = ', primitive3%density
    print "(A,3(F6.3,1X))", '  velocity  = ', primitive3%velocity
    print "(A,F6.3)",       '  pressure  = ', primitive3%pressure
    print "(A,2(F7.4,1X))", '  densities = ', primitive3%partial_densities(1), primitive3%partial_densities(2)
  endif
  endsubroutine print_error
endprogram flow_test_primitive_compressible_multispecie_div
