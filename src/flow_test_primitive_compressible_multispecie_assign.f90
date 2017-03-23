!< FLOw test.

program flow_test_primitive_compressible_multispecie_assign
!< FLOw test.

use flow, only : primitive_compressible_multispecie
use penf, only : R8P
use vecfor, only : vector

implicit none
type(primitive_compressible_multispecie) :: primitive1     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive2     !< A primitive object.
type(vector)                             :: velocity       !< A vector object.
logical                                  :: test_passed(2) !< List of passed tests.

test_passed = .false.

primitive1 = 0.125_R8P
test_passed(1) = (primitive1%density    == 0.125_R8P).and. &
                 (primitive1%velocity%x == 0.125_R8P).and. &
                 (primitive1%velocity%y == 0.125_R8P).and. &
                 (primitive1%velocity%z == 0.125_R8P).and. &
                 (primitive1%pressure   == 0.125_R8P)
print "(A,F6.3)",       'density   = ', primitive1%density
print "(A,3(F6.3,1X))", 'velocity  = ', primitive1%velocity
print "(A,F6.3)",       'pressure  = ', primitive1%pressure

velocity = 1._R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P,  &
                                                velocity=velocity,  &
                                                pressure=1._R8P,    &
                                                partial_densities=[0.125_R8P / 2,  0.125_R8P / 2])
primitive2 = primitive1
test_passed(2) = primitive1 == primitive2
print "(A)", ''
print "(A,F6.3)",       'density   = ', primitive2%density
print "(A,3(F6.3,1X))", 'velocity  = ', primitive2%velocity
print "(A,F6.3)",       'pressure  = ', primitive2%pressure
print "(A,2(F7.4,1X))", 'densities = ', primitive2%partial_densities(1), primitive2%partial_densities(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_compressible_multispecie_assign
