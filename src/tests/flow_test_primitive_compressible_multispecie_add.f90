!< FLOw test.

program flow_test_primitive_compressible_multispecie_add
!< FLOw test.

use flow, only : primitive_compressible_multispecie
use penf, only : R8P
use vecfor, only : vector

implicit none
type(primitive_compressible_multispecie) :: primitive1     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive2     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive3     !< A primitive object.
type(vector)                             :: velocity       !< A vector object.
logical                                  :: test_passed(1) !< List of passed tests.

test_passed = .false.

velocity = 1._R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P,  &
                                                velocity=velocity,  &
                                                pressure=1._R8P,    &
                                                partial_densities=[0.125_R8P / 2,  0.125_R8P / 2])
velocity = 2._R8P
primitive2 = primitive_compressible_multispecie(density=1._R8P,  &
                                                velocity=velocity,  &
                                                pressure=1._R8P,    &
                                                partial_densities=[1._R8P / 2,  1._R8P / 2])
primitive3 = primitive1 + primitive2
test_passed(1) = primitive3 == (primitive1 + primitive2)
print "(A,F6.3)",       'density   => 0.125  + 1   = ', primitive3%density
print "(A,3(F6.3,1X))", 'velocity  => 1      + 2   = ', primitive3%velocity
print "(A,F6.3)",       'pressure  => 1      + 1   = ', primitive3%pressure
print "(A,2(F7.4,1X))", 'densities => 0.0625 + 0.5 = ', primitive3%partial_densities(1), primitive3%partial_densities(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_compressible_multispecie_add
