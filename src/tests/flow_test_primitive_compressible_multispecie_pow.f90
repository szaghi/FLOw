!< FLOw test.

program flow_test_primitive_compressible_multispecie_add
!< FLOw test.

use flow, only : primitive_compressible_multispecie
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
type(primitive_compressible_multispecie) :: primitive1     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive2     !< A primitive object.
type(vector)                             :: velocity       !< A vector object.
logical                                  :: test_passed(4) !< List of passed tests.

test_passed = .false.

velocity = 0.5_R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P, velocity=velocity, pressure=1._R8P)
primitive2 = primitive1 ** 2_I4P
print "(A,F6.3)",       'density  => 0.125 ** 2 = ', primitive2%density
print "(A,3(F6.3,1X))", 'velocity => 0.5   ** 2 = ', primitive2%velocity
print "(A,F6.3)",       'pressure => 1     ** 2 = ', primitive2%pressure
test_passed(1) = primitive2%density == (0.125_R8P ** 2_I4P)

velocity = 0.5_R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P,  &
                                                velocity=velocity,  &
                                                pressure=1._R8P,    &
                                                partial_densities=[0.125_R8P,  0.125_R8P])
primitive2 = primitive1 ** 2_I4P
print "(A,F6.3)",       'density   => 0.125 ** 2 = ', primitive2%density
print "(A,3(F6.3,1X))", 'velocity  => 0.5   ** 2 = ', primitive2%velocity
print "(A,F6.3)",       'pressure  => 1     ** 2 = ', primitive2%pressure
print "(A,2(F6.3,1X))", 'densities => 0.125 ** 2 = ', primitive2%partial_densities
test_passed(2) = primitive2%partial_densities(1) == (0.125_R8P ** 2_I4P)

velocity = 0.5_R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P, velocity=velocity, pressure=1._R8P)
primitive2 = primitive1 ** 2._R8P
print "(A,F6.3)",       'density  => 0.125 ** 2.0 = ', primitive2%density
print "(A,3(F6.3,1X))", 'velocity => 0.5   ** 2.0 = ', primitive2%velocity
print "(A,F6.3)",       'pressure => 1     ** 2.0 = ', primitive2%pressure
test_passed(3) = primitive2%density == (0.125_R8P ** 2._R8P)

velocity = 0.5_R8P
primitive1 = primitive_compressible_multispecie(density=0.125_R8P,  &
                                                velocity=velocity,  &
                                                pressure=1._R8P,    &
                                                partial_densities=[0.125_R8P,  0.125_R8P])
primitive2 = primitive1 ** 2._R8P
print "(A,F6.3)",       'density   => 0.125 ** 2.0 = ', primitive2%density
print "(A,3(F6.3,1X))", 'velocity  => 0.5   ** 2.0 = ', primitive2%velocity
print "(A,F6.3)",       'pressure  => 1     ** 2.0 = ', primitive2%pressure
print "(A,2(F6.3,1X))", 'densities => 0.125 ** 2.0 = ', primitive2%partial_densities
test_passed(4) = primitive2%partial_densities(1) == (0.125_R8P ** 2._R8P)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_compressible_multispecie_add
