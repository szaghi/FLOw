!< FLOw test.

program flow_test_primitive_compressible_multispecie_eq
!< FLOw test.

use flow, only : primitive_compressible_multispecie
use penf, only : R_P
use vecfor, only : vector

implicit none
type(primitive_compressible_multispecie) :: primitive1     !< A primitive object.
type(primitive_compressible_multispecie) :: primitive2     !< A primitive object.
type(vector)                             :: velocity       !< A vector object.
logical                                  :: test_passed(8) !< List of passed tests.

test_passed = .false.

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
velocity = 1._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P,    velocity=velocity, pressure=1._R_P)
test_passed(1) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(1)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
velocity = 2._R_P
primitive2 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
test_passed(2) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'velocity => 1     /= 2 = ', test_passed(2)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
velocity = 2._R_P
primitive2 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=2._R_P)
test_passed(3) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'pressure => 1     /= 2 = ', test_passed(3)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=0.125_R_P, velocity=velocity, pressure=1._R_P)
velocity = 2._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P,    velocity=velocity, pressure=2._R_P)
test_passed(4) = (primitive1 == primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(4)
print "(A,L1)", 'velocity => 1     /= 2 = ', test_passed(4)
print "(A,L1)", 'pressure => 1     /= 2 = ', test_passed(4)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P)
velocity = 1._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P)
test_passed(5) = (primitive1 == primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together true'
print "(A,L1)", 'density  => 1 == 1 = ', test_passed(5)
print "(A,L1)", 'velocity => 1 == 1 = ', test_passed(5)
print "(A,L1)", 'pressure => 1 == 1 = ', test_passed(5)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P, partial_densities=[0.5_R_P])
velocity = 1._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P)
test_passed(6) = (primitive1 == primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together true with imcompatible densities'
print "(A,L1)", 'density   => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'velocity  => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'pressure  => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'densities => 0.5 /= null = ', test_passed(6)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P, partial_densities=[0.5_R_P])
velocity = 1._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P, partial_densities=[0.1_R_P])
test_passed(7) = (primitive1 == primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together true with different densities'
print "(A,L1)", 'density   => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'velocity  => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'pressure  => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'densities => 0.5 /= 0.1 = ', test_passed(7)

velocity = 1._R_P
primitive1 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P, partial_densities=[0.5_R_P])
velocity = 1._R_P
primitive2 = primitive_compressible_multispecie(density=1._R_P, velocity=velocity, pressure=1._R_P, partial_densities=[0.5_R_P])
test_passed(8) = (primitive1 == primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together true with densities'
print "(A,L1)", 'density   => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'velocity  => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'pressure  => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'densities => 0.5 == 0.5 = ', test_passed(8)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_compressible_multispecie_eq
