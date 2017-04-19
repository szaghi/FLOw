!< FLOw test.

program flow_test_primitive_compressible_normalize
!< FLOw test.

use flow, only : primitive_compressible
use penf, only : R_P
use vecfor, only : ex, ey, ez, vector

implicit none
type(primitive_compressible) :: primitive1     !< A primitive object.
type(primitive_compressible) :: primitive2     !< A primitive object.
type(vector)                 :: velocity       !< A vector object.
logical                      :: test_passed(1) !< List of passed tests.

test_passed = .false.

velocity = 1._R_P * ex + 2._R_P * ey + 3._R_P * ez
primitive1 = primitive_compressible(density=1._R_P, velocity=velocity, pressure=9.5_R_P)
velocity = 3._R_P * ez
primitive2 = primitive_compressible(density=1._R_P, velocity=velocity, pressure=9.5_R_P)
call primitive1%normalize(normal=ez)
test_passed(1) = primitive1 == primitive2
print "(A)", 'Normalize (1, [1,2,3], 9.5) with respect [0,0,1]:'
print "(A)", primitive1%description()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_compressible_normalize
