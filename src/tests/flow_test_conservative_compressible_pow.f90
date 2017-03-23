!< FLOw test.

program flow_test_conservative_compressible_add
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(2) !< List of passed tests.

test_passed = .false.

momentum = 0.5_R8P
conservative1 = conservative_compressible(density=0.125_R8P, momentum=momentum, energy=1._R8P)
conservative2 = conservative1 ** 2_I4P
print "(A,F6.3)",       'density  => 0.125 ** 2 = ', conservative2%density
print "(A,3(F6.3,1X))", 'momentum => 0.5   ** 2 = ', conservative2%momentum
print "(A,F6.3)",       'energy   => 1     ** 2 = ', conservative2%energy
test_passed(1) = conservative2%density == (0.125_R8P ** 2_I4P)

momentum = 0.5_R8P
conservative1 = conservative_compressible(density=0.125_R8P, momentum=momentum, energy=1._R8P)
conservative2 = conservative1 ** 2._R8P
print "(A,F6.3)",       'density  => 0.125 ** 2.0 = ', conservative2%density
print "(A,3(F6.3,1X))", 'momentum => 0.5   ** 2.0 = ', conservative2%momentum
print "(A,F6.3)",       'energy   => 1     ** 2.0 = ', conservative2%energy
test_passed(2) = conservative2%density == (0.125_R8P ** 2._R8P)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_add
