!< FLOw test.

program flow_test_conservative_compressible_not_eq
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : R_P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(5) !< List of passed tests.

test_passed = .false.

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
momentum = 1._R_P
conservative2 = conservative_compressible(density=1._R_P,    momentum=momentum, energy=1._R_P)
test_passed(1) = (conservative1 /= conservative2).eqv.(.true.)
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(1)

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
momentum = 2._R_P
conservative2 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
test_passed(2) = (conservative1 /= conservative2).eqv.(.true.)
print "(A,L1)", 'momentum => 1     /= 2 = ', test_passed(2)

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
momentum = 2._R_P
conservative2 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=2._R_P)
test_passed(3) = (conservative1 /= conservative2).eqv.(.true.)
print "(A,L1)", 'energy   => 1     /= 2 = ', test_passed(3)

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
momentum = 2._R_P
conservative2 = conservative_compressible(density=1._R_P,    momentum=momentum, energy=2._R_P)
test_passed(4) = (conservative1 /= conservative2).eqv.(.true.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(4)
print "(A,L1)", 'momentum => 1     /= 2 = ', test_passed(4)
print "(A,L1)", 'energy   => 1     /= 2 = ', test_passed(4)

momentum = 1._R_P
conservative1 = conservative_compressible(density=1._R_P, momentum=momentum, energy=1._R_P)
momentum = 1._R_P
conservative2 = conservative_compressible(density=1._R_P, momentum=momentum, energy=1._R_P)
test_passed(5) = (conservative1 /= conservative2).eqv.(.false.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(5)
print "(A,L1)", 'momentum => 1     /= 2 = ', test_passed(5)
print "(A,L1)", 'energy   => 1     /= 2 = ', test_passed(5)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_not_eq
