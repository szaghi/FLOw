!< FLOw test.

program flow_test_conservative_compressible_sub
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : R8P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(conservative_compressible) :: conservative3  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(2) !< List of passed tests.

test_passed = .false.

momentum = 1._R8P
conservative1 = conservative_compressible(density=0.125_R8P, momentum=momentum, energy=1._R8P)
momentum = 2._R8P
conservative2 = conservative_compressible(density=1._R8P, momentum=momentum, energy=1._R8P)
conservative3 = conservative1 - conservative2
test_passed(1) = conservative3 == (conservative1 - conservative2)
print "(A,F6.3)",       'density   => 0.125  - 1   = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 1      - 2   = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 1      - 1   = ', conservative3%energy

momentum = -1._R8P
conservative1 = conservative_compressible(density=0.125_R8P, momentum=momentum, energy=1._R8P)
conservative2 = - conservative1
test_passed(2) = conservative2 == - conservative1
print "(A,F6.3)",       'density   => - (0.125) = ', conservative2%density
print "(A,3(F6.3,1X))", 'momentum  => - (-1)    = ', conservative2%momentum
print "(A,F6.3)",       'energy    => - (1)     = ', conservative2%energy

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_sub
