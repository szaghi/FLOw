!< FLOw test.

program flow_test_conservative_compressible_sub
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : R_P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(conservative_compressible) :: conservative3  !< A conservative object.
type(conservative_compressible) :: conservative4  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(2) !< List of passed tests.

test_passed = .false.

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
momentum = 2._R_P
conservative2 = conservative_compressible(density=1._R_P, momentum=momentum, energy=1._R_P)
momentum = -1._R_P
conservative3 = conservative_compressible(density=-0.875_R_P, momentum=momentum, energy=0._R_P)
conservative4 = conservative1 - conservative2
test_passed(1) = conservative3 == conservative4
print "(A,F6.3)",       'density   => 0.125  - 1   = ', conservative4%density
print "(A,3(F6.3,1X))", 'momentum  => 1      - 2   = ', conservative4%momentum
print "(A,F6.3)",       'energy    => 1      - 1   = ', conservative4%energy

momentum = -1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=1._R_P)
conservative2 = conservative_compressible(density=-0.125_R_P, momentum=-momentum, energy=-1._R_P)
conservative3 = - conservative1
test_passed(2) = conservative2 == conservative3
print "(A,F6.3)",       'density   => - (0.125) = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => - (-1)    = ', conservative3%momentum
print "(A,F6.3)",       'energy    => - (1)     = ', conservative3%energy

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_sub
