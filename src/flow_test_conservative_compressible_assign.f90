!< FLOw test.

program flow_test_conservative_compressible_assign
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : R_P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(2) !< List of passed tests.

test_passed = .false.

conservative1 = 0.125_R_P
test_passed(1) = (conservative1%density    == 0.125_R_P).and. &
                 (conservative1%momentum%x == 0.125_R_P).and. &
                 (conservative1%momentum%y == 0.125_R_P).and. &
                 (conservative1%momentum%z == 0.125_R_P).and. &
                 (conservative1%energy     == 0.125_R_P)
print "(A,F6.3)",       'density   = ', conservative1%density
print "(A,3(F6.3,1X))", 'momentum  = ', conservative1%momentum
print "(A,F6.3)",       'energy    = ', conservative1%energy

momentum = 1._R_P
conservative1 = conservative_compressible(density=0.125_R_P, momentum=momentum, energy=2._R_P)
conservative2 = conservative1
test_passed(2) = conservative1 == conservative2
print "(A)", ''
print "(A,F6.3)",       'density   = ', conservative2%density
print "(A,3(F6.3,1X))", 'momentum  = ', conservative2%momentum
print "(A,F6.3)",       'energy    = ', conservative2%energy

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_assign
