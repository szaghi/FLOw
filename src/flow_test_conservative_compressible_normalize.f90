!< FLOw test.

program flow_test_conservative_compressible_normalize
!< FLOw test.

use flow, only : conservative_compressible, eos_compressible
use penf, only : R_P
use vecfor, only : ex, ey, ez, vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(eos_compressible)          :: eos            !< An equation of state.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(3) !< List of passed tests.

test_passed = .false.

eos = eos_compressible(cp=1040.004_R_P, cv=742.86_R_P)

momentum = 1._R_P * ex + 2._R_P * ey + 3._R_P * ez
conservative1 = conservative_compressible(density=1._R_P, momentum=momentum, energy=9.5_R_P)
momentum = 3._R_P * ez
conservative2 = conservative_compressible(density=1._R_P, momentum=momentum, energy=7._R_P)
call conservative1%normalize(eos=eos, normal=ez)
test_passed(1) = conservative1 == conservative2
print "(A)", 'Normalize (1, [1,2,3], 9.5) with respect [0,0,1]:'
print "(A)", conservative1%description()

momentum = 1._R_P * ex + 2._R_P * ey + 3._R_P * ez
conservative1 = conservative_compressible(density=1._R_P, momentum=momentum, energy=9.5_R_P)
momentum = 2._R_P * ey
conservative2 = conservative_compressible(density=1._R_P, momentum=momentum, energy=4.5_R_P)
call conservative1%normalize(eos=eos, normal=ey)
test_passed(2) = conservative1 == conservative2
print "(A)", 'Normalize (1, [1,2,3], 9.5) with respect [0,1,0]:'
print "(A)", conservative1%description()

momentum = 1._R_P * ex + 2._R_P * ey + 3._R_P * ez
conservative1 = conservative_compressible(density=1._R_P, momentum=momentum, energy=9.5_R_P)
momentum = 1._R_P * ex
conservative2 = conservative_compressible(density=1._R_P, momentum=momentum, energy=3._R_P)
call conservative1%normalize(eos=eos, normal=ex)
test_passed(3) = conservative1 == conservative2
print "(A)", 'Normalize (1, [1,2,3], 9.5) with respect [1,0,0]:'
print "(A)", conservative1%description()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_conservative_compressible_normalize
