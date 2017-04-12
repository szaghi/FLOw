!< FLOw test.

program flow_test_compressible_transformations
!< FLOw test.

use flow, only : eos_compressible, conservative_compressible, primitive_compressible, &
                 conservative_to_primitive_compressible, primitive_to_conservative_compressible
use penf, only : R_P, Zero

implicit none
type(eos_compressible)          :: eos                 !< An equation of state.
type(primitive_compressible)    :: p                   !< A primitive compressible instance.
type(conservative_compressible) :: u                   !< A conservative compressible instance.
logical                         :: are_tests_passed(2) !< List of passed tests.

are_tests_passed = .false.

eos = eos_compressible(cp=1040.004_R_P, cv=742.86_R_P)
u = conservative_compressible(density=1._R_P, energy=2.5_R_P)
p = conservative_to_primitive_compressible(conservative=u, eos=eos)
are_tests_passed(1) = (p%density  >= 1._R_P - Zero).and.(p%density  <= 1._R_P + Zero).and. &
                      (p%velocity >= 0._R_P - Zero).and.(p%velocity <= 0._R_P + Zero).and. &
                      (p%pressure >= 1._R_P - Zero).and.(p%pressure <= 1._R_P + Zero)
print "(A,L1)", 'u to p, is done right? ', are_tests_passed(1)

p = primitive_compressible(density=1._R_P, pressure=1._R_P)
u = primitive_to_conservative_compressible(primitive=p, eos=eos)
are_tests_passed(2) = (u%density  >= 1._R_P  - Zero).and.(u%density  <= 1._R_P  + Zero).and. &
                      (u%momentum >= 0._R_P  - Zero).and.(u%momentum <= 0._R_P  + Zero).and. &
                      (u%energy   >= 2.5_R_P - Zero).and.(u%energy   <= 2.5_R_P + Zero)
print "(A,L1)", 'p to u, is done right? ', are_tests_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(are_tests_passed)
endprogram flow_test_compressible_transformations
