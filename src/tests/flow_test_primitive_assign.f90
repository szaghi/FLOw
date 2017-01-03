!< FLOw test.
program flow_test_primitive_assign
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1     !< A primitive object.
type(primitive_object) :: primitive2     !< A primitive object.
logical                :: test_passed(2) !< List of passed tests.

test_passed = .false.

primitive1 = 0.125_R_P
test_passed(1) = (primitive1%density%field == 0.125_R_P).and.    &
                 (primitive1%velocity%field%x == 0.125_R_P).and. &
                 (primitive1%velocity%field%y == 0.125_R_P).and. &
                 (primitive1%velocity%field%z == 0.125_R_P).and. &
                 (primitive1%pressure%field == 0.125_R_P)
print "(A,L1)", 'primitive1 = 0.125, is correct? ', test_passed(1)

primitive2 = primitive1
test_passed(2) = primitive1 == primitive2
print "(A,L1)", 'primitive2 = 0.125, is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_assign
