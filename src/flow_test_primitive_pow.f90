!< FLOw test.
program flow_test_primitive_add
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1     !< A primitive object.
type(primitive_object) :: primitive2     !< A primitive object.
logical                :: test_passed(2) !< List of passed tests.

test_passed = .false.

primitive1%density = 0.125_R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P

primitive2%density = primitive1%density ** 2_I_P
primitive2%velocity = primitive1%velocity ** 2_I_P
primitive2%pressure = primitive1%pressure ** 2_I_P
print "(A,F6.3)",       'density  => 0.125 ** 2 = ', primitive2%density%field
print "(A,3(F6.3,1X))", 'velocity => 1     ** 2 = ', primitive2%velocity%field
print "(A,F6.3)",       'pressure => 1     ** 2 = ', primitive2%pressure%field
test_passed(1) = primitive2%density%field == (0.125_R_P ** 2_I_P)

primitive2%density = primitive1%density ** 2._R_P
primitive2%velocity = primitive1%velocity ** 2._R_P
primitive2%pressure = primitive1%pressure ** 2._R_P
print "(A)", ''
print "(A,F6.3)",       'density  => 0.125 ** 2.0 = ', primitive2%density%field
print "(A,3(F6.3,1X))", 'velocity => 1     ** 2.0 = ', primitive2%velocity%field
print "(A,F6.3)",       'pressure => 1     ** 2.0 = ', primitive2%pressure%field
test_passed(2) = primitive2%density%field == (0.125_R_P ** 2._R_P)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_add
