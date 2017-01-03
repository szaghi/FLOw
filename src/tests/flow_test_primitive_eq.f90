!< FLOw test.
program flow_test_primitive_eq
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1     !< A primitive object.
type(primitive_object) :: primitive2     !< A primitive object.
logical                :: test_passed(5) !< List of passed tests.

test_passed = .false.

primitive1%density = 0.125_R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P
primitive2%density = 1._R_P    ; primitive2%velocity = 1._R_P ; primitive2%pressure = 1._R_P
test_passed(1) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(1)

primitive1%density = 0.125_R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P
primitive2%density = 0.125_R_P ; primitive2%velocity = 2._R_P ; primitive2%pressure = 1._R_P
test_passed(2) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'velocity => 1 /= 2     = ', test_passed(2)

primitive1%density = 0.125_R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P
primitive2%density = 0.125_R_P ; primitive2%velocity = 1._R_P ; primitive2%pressure = 2._R_P
test_passed(3) = (primitive1 == primitive2).eqv.(.false.)
print "(A,L1)", 'pressure => 1 /= 2     = ', test_passed(3)

primitive1%density = 0.125_R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P
primitive2%density = 1._R_P    ; primitive2%velocity = 2._R_P ; primitive2%pressure = 2._R_P
test_passed(4) = (primitive1 == primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(4)
print "(A,L1)", 'velocity => 1 /= 2     = ', test_passed(4)
print "(A,L1)", 'pressure => 1 /= 2     = ', test_passed(4)

primitive1%density = 1._R_P ; primitive1%velocity = 1._R_P ; primitive1%pressure = 1._R_P
primitive2%density = 1._R_P ; primitive2%velocity = 1._R_P ; primitive2%pressure = 1._R_P
test_passed(5) = (primitive1 == primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together true'
print "(A,L1)", 'density  => 1 == 1 = ', test_passed(5)
print "(A,L1)", 'velocity => 1 == 1 = ', test_passed(5)
print "(A,L1)", 'pressure => 1 == 1 = ', test_passed(5)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_eq
