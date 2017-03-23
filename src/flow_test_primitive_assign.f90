!< FLOw test.
program flow_test_primitive_assign
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1           !< A primitive object.
type(primitive_object) :: primitive2           !< A primitive object.
type(field_scalar)     :: partial_densities(2) !< A scalar field object.
logical                :: test_passed(2)       !< List of passed tests.

test_passed = .false.

primitive1 = 0.125_R_P
test_passed(1) = (primitive1%density%field == 0.125_R_P).and.    &
                 (primitive1%velocity%field%x == 0.125_R_P).and. &
                 (primitive1%velocity%field%y == 0.125_R_P).and. &
                 (primitive1%velocity%field%z == 0.125_R_P).and. &
                 (primitive1%pressure%field == 0.125_R_P)
print "(A,F6.3)",       'density   = ', primitive1%density%field
print "(A,3(F6.3,1X))", 'velocity  = ', primitive1%velocity%field
print "(A,F6.3)",       'pressure  = ', primitive1%pressure%field

partial_densities(1) = 0.125_R_P / 2
partial_densities(2) = 0.125_R_P / 2
primitive1%partial_densities = partial_densities
primitive2 = primitive1
test_passed(2) = primitive1 == primitive2
print "(A)", ''
print "(A,F6.3)",       'density   = ', primitive2%density%field
print "(A,3(F6.3,1X))", 'velocity  = ', primitive2%velocity%field
print "(A,F6.3)",       'pressure  = ', primitive2%pressure%field
print "(A,2(F7.4,1X))", 'densities = ', primitive2%partial_densities(1)%field, primitive2%partial_densities(2)%field

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_assign
