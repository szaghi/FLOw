!< FLOw test.
program flow_test_primitive_add
!< FLOw test.
use flow

implicit none
type(primitive_object)    :: primitive1           !< A primitive object.
type(primitive_object)    :: primitive2           !< A primitive object.
type(primitive_object)    :: primitive3           !< A primitive object.
type(field_object_scalar) :: partial_densities(2) !< A scalar field object.
logical                   :: test_passed(1)       !< List of passed tests.

test_passed = .false.

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P
partial_densities(1) = 0.125_R_P / 2
partial_densities(2) = 0.125_R_P / 2
primitive1%partial_densities = partial_densities

primitive2%density = 1._R_P
primitive2%velocity = 2._R_P
primitive2%pressure = 1._R_P
partial_densities(1) = 1._R_P / 2
partial_densities(2) = 1._R_P / 2
primitive2%partial_densities = partial_densities

primitive3 = primitive1 + primitive2
test_passed(1) = primitive3 == (primitive1 + primitive2)
print "(A,F6.3)",       'density   => 0.125  + 1   = ', primitive3%density%field
print "(A,3(F6.3,1X))", 'velocity  => 1      + 2   = ', primitive3%velocity%field
print "(A,F6.3)",       'pressure  => 1      + 1   = ', primitive3%pressure%field
print "(A,2(F7.4,1X))", 'densities => 0.0625 + 0.5 = ', primitive3%partial_densities(1)%field, primitive3%partial_densities(2)%field

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_add
