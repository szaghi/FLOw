!< FLOw test.
program flow_test_primitive_not_eq
!< FLOw test.
use flow

implicit none
type(primitive_object) :: primitive1           !< A primitive object.
type(primitive_object) :: primitive2           !< A primitive object.
type(field_scalar)     :: partial_densities(2) !< A scalar field object.
logical                :: test_passed(8)       !< List of passed tests.

test_passed = .false.

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P

primitive2%density = 1._R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 1._R_P
test_passed(1) = (primitive1 /= primitive2).eqv.(.true.)
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(1)

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P

primitive2%density = 0.125_R_P
primitive2%velocity = 2._R_P
primitive2%pressure = 1._R_P
test_passed(2) = (primitive1 /= primitive2).eqv.(.true.)
print "(A,L1)", 'velocity => 1 /= 2     = ', test_passed(2)

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P

primitive2%density = 0.125_R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 2._R_P
test_passed(3) = (primitive1 /= primitive2).eqv.(.true.)
print "(A,L1)", 'pressure => 1 /= 2     = ', test_passed(3)

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P

primitive2%density = 1._R_P
primitive2%velocity = 2._R_P
primitive2%pressure = 2._R_P
test_passed(4) = (primitive1 /= primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(4)
print "(A,L1)", 'velocity => 1 /= 2     = ', test_passed(4)
print "(A,L1)", 'pressure => 1 /= 2     = ', test_passed(4)

primitive1%density = 0.125_R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P

primitive2%density = 0.125_R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 1._R_P
test_passed(5) = (primitive1 /= primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together'
print "(A,L1)", 'density  => 0.125 /= 1 = ', test_passed(5)
print "(A,L1)", 'velocity => 1 /= 2     = ', test_passed(5)
print "(A,L1)", 'pressure => 1 /= 2     = ', test_passed(5)

primitive1%density = 1._R_P
primitive1%velocity = 1._R_P
primitive1%pressure = 1._R_P
partial_densities(1) = 0.5_R_P
partial_densities(2) = 0.5_R_P
primitive1%partial_densities = partial_densities

primitive2%density = 1._R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 1._R_P
test_passed(6) = (primitive1 /= primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together true with densities'
print "(A,L1)", 'density   => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'velocity  => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'pressure  => 1   == 1    = ', test_passed(6)
print "(A,L1)", 'densities => 0.5 /= null = ', test_passed(6)

primitive2%density = 1._R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 1._R_P
partial_densities(1) = 0.1_R_P
partial_densities(2) = 0.5_R_P
primitive2%partial_densities = partial_densities
test_passed(7) = (primitive1 /= primitive2).eqv.(.true.)
print "(A)", new_line('a')//'all together true with densities'
print "(A,L1)", 'density   => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'velocity  => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'pressure  => 1   == 1   = ', test_passed(7)
print "(A,L1)", 'densities => 0.5 /= 0.1 = ', test_passed(7)

primitive2%density = 1._R_P
primitive2%velocity = 1._R_P
primitive2%pressure = 1._R_P
partial_densities(1) = 0.5_R_P
partial_densities(2) = 0.5_R_P
primitive2%partial_densities = partial_densities
test_passed(8) = (primitive1 /= primitive2).eqv.(.false.)
print "(A)", new_line('a')//'all together true with densities'
print "(A,L1)", 'density   => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'velocity  => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'pressure  => 1   == 1   = ', test_passed(8)
print "(A,L1)", 'densities => 0.5 == 0.5 = ', test_passed(8)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram flow_test_primitive_not_eq
