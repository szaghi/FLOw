!< Flow test.

program flow_test_eos_compressible
!< Flow test.

use flow, only : eos_compressible
use penf, only : R_P, ZeroR_P

implicit none
type(eos_compressible) :: eos                  !< An equation of state.
logical                :: are_tests_passed(13) !< List of passed tests.

are_tests_passed = .false.

eos = eos_compressible(cp=1040.004_R_P, cv=742.86_R_P)
print "(A)", 'EOS description:'
print "(A)", eos%description()

are_tests_passed(1) = (eos%g() >= 1.4_R_P - ZeroR_P).and.(eos%g() <= 1.4_R_P + ZeroR_P)
print "(A,L1)", 'eos%g() = 1.4, is right? ', are_tests_passed(1)

eos = eos_compressible(cp=1040.004_R_P, gam=1.4_R_P)
are_tests_passed(2) = (eos%cv() >= 742.86_R_P - ZeroR_P).and.(eos%cv() <= 742.86_R_P + ZeroR_P)
print "(A,L1)", 'eos%cv() = 742.86, is right? ', are_tests_passed(2)

eos = eos_compressible(cv=742.86_R_P, gam=1.4_R_P)
are_tests_passed(3) = (eos%R() >= 297.144_R_P - 1000*ZeroR_P).and.(eos%R() <= 297.144_R_P + 1000*ZeroR_P)
print "(A,L1)", 'eos%R() = 297.144, is right? ', are_tests_passed(3)

eos = eos_compressible(cv=742.86_R_P, R=297.144_R_P)
are_tests_passed(4) = (eos%cp() >= 1040.004_R_P - ZeroR_P).and.(eos%cp() <= 1040.004_R_P + ZeroR_P)
print "(A,L1)", 'eos%cp() = 1040.004, is right? ', are_tests_passed(4)

eos = eos_compressible(cp=1040.004_R_P, R=297.144_R_P)
are_tests_passed(5) = (eos%cv() >= 742.86_R_P - 1000*ZeroR_P).and.(eos%cv() <= 742.86_R_P + 1000*ZeroR_P)
print "(A,L1)", 'eos%cv() = 742.86, is right? ', are_tests_passed(5)

eos = eos_compressible(gam=1.4_R_P, R=297.144_R_P)
are_tests_passed(6) = (eos%cp() >= 1040.004_R_P - 1000*ZeroR_P).and.(eos%cp() <= 1040.004_R_P + 1000*ZeroR_P)
print "(A,L1)", 'eos%cp() = 1040.004, is right? ', are_tests_passed(6)

eos = eos_compressible(cp=1040.004_R_P, cv=742.86_R_P)
are_tests_passed(7) = (eos%pressure(density=1._R_P, energy=1._R_P) >= 0.4_R_P - ZeroR_P).and.&
                      (eos%pressure(density=1._R_P, energy=1._R_P) <= 0.4_R_P + ZeroR_P)
print "(A,L1)", 'eos%pressure(density=1, energy=1) = 0.4, is right? ', are_tests_passed(7)

are_tests_passed(8) = (eos%pressure(density=1._R_P, temperature=1._R_P) >= 297.144_R_P - 1000*ZeroR_P).and.&
                      (eos%pressure(density=1._R_P, temperature=1._R_P) <= 297.144_R_P + 1000*ZeroR_P)
print "(A,L1)", 'eos%pressure(density=1, temperature=1) = 297.144, is right? ', are_tests_passed(8)

are_tests_passed(9) = (eos%density(pressure=1._R_P, speed_of_sound=1._R_P) >= 1.4_R_P - ZeroR_P).and.&
                      (eos%density(pressure=1._R_P, speed_of_sound=1._R_P) <= 1.4_R_P + ZeroR_P)
print "(A,L1)", 'eos%density(pressure=1, speed_of_sound=1) = 1.4, is right? ', are_tests_passed(9)

are_tests_passed(10) = (eos%internal_energy(density=1._R_P, pressure=1._R_P) >= 1._R_P/0.4_R_P - ZeroR_P).and.&
                       (eos%internal_energy(density=1._R_P, pressure=1._R_P) <= 1._R_P/0.4_R_P + ZeroR_P)
print "(A,L1)", 'eos%internal_energy(density=1, pressure=1) = 1/0.4, is right? ', are_tests_passed(10)

are_tests_passed(11) = (eos%temperature(density=1._R_P, pressure=1._R_P) >= 1._R_P/297.144_R_P - ZeroR_P).and.&
                       (eos%temperature(density=1._R_P, pressure=1._R_P) <= 1._R_P/297.144_R_P + ZeroR_P)
print "(A,L1)", 'eos%temperature(density=1, pressure=1) = 1/297.144, is right? ', are_tests_passed(11)

are_tests_passed(12) = (eos%temperature(energy=1._R_P) >= 1._R_P/742.86_R_P - ZeroR_P).and.&
                       (eos%temperature(energy=1._R_P) <= 1._R_P/742.86_R_P + ZeroR_P)
print "(A,L1)", 'eos%temperature(energy=1) = 1/742.86, is right? ', are_tests_passed(12)

are_tests_passed(13) = (eos%total_entalpy(pressure=1._R_P,                                  &
                                          density=1._R_P,                                   &
                                          velocity_sq_norm=2._R_P) >= 4.5_R_P - ZeroR_P).and.&
                       (eos%total_entalpy(pressure=1._R_P,                                  &
                                          density=1._R_P,                                   &
                                          velocity_sq_norm=2._R_P) <= 4.5_R_P + ZeroR_P)
print "(A,L1)", 'eos%total_entalpy(pressure=1, density=1, velocity_sq_norm=2) = 4.5, is right? ', are_tests_passed(13)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(are_tests_passed)
endprogram flow_test_eos_compressible
