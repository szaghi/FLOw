!< FLOw test.

program flow_test_conservative_compressible_mul
!< FLOw test.

use flow, only : conservative_compressible
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
type(conservative_compressible) :: conservative1  !< A conservative object.
type(conservative_compressible) :: conservative2  !< A conservative object.
type(conservative_compressible) :: conservative3  !< A conservative object.
type(vector)                    :: momentum       !< A vector object.
logical                         :: test_passed(5) !< List of passed tests.

test_passed = .false.

momentum = 1._R8P
conservative1 = conservative_compressible(density=0.125_R8P, momentum=momentum, energy=1._R8P)
momentum = 2._R8P
conservative2 = conservative_compressible(density=1._R8P, momentum=momentum, energy=1._R8P)

conservative3 = conservative1 * conservative2
test_passed(1) = conservative3 == (conservative1 * conservative2)
print "(A,F6.3)",       'density   => 0.125  * 1 = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 1      * 2 = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 1      * 1 = ', conservative3%energy
call print_error(test='conservative3 = conservative1 * conservative2', is_test_passed=test_passed(1))

conservative3 = 2._R8P * conservative1
test_passed(2) = conservative3 == (2._R8P * conservative1)
print "(A)", ''
print "(A,F6.3)",       'density   => 2.0 * 0.125  = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 2.0 * 1      = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 2.0 * 1      = ', conservative3%energy
call print_error(test='conservative3 = 2.0 * conservative1', is_test_passed=test_passed(2))

conservative3 = conservative1 * 2._R8P
test_passed(3) = conservative3 == (conservative1 * 2._R8P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2.0 = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 1      * 2.0 = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 1      * 2.0 = ', conservative3%energy
call print_error(test='conservative3 = conservative1 * 2.0', is_test_passed=test_passed(3))

conservative3 = 2_I4P * conservative1
test_passed(4) = conservative3 == (2_I4P * conservative1)
print "(A)", ''
print "(A,F6.3)",       'density   => 2 * 0.125  = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 2 * 1      = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 2 * 1      = ', conservative3%energy
call print_error(test='conservative3 = 2 * conservative1', is_test_passed=test_passed(4))

conservative3 = conservative1 * 2_I4P
test_passed(5) = conservative3 == (conservative1 * 2_I4P)
print "(A)", ''
print "(A,F6.3)",       'density   => 0.125  * 2 = ', conservative3%density
print "(A,3(F6.3,1X))", 'momentum  => 1      * 2 = ', conservative3%momentum
print "(A,F6.3)",       'energy    => 1      * 2 = ', conservative3%energy
call print_error(test='conservative3 = conservative1 * 2', is_test_passed=test_passed(5))

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
contains
  subroutine print_error(test, is_test_passed)
  !< Print error message if test is failed.
  character(*), intent(in) :: test           !< Test description.
  logical,      intent(in) :: is_test_passed !< Test result.
  if (.not.is_test_passed) then
    print*, '"'//test//'" failed!'
    print*, 'conservative1:'
    print "(A,F6.3)",       '  density  = ', conservative1%density
    print "(A,3(F6.3,1X))", '  momentum = ', conservative1%momentum
    print "(A,F6.3)",       '  energy   = ', conservative1%energy
    print*, 'conservative2:'
    print "(A,F6.3)",       '  density  = ', conservative2%density
    print "(A,3(F6.3,1X))", '  momentum = ', conservative2%momentum
    print "(A,F6.3)",       '  energy   = ', conservative2%energy
    print*, 'conservative3:'
    print "(A,F6.3)",       '  density  = ', conservative3%density
    print "(A,3(F6.3,1X))", '  momentum = ', conservative3%momentum
    print "(A,F6.3)",       '  energy   = ', conservative3%energy
  endif
  endsubroutine print_error
endprogram flow_test_conservative_compressible_mul
