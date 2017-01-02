!< FLOw **primitive** class definition.
module flow_primitive_object
!< FLOw **primitive** class definition.
!<
!< **primitive** is a class that handles primitive fluid dynamic variables.
!< @note The operators of assignment (=), multiplication (*), division (/), sum (+) and subtraction (-) have been overloaded.
!< Therefore this module provides a far-complete algebra.
use penf
use vecfor, only : vector

implicit none
private
public :: primitive_object

type, public :: primitive_object
  !< FLOw **primitive** class definition.
  !<
  !< @note **primitive** class can represent multi species fluids. The density component, `r`, is a dynamic memory component
  !< defined as an allocatable rank-1 array. `r` is allocated at runtime with the number of initial species that constitute the
  !< initial fluid mixture.
  !<
  !< @note It is worth noting that the component `r`, `v` and `p` are independent whereas `d` and `g` are dependent from the
  !< previous ones.
  real(R8P), allocatable :: r(:)       !< Density of single species [1:species_number].
  type(vector)           :: v          !< Velocity vector.
  real(R8P)              :: p = 0._R8P !< Pressure.
  real(R8P)              :: d = 0._R8P !< Density, `sum(r(1:Ns))`.
  real(R8P)              :: g = 0._R8P !< Specific heats ratio \( \gamma = \frac{c_p}{c_v} \); it depends on partial densities
                                       !< `r` and on the initial species specific heats.
  contains
    procedure :: destroy         !< Destroy primitive.
    procedure :: alloc           !< Allocate dynamic memory.
    procedure :: compute_density !< Compute the density from the densities of single species.
    procedure :: compute_gamma   !< Compute the specific heats ratio.
    procedure :: to_array        !< Convert primitive to array.
    ! procedure :: set             !< Set primitive.
endtype primitive_object
contains
  ! public methods
  elemental subroutine destroy(self)
  !< Destroy primitive.
  class(primitive_object), intent(inout):: self !< Primitive object.

  if (allocated(self%r)) deallocate(self%r)
  endsubroutine destroy

  elemental subroutine alloc(self, species_number)
  !< Allocate dynamic memory.
  class(primitive_object), intent(inout) :: self           !< Primitive object.
  integer(I4P),            intent(in)    :: species_number !< Number of species.
  integer(I4P)                           :: s              !< Counter.

  if (species_number > 0) self%r = [0._R8P, s = 1, species_number]
  endsubroutine alloc

  elemental subroutine compute_density(self)
  class(primitive_object), intent(inout) :: self !< Primitive object.

  self%d = sum(self%r(:))
  endsubroutine compute_density

  elemental subroutine compute_gamma(self, species0)
  !< Compute the specific heats ratio.
  class(primitive_object), intent(inout) :: self     !< Primitive object.
  type(Type_Species),      intent(IN)    :: species0 !< Initial species.
  real(R8P), allocatable                 :: c(:)     !< Species concentration.

  c = self%r/self%d
  self%g = dot_product(c, species0%heats(:)%cp) / dot_product(c, species0%heats(:)%cv)
  endsubroutine compute_gamma

  pure function to_array(prim) result(array)
  !< Convert primitive to array.
  class(primitive_object), intent(in) :: self                    !< Primitive object.
  real(R8P)                           :: array(1:size(prim%r)+6) !< Primitive data in the form of array.
  integer(I_P)                        :: species_number          !< Number of species.

  species_number = size(prim%r, dim=1)
  array(1:species_number) = self%r
  array(species_number+1) = self%v%x
  array(species_number+2) = self%v%y
  array(species_number+3) = self%v%z
  array(species_number+4) = self%p
  array(species_number+5) = self%d
  array(species_number+6) = self%g
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction to_array
endmodule flow_primitive_object
