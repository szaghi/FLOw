!< FLOw **conservative** compressible object.

module flow_conservative_compressible
!< FLOw **conservative** compressible object.
!<
!< [[conservative_compressible]] is a class that handles compressible conservative fluid dynamic variables.

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_conservative_object, only : conservative_object
use penf, only : I_P, R_P, str
use vecfor, only : vector

implicit none
private
public :: conservative_compressible
public :: conservative_compressible_pointer

type, extends(conservative_object) :: conservative_compressible
   !< **Conservative** compressible multispecie object.
   real(R_P)    :: density=0._R_P !< Density, `rho`.
   type(vector) :: momentum       !< Momentum, `rho * v`, `rho` being the density and `v` the velocity vector.
   real(R_P)    :: energy=0._R_P  !< Energy, `rho * E`, `rho` being the density and `E` the specific energy.
   contains
      ! public methods
      procedure, pass(self) :: compute_fluxes_from_primitive !< Compute conservative fluxes from primitives at interface.
      ! deferred methods
      procedure, pass(self) :: array          !< Return serialized array of field.
      procedure, pass(self) :: compute_fluxes !< Compute conservative fluxes.
      procedure, pass(self) :: description    !< Return pretty-printed object description.
      procedure, pass(self) :: destroy        !< Destroy conservative.
      procedure, pass(self) :: initialize     !< Initialize conservative.
      procedure, pass(self) :: pressure       !< Return pressure value.
      procedure, pass(self) :: velocity       !< Return velocity vector.
      ! deferred operators
      procedure, pass(lhs)  :: assign_field !< Operator `=`.
      procedure, pass(lhs)  :: assign_real  !< Operator `field = real`.
      procedure, pass(self) :: positive     !< Unary operator `+ field`.
      procedure, pass(lhs)  :: add          !< Operator `+`.
      procedure, pass(lhs)  :: div          !< Operator `/`.
      procedure, pass(lhs)  :: div_integer  !< Operator `field / integer`.
      procedure, pass(lhs)  :: div_real     !< Operator `field / real`.
      procedure, pass(lhs)  :: mul          !< Operator `*`.
      procedure, pass(lhs)  :: mul_integer  !< Operator `field * integer`.
      procedure, pass(rhs)  :: integer_mul  !< Operator `integer * field`.
      procedure, pass(lhs)  :: mul_real     !< Operator `field * real`.
      procedure, pass(rhs)  :: real_mul     !< Operator `real * field`.
      procedure, pass(self) :: negative     !< Unary operator `- field`.
      procedure, pass(lhs)  :: sub          !< Operator `-`.
      procedure, pass(lhs)  :: pow_integer  !< Operator `field ** integer`.
      procedure, pass(lhs)  :: pow_real     !< Operator `field ** real`.
      procedure, pass(lhs)  :: eq           !< Operator `=='.
      procedure, pass(lhs)  :: not_eq       !< Operator `/='.
endtype conservative_compressible

interface conservative_compressible
   !< Overload [[conservative_compressible]] name with its constructor.
   module procedure conservative_compressible_instance
endinterface

contains
   ! public non TBP
   function conservative_compressible_pointer(to, error_message) result(pointer_)
   !< Return [[conservative_compressible]] pointer associated to [[conservative_object]] or its extensions until
   !< [[conservative_compressible]] included.
   !<
   !< @note A type-guard check is performed and error stop is raised if necessary.
   class(conservative_object), intent(in), target   :: to            !< Target of associate.
   character(*),               intent(in), optional :: error_message !< Auxiliary error message.
   class(conservative_compressible), pointer        :: pointer_      !< Associated pointer.

   select type(to)
   type is(conservative_compressible)
      pointer_ => to
   class default
      write(stderr, '(A)') 'error: cast conservative_object to conservative_compressible failed!'
      if (present(error_message)) write(stderr, '(A)') error_message
      stop
   endselect
   endfunction conservative_compressible_pointer

   ! public methods
   elemental subroutine compute_fluxes_from_primitive(self, eos, p, r, u, normal)
   !< Compute conservative fluxes from primitives at interface.
   class(conservative_compressible), intent(inout) :: self   !< Conservative.
   class(eos_object),                intent(in)    :: eos    !< Equation of state.
   real(R_P),                        intent(in)    :: p      !< Pressure at interface.
   real(R_P),                        intent(in)    :: r      !< Density at interface.
   real(R_P),                        intent(in)    :: u      !< Velocity (normal component) at interface.
   type(vector),                     intent(in)    :: normal !< Normal (versor) of face where fluxes are given.

   self%density = r * u
   self%momentum = (r * u * u + p) * normal
   self%energy = (r * eos%internal_energy(density=r, pressure=p) + r * u * u * 0.5_R_P + p) * u
   endsubroutine compute_fluxes_from_primitive

   ! deferred methods
   pure function array(self) result(array_)
   !< Return serialized array of field.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   real(R_P), allocatable                       :: array_(:) !< Serialized array of field.

   allocate(array_(1:5))
   array_(1) = self%density
   array_(2) = self%momentum%x
   array_(3) = self%momentum%y
   array_(4) = self%momentum%z
   array_(5) = self%energy
   endfunction array

   subroutine compute_fluxes(self, eos, normal, fluxes)
   !< Compute conservative fluxes.
   class(conservative_compressible), intent(in)  :: self             !< Conservative.
   class(eos_object),                intent(in)  :: eos              !< Equation of state.
   type(vector),                     intent(in)  :: normal           !< Normal (versor) of face where fluxes are given.
   class(conservative_object),       intent(out) :: fluxes           !< Conservative fluxes.
   real(R_P)                                     :: pressure_        !< Pressure value.
   type(vector)                                  :: velocity_        !< Velocity vector.
   real(R_P)                                     :: velocity_normal_ !< Velocity component parallel to given normal.

   select type(fluxes)
   class is(conservative_compressible)
      pressure_ = self%pressure(eos=eos)
      velocity_ = self%velocity()
      velocity_normal_ = velocity_.dot.normal
      fluxes%density = self%momentum.dot.normal
      fluxes%momentum = self%density * velocity_ * velocity_normal_ + pressure_ * normal
      fluxes%energy = (self%energy + pressure_) * velocity_normal_
   endselect
   endsubroutine compute_fluxes

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(conservative_compressible), intent(in)           :: self             !< Conservative.
   character(*),                     intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                          :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                          :: desc             !< Description.
   character(len=1), parameter                            :: NL=new_line('a') !< New line character.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = ''
   desc = desc//prefix_//'density  = '//trim(str(n=self%density))//NL
   desc = desc//prefix_//'momentum = '//trim(str(n=[self%momentum%x, self%momentum%y, self%momentum%z]))//NL
   desc = desc//prefix_//'energy   = '//trim(str(n=self%energy))
   endfunction description

   elemental subroutine destroy(self)
   !< Destroy conservative.
   class(conservative_compressible), intent(inout) :: self  !< Conservative.
   type(conservative_compressible)                 :: fresh !< Fresh instance of conservative object.

   self = fresh
   endsubroutine destroy

   subroutine initialize(self, initial_state)
   !< Initialize conservative.
   class(conservative_compressible), intent(inout)        :: self          !< conservative.
   class(conservative_object),       intent(in), optional :: initial_state !< Initial state.

   call self%destroy
   if (present(initial_state)) then
      select type(initial_state)
      class is(conservative_compressible)
         self = initial_state
      endselect
   endif
   endsubroutine initialize

   elemental function pressure(self, eos) result(pressure_)
   !< Return pressure value.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   class(eos_object),                intent(in) :: eos       !< Equation of state.
   real(R_P)                                    :: pressure_ !< Pressure value.
   type(vector)                                 :: velocity_ !< Velocity vector.

   velocity_ = self%velocity()
   pressure_ = (eos%g() - 1._R_P) * (self%energy - 0.5_R_P * self%density * velocity_%sq_norm())
   endfunction pressure

   elemental function velocity(self) result(velocity_)
   !< Return velocity vector.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   type(vector)                                 :: velocity_ !< Velocity vector.

   velocity_ = self%momentum / self%density
   endfunction velocity

   ! deferred oprators
   elemental subroutine assign_field(lhs, rhs)
   !< Operator `=`.
   class(conservative_compressible), intent(inout) :: lhs !< Left hand side.
   class(field_object),              intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(conservative_compressible)
      lhs%density  = rhs%density
      lhs%momentum = rhs%momentum
      lhs%energy   = rhs%energy
   endselect
   endsubroutine assign_field

   elemental subroutine assign_real(lhs, rhs)
   !< Operator `field = real`.
   class(conservative_compressible), intent(inout) :: lhs !< Left hand side.
   real(R_P),                        intent(in)    :: rhs !< Right hand side.

   lhs%density  = rhs
   lhs%momentum = rhs
   lhs%energy   = rhs
   endsubroutine assign_real

   function positive(self) result(opr)
   !< Unary operator `+ field`.
   class(conservative_compressible), intent(in) :: self !< conservative.
   class(field_object), allocatable             :: opr  !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = + self%density
      opr%momentum = + self%momentum
      opr%energy   = + self%energy
   endselect
   endfunction positive

   elemental function add(lhs, rhs) result(opr)
   !< Operator `+`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      select type(rhs)
      class is(conservative_compressible)
         opr%density  = lhs%density  + rhs%density
         opr%momentum = lhs%momentum + rhs%momentum
         opr%energy   = lhs%energy   + rhs%energy
      endselect
   endselect
   endfunction add

   elemental function div(lhs, rhs) result(opr)
   !< Operator `/`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      select type(rhs)
      class is(conservative_compressible)
         opr%density  = lhs%density  / rhs%density
         opr%momentum = lhs%momentum / rhs%momentum
         opr%energy   = lhs%energy   / rhs%energy
      endselect
   endselect
   endfunction div

   elemental function div_integer(lhs, rhs) result(opr)
   !< Operator `field / integer`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   integer(I_P),                     intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs%density  / rhs
      opr%momentum = lhs%momentum / rhs
      opr%energy   = lhs%energy   / rhs
   endselect
   endfunction div_integer

   elemental function div_real(lhs, rhs) result(opr)
   !< Operator `field / real`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   real(R_P),                        intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs%density  / rhs
      opr%momentum = lhs%momentum / rhs
      opr%energy   = lhs%energy   / rhs
   endselect
   endfunction div_real

   elemental function mul(lhs, rhs) result(opr)
   !< Operator `*`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      select type(rhs)
      class is(conservative_compressible)
         opr%density  = lhs%density  * rhs%density
         opr%momentum = lhs%momentum * rhs%momentum
         opr%energy   = lhs%energy   * rhs%energy
      endselect
   endselect
   endfunction mul

   elemental function mul_integer(lhs, rhs) result(opr)
   !< Operator `field * integer`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   integer(I_P),                     intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs%density  * rhs
      opr%momentum = lhs%momentum * rhs
      opr%energy   = lhs%energy   * rhs
   endselect
   endfunction mul_integer

   elemental function integer_mul(lhs, rhs) result(opr)
   !< Operator `integer * field`.
   integer(I_P),                     intent(in) :: lhs !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs * rhs%density
      opr%momentum = lhs * rhs%momentum
      opr%energy   = lhs * rhs%energy
   endselect
   endfunction integer_mul

   elemental function mul_real(lhs, rhs) result(opr)
   !< Operator `field * real`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   real(R_P),                        intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs%density  * rhs
      opr%momentum = lhs%momentum * rhs
      opr%energy   = lhs%energy   * rhs
   endselect
   endfunction mul_real

   elemental function real_mul(lhs, rhs) result(opr)
   !< Operator `real * field`.
   real(R_P),                        intent(in) :: lhs !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = lhs * rhs%density
      opr%momentum = lhs * rhs%momentum
      opr%energy   = lhs * rhs%energy
   endselect
   endfunction real_mul

   function negative(self) result(opr)
   !< Unary operator `- field`.
   class(conservative_compressible), intent(in) :: self !< conservative.
   class(field_object), allocatable             :: opr  !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density  = - self%density
      opr%momentum = - self%momentum
      opr%energy   = - self%energy
   endselect
   endfunction negative

   elemental function sub(lhs, rhs) result(opr)
   !< Operator `-`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      select type(rhs)
      class is(conservative_compressible)
         opr%density  = lhs%density  - rhs%density
         opr%momentum = lhs%momentum - rhs%momentum
         opr%energy   = lhs%energy   - rhs%energy
      endselect
   endselect
   endfunction sub

   elemental function pow_integer(lhs, rhs) result(opr)
   !< Operator `field ** integer`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   integer(I_P),                     intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density    = lhs%density    ** rhs
      opr%momentum%x = lhs%momentum%x ** rhs
      opr%momentum%y = lhs%momentum%y ** rhs
      opr%momentum%z = lhs%momentum%z ** rhs
      opr%energy     = lhs%energy     ** rhs
   endselect
   endfunction pow_integer

   elemental function pow_real(lhs, rhs) result(opr)
   !< Operator `field ** real`.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   real(R_P),                        intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable             :: opr !< Operator result.

   allocate(conservative_compressible :: opr)
   select type(opr)
   class is(conservative_compressible)
      opr%density    = lhs%density    ** rhs
      opr%momentum%x = lhs%momentum%x ** rhs
      opr%momentum%y = lhs%momentum%y ** rhs
      opr%momentum%z = lhs%momentum%z ** rhs
      opr%energy     = lhs%energy     ** rhs
   endselect
   endfunction pow_real

   elemental function eq(lhs, rhs) result(opr)
   !< Operator `=='.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   logical                                      :: opr !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
               opr = lhs%density  == rhs%density
      if (opr) opr = lhs%momentum == rhs%momentum
      if (opr) opr = lhs%energy   == rhs%energy
   endselect
   endfunction eq

   elemental function not_eq(lhs, rhs) result(opr)
   !< Operator `/='.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   logical                                      :: opr !< Operator result.

   opr = .not.(lhs%eq(rhs=rhs))
   endfunction not_eq

   ! private non TBP
   pure function conservative_compressible_instance(density, velocity, pressure) result(instance)
   !< Return and instance of [[conservative_compressible]].
   !<
   !< @note This procedure is used for overloading [[conservative_compressible]] name.
   real(R_P),    intent(in), optional :: density  !< Density field.
   type(vector), intent(in), optional :: velocity !< Velocity field.
   real(R_P),    intent(in), optional :: pressure !< Pressure field.
   type(conservative_compressible)    :: instance !< Instance of [[conservative_compressible]].

   if (present(density )) instance%density  = density
   if (present(velocity)) instance%momentum = velocity
   if (present(pressure)) instance%energy   = pressure
   endfunction conservative_compressible_instance
endmodule flow_conservative_compressible
