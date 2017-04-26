!< FLOw **primitive** compressible mutlispecie object.

module flow_primitive_compressible_multispecie
!< FLOw **primitive** compressible mutlispecie object.
!<
!< [[primitive_compressible_multispecie]] is a class that handles compressible multispecie primitive fluid dynamic variables.

use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_primitive_object, only : primitive_object
use penf, only : I_P, R_P, str
use vecfor, only : vector

implicit none
private
public :: primitive_compressible_multispecie

type, extends(primitive_object) :: primitive_compressible_multispecie
   !< **Primitive** compressible multispecie object.
   real(R_P)              :: density=0._R_P       !< Density, `rho`.
   type(vector)           :: velocity             !< Velocity, `v`.
   real(R_P)              :: pressure=0._R_P      !< Pressure, `p`.
   real(R_P), allocatable :: partial_densities(:) !< Partial densities `rho(s), rho = sum(rho(s))`.
   contains
      ! deferred methods
      procedure, pass(self) :: array       !< Return serialized array of field.
      procedure, pass(self) :: description !< Return pretty-printed object description.
      procedure, pass(self) :: destroy     !< Destroy primitive.
      procedure, pass(self) :: energy      !< Return energy value.
      procedure, pass(self) :: initialize  !< Initialize primitive.
      procedure, pass(self) :: momentum    !< Return momentum vector.
      procedure, pass(self) :: normalize   !< Normalize primitive with respect a given normal vector.
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
      ! public operators
      generic :: operator(.compatible.) => compatible !< Operator `.compatible.` overloading.
      ! public methods
      procedure, pass(lhs) :: compatible !< Operator `.compatible.`.
endtype primitive_compressible_multispecie

interface primitive_compressible_multispecie
   !< Overload [[primitive_compressible_multispecie]] name with its constructor.
   module procedure primitive_compressible_multispecie_instance
endinterface

contains
   ! deferred methods
   pure function array(self) result(array_)
   !< Return serialized array of field.
   class(primitive_compressible_multispecie), intent(in) :: self      !< Primitive.
   real(R_P), allocatable                                :: array_(:) !< Serialized array of field.

   if (allocated(self%partial_densities)) then
      allocate(array_(1:5+size(self%partial_densities, dim=1)))
      array_(1)  = self%density
      array_(2)  = self%velocity%x
      array_(3)  = self%velocity%y
      array_(4)  = self%velocity%z
      array_(5)  = self%pressure
      array_(6:) = self%partial_densities(:)
   else
      allocate(array_(1:5))
      array_(1) = self%density
      array_(2) = self%velocity%x
      array_(3) = self%velocity%y
      array_(4) = self%velocity%z
      array_(5) = self%pressure
   endif
   endfunction array

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(primitive_compressible_multispecie), intent(in)           :: self             !< Primitive.
   character(*),                              intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                                   :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                                   :: desc             !< Description.
   character(len=1), parameter                                     :: NL=new_line('a') !< New line character.

   if (allocated(self%partial_densities)) then
      prefix_ = '' ; if (present(prefix)) prefix_ = prefix
      desc = ''
      desc = desc//prefix_//'density           = '//trim(str(n=self%density))//NL
      desc = desc//prefix_//'velocity          = '//trim(str(n=[self%velocity%x, self%velocity%y, self%velocity%z]))//NL
      desc = desc//prefix_//'pressure          = '//trim(str(n=self%pressure))//NL
      desc = desc//prefix_//'partial densities = '//trim(str(n=self%partial_densities))
   else
      prefix_ = '' ; if (present(prefix)) prefix_ = prefix
      desc = ''
      desc = desc//prefix_//'density  = '//trim(str(n=self%density))//NL
      desc = desc//prefix_//'velocity = '//trim(str(n=[self%velocity%x, self%velocity%y, self%velocity%z]))//NL
      desc = desc//prefix_//'pressure = '//trim(str(n=self%pressure))
   endif
   endfunction description

   elemental subroutine destroy(self)
   !< Destroy primitive.
   class(primitive_compressible_multispecie), intent(inout) :: self  !< Primitive.
   type(primitive_compressible_multispecie)                 :: fresh !< Fresh instance of primitive object.

   self = fresh
   endsubroutine destroy

   elemental function energy(self, eos) result(energy_)
   !< Return energy value.
   class(primitive_compressible_multispecie), intent(in) :: self    !< Primitive.
   class(eos_object),                         intent(in) :: eos     !< Equation of state.
   real(R_P)                                             :: energy_ !< Energy value.

   energy_ = self%pressure / (eos%g() - 1._R_P) + 0.5_R_P * self%density * self%velocity%sq_norm()
   endfunction energy

   subroutine initialize(self, initial_state)
   !< Initialize primitive.
   class(primitive_compressible_multispecie), intent(inout)         :: self          !< Primitive.
   class(primitive_object),                    intent(in), optional :: initial_state !< Initial state.

   call self%destroy
   if (present(initial_state)) then
      select type(initial_state)
      class is(primitive_compressible_multispecie)
         self = initial_state
      endselect
   endif
   endsubroutine initialize

   elemental function momentum(self) result(momentum_)
   !< Return momentum vector.
   class(primitive_compressible_multispecie), intent(in) :: self      !< Primitive.
   type(vector)                                          :: momentum_ !< Momentum vector.

   momentum_ = self%density * self%velocity
   endfunction momentum

   elemental subroutine normalize(self, normal)
   !< *Normalize* primitive with respect a given normal vector.
   class(primitive_compressible_multispecie), intent(inout) :: self   !< Primitive.
   type(vector),                              intent(in)    :: normal !< Normal vector.

   self%velocity = self%velocity .paral. normal
   endsubroutine normalize

   ! deferred oprators
   elemental subroutine assign_field(lhs, rhs)
   !< Operator `=`.
   class(primitive_compressible_multispecie), intent(inout) :: lhs !< Left hand side.
   class(field_object),                       intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(primitive_compressible_multispecie)
                                            lhs%density           = rhs%density
                                            lhs%velocity          = rhs%velocity
                                            lhs%pressure          = rhs%pressure
      if (allocated(rhs%partial_densities)) lhs%partial_densities = rhs%partial_densities
   endselect
   endsubroutine assign_field

   elemental subroutine assign_real(lhs, rhs)
   !< Operator `field = real`.
   class(primitive_compressible_multispecie), intent(inout) :: lhs !< Left hand side.
   real(R_P),                                 intent(in)    :: rhs !< Right hand side.

                                         lhs%density           = rhs
                                         lhs%velocity          = rhs
                                         lhs%pressure          = rhs
   if (allocated(lhs%partial_densities)) lhs%partial_densities = rhs
   endsubroutine assign_real

   function positive(self) result(opr)
   !< Unary operator `+ field`.
   class(primitive_compressible_multispecie), intent(in) :: self !< Primitive.
   class(field_object), allocatable                      :: opr  !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                             opr%density           = self%density
                                             opr%velocity          = self%velocity
                                             opr%pressure          = self%pressure
      if (allocated(self%partial_densities)) opr%partial_densities = self%partial_densities
   endselect
   endfunction positive

   pure function add(lhs, rhs) result(opr)
   !< Operator `+`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
      select type(rhs)
      class is(primitive_compressible_multispecie)
                                 opr%density           = lhs%density           + rhs%density
                                 opr%velocity          = lhs%velocity          + rhs%velocity
                                 opr%pressure          = lhs%pressure          + rhs%pressure
         if (lhs.compatible.rhs) opr%partial_densities = lhs%partial_densities + rhs%partial_densities
      endselect
   endselect
   endfunction add

   pure function div(lhs, rhs) result(opr)
   !< Operator `/`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
      select type(rhs)
      class is(primitive_compressible_multispecie)
                                 opr%density           = lhs%density           / rhs%density
                                 opr%velocity          = lhs%velocity          / rhs%velocity
                                 opr%pressure          = lhs%pressure          / rhs%pressure
         if (lhs.compatible.rhs) opr%partial_densities = lhs%partial_densities / rhs%partial_densities
      endselect
   endselect
   endfunction div

   pure function div_integer(lhs, rhs) result(opr)
   !< Operator `field / integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I_P),                              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           / rhs
                                            opr%velocity          = lhs%velocity          / rhs
                                            opr%pressure          = lhs%pressure          / rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities / rhs
   endselect
   endfunction div_integer

   pure function div_real(lhs, rhs) result(opr)
   !< Operator `field / real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R_P),                                 intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           / rhs
                                            opr%velocity          = lhs%velocity          / rhs
                                            opr%pressure          = lhs%pressure          / rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities / rhs
   endselect
   endfunction div_real

   pure function mul(lhs, rhs) result(opr)
   !< Operator `*`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
      select type(rhs)
      class is(primitive_compressible_multispecie)
                                 opr%density           = lhs%density           * rhs%density
                                 opr%velocity          = lhs%velocity          * rhs%velocity
                                 opr%pressure          = lhs%pressure          * rhs%pressure
         if (lhs.compatible.rhs) opr%partial_densities = lhs%partial_densities * rhs%partial_densities
      endselect
   endselect
   endfunction mul

   pure function mul_integer(lhs, rhs) result(opr)
   !< Operator `field * integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I_P),                              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           * rhs
                                            opr%velocity          = lhs%velocity          * rhs
                                            opr%pressure          = lhs%pressure          * rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities * rhs
   endselect
   endfunction mul_integer

   pure function integer_mul(lhs, rhs) result(opr)
   !< Operator `integer * field`.
   integer(I_P),                              intent(in) :: lhs !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs * rhs%density
                                            opr%velocity          = lhs * rhs%velocity
                                            opr%pressure          = lhs * rhs%pressure
      if (allocated(rhs%partial_densities)) opr%partial_densities = lhs * rhs%partial_densities
   endselect
   endfunction integer_mul

   pure function mul_real(lhs, rhs) result(opr)
   !< Operator `field * real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R_P),                                 intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           * rhs
                                            opr%velocity          = lhs%velocity          * rhs
                                            opr%pressure          = lhs%pressure          * rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities * rhs
   endselect
   endfunction mul_real

   pure function real_mul(lhs, rhs) result(opr)
   !< Operator `real * field`.
   real(R_P),                                 intent(in) :: lhs !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs * rhs%density
                                            opr%velocity          = lhs * rhs%velocity
                                            opr%pressure          = lhs * rhs%pressure
      if (allocated(rhs%partial_densities)) opr%partial_densities = lhs * rhs%partial_densities
   endselect
   endfunction real_mul

   function negative(self) result(operator_result)
   !< Unary operator `- field`.
   class(primitive_compressible_multispecie), intent(in) :: self            !< Primitive.
   class(field_object), allocatable                      :: operator_result !< Operator result.

   allocate(primitive_compressible_multispecie :: operator_result)
   select type(operator_result)
   class is(primitive_compressible_multispecie)
                                             operator_result%density           = - self%density
                                             operator_result%velocity          = - self%velocity
                                             operator_result%pressure          = - self%pressure
      if (allocated(self%partial_densities)) operator_result%partial_densities = - self%partial_densities
   endselect
   endfunction negative

   pure function sub(lhs, rhs) result(opr)
   !< Operator `-`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
      select type(rhs)
      class is(primitive_compressible_multispecie)
                                 opr%density           = lhs%density           - rhs%density
                                 opr%velocity          = lhs%velocity          - rhs%velocity
                                 opr%pressure          = lhs%pressure          - rhs%pressure
         if (lhs.compatible.rhs) opr%partial_densities = lhs%partial_densities - rhs%partial_densities
      endselect
   endselect
   endfunction sub

   pure function pow_integer(lhs, rhs) result(opr)
   !< Operator `field ** integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I_P),                              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           ** rhs
                                            opr%velocity%x        = lhs%velocity%x        ** rhs
                                            opr%velocity%y        = lhs%velocity%y        ** rhs
                                            opr%velocity%z        = lhs%velocity%z        ** rhs
                                            opr%pressure          = lhs%pressure          ** rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities ** rhs
   endselect
   endfunction pow_integer

   pure function pow_real(lhs, rhs) result(opr)
   !< Operator `field ** real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R_P),                                 intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable                      :: opr !< Operator result.

   allocate(primitive_compressible_multispecie :: opr)
   select type(opr)
   class is(primitive_compressible_multispecie)
                                            opr%density           = lhs%density           ** rhs
                                            opr%velocity%x        = lhs%velocity%x        ** rhs
                                            opr%velocity%y        = lhs%velocity%y        ** rhs
                                            opr%velocity%z        = lhs%velocity%z        ** rhs
                                            opr%pressure          = lhs%pressure          ** rhs
      if (allocated(lhs%partial_densities)) opr%partial_densities = lhs%partial_densities ** rhs
   endselect
   endfunction pow_real

   elemental function eq(lhs, rhs) result(opr)
   !< Operator `=='.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.
   integer(I_P)                                          :: d   !< Counter.

   select type(rhs)
   class is(primitive_compressible_multispecie)
               opr = lhs%density  == rhs%density
      if (opr) opr = lhs%velocity == rhs%velocity
      if (opr) opr = lhs%pressure == rhs%pressure
      if (opr.and.allocated(lhs%partial_densities).and.allocated(rhs%partial_densities)) then
         if (opr) opr = lhs.compatible.rhs
         if (opr) then
            do d=1, size(lhs%partial_densities, dim=1)
               opr = lhs%partial_densities(d) == rhs%partial_densities(d)
               if (.not.opr) exit
            enddo
         endif
      elseif (opr.and.allocated(lhs%partial_densities).and.(.not.allocated(rhs%partial_densities))) then
         opr = .false.
      elseif (opr.and.(.not.allocated(lhs%partial_densities)).and.allocated(rhs%partial_densities)) then
         opr = .false.
      endif
   endselect
   endfunction eq

   elemental function not_eq(lhs, rhs) result(opr)
   !< Operator `/='.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.

   opr = .not.(lhs%eq(rhs=rhs))
   endfunction not_eq

   ! public methods
   elemental function compatible(lhs, rhs) result(opr)
   !< Operator `.compatible.`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   type(primitive_compressible_multispecie),  intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.

   opr = allocated(lhs%partial_densities).and.allocated(rhs%partial_densities)
   if (opr) opr = size(lhs%partial_densities, dim=1) == size(rhs%partial_densities, dim=1)
   endfunction compatible

   ! private non TBP
   pure function primitive_compressible_multispecie_instance(density, velocity, pressure, partial_densities) result(instance)
   !< Return and instance of [[primitive_compressible_multispecie]].
   !<
   !< @note This procedure is used for overloading [[primitive_compressible_multispecie]] name.
   real(R_P),    intent(in), optional       :: density              !< Density field.
   type(vector), intent(in), optional       :: velocity             !< Velocity field.
   real(R_P),    intent(in), optional       :: pressure             !< Pressure field.
   real(R_P),    intent(in), optional       :: partial_densities(:) !< Partial densities field.
   type(primitive_compressible_multispecie) :: instance !< Instance of [[primitive_compressible_multispecie]].

   if (present(density          )) instance%density           = density
   if (present(velocity         )) instance%velocity          = velocity
   if (present(pressure         )) instance%pressure          = pressure
   if (present(partial_densities)) instance%partial_densities = partial_densities
   endfunction primitive_compressible_multispecie_instance
endmodule flow_primitive_compressible_multispecie
