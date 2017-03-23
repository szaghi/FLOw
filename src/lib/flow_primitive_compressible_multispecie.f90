!< FLOw **primitive** compressible mutlispecie object.

module flow_primitive_compressible_multispecie
!< FLOw **primitive** compressible mutlispecie object.
!<
!< [[primitive_compressible_multispecie]] is a class that handles compressible multispecie primitive fluid dynamic variables.

use flow_field_object, only : field_object
use flow_primitive_object, only : primitive_object
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
private
public :: primitive_compressible_multispecie

type, extends(primitive_object) :: primitive_compressible_multispecie
   !< **Primitive** compressible multispecie object.
   real(R8P)                :: density              !< Density field.
   type(vector)             :: velocity             !< Velocity field.
   real(R8P)                :: pressure             !< Pressure field.
   real(R8P),   allocatable :: partial_densities(:) !< Partial densities fields.
   contains
      ! deferred methods
      procedure, pass(lhs) :: assign_field !< Operator `=`.
      procedure, pass(lhs) :: assign_real  !< Operator `field = real`.
      procedure, pass(lhs) :: add          !< Operator `+`.
      procedure, pass(lhs) :: div          !< Operator `/`.
      procedure, pass(lhs) :: div_integer  !< Operator `field / integer`.
      procedure, pass(lhs) :: div_real     !< Operator `field / real`.
      procedure, pass(lhs) :: mul          !< Operator `*`.
      procedure, pass(lhs) :: mul_integer  !< Operator `field * integer`.
      procedure, pass(rhs) :: integer_mul  !< Operator `integer * field`.
      procedure, pass(lhs) :: mul_real     !< Operator `field * real`.
      procedure, pass(rhs) :: real_mul     !< Operator `real * field`.
      procedure, pass(lhs) :: sub          !< Operator `-`.
      procedure, pass(lhs) :: pow_integer  !< Operator `field ** integer`.
      procedure, pass(lhs) :: pow_real     !< Operator `field ** real`.
      procedure, pass(lhs) :: eq           !< Operator `=='.
      procedure, pass(lhs) :: not_eq       !< Operator `/='.
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
   real(R8P),                                 intent(in)    :: rhs !< Right hand side.

                                         lhs%density           = rhs
                                         lhs%velocity          = rhs
                                         lhs%pressure          = rhs
   if (allocated(lhs%partial_densities)) lhs%partial_densities = rhs
   endsubroutine assign_real

   elemental function add(lhs, rhs) result(opr)
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

   elemental function div(lhs, rhs) result(opr)
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

   elemental function div_integer(lhs, rhs) result(opr)
   !< Operator `field / integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I4P),                              intent(in) :: rhs !< Right hand side.
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

   elemental function div_real(lhs, rhs) result(opr)
   !< Operator `field / real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R8P),                                 intent(in) :: rhs !< Right hand side.
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

   elemental function mul(lhs, rhs) result(opr)
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

   elemental function mul_integer(lhs, rhs) result(opr)
   !< Operator `field * integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I4P),                              intent(in) :: rhs !< Right hand side.
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

   elemental function integer_mul(lhs, rhs) result(opr)
   !< Operator `integer * field`.
   integer(I4P),                              intent(in) :: lhs !< Left hand side.
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

   elemental function mul_real(lhs, rhs) result(opr)
   !< Operator `field * real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R8P),                                 intent(in) :: rhs !< Right hand side.
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

   elemental function real_mul(lhs, rhs) result(opr)
   !< Operator `real * field`.
   real(R8P),                                 intent(in) :: lhs !< Left hand side.
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

   elemental function sub(lhs, rhs) result(opr)
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

   elemental function pow_integer(lhs, rhs) result(opr)
   !< Operator `field ** integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   integer(I4P),                              intent(in) :: rhs !< Right hand side.
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

   elemental function pow_real(lhs, rhs) result(opr)
   !< Operator `field ** real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R8P),                                 intent(in) :: rhs !< Right hand side.
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
   integer(I4P)                                          :: d   !< Counter.

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
   real(R8P),    intent(in), optional       :: density              !< Density field.
   type(vector), intent(in), optional       :: velocity             !< Velocity field.
   real(R8P),    intent(in), optional       :: pressure             !< Pressure field.
   real(R8P),    intent(in), optional       :: partial_densities(:) !< Partial densities field.
   type(primitive_compressible_multispecie) :: instance !< Instance of [[primitive_compressible_multispecie]].

   if (present(density          )) instance%density           = density
   if (present(velocity         )) instance%velocity          = velocity
   if (present(pressure         )) instance%pressure          = pressure
   if (present(partial_densities)) instance%partial_densities = partial_densities
   endfunction primitive_compressible_multispecie_instance
endmodule flow_primitive_compressible_multispecie
