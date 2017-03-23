!< FLOw **primitive** class definition.

module flow_primitive_object
!< FLOw **primitive** class definition.
!<
!< **primitive** is a class that handles primitive fluid dynamic variables.
!< @note The operators of assignment (=), multiplication (*), division (/), sum (+) and subtraction (-) have been overloaded.
!< Therefore this module provides a far-complete algebra.

use flow_field_scalar_vectorial, only : field_scalar, field_vectorial
use penf, only : I4P, R8P

implicit none
private
public :: primitive_object

type :: primitive_object
  !< FLOw **primitive** class definition.
  type(field_scalar)              :: density              !< Density field.
  type(field_vectorial)           :: velocity             !< Velocity field.
  type(field_scalar)              :: pressure             !< Pressure field.
  type(field_scalar), allocatable :: partial_densities(:) !< Partial densities fields.
  contains
    ! public operators
    generic :: assignment(=) => assign_primitive, assign_real                   !< Assignment overloading.
    generic :: operator(+) => add                                               !< Operator `+` overloading.
    generic :: operator(/) => div, div_integer, div_real                        !< Operator `/` overloading.
    generic :: operator(*) => mul, mul_integer, integer_mul, mul_real, real_mul !< Operator `*` overloading.
    generic :: operator(-) => sub                                               !< Operator `-` overloading.
    generic :: operator(==) => eq                                               !< Operator `==` overloading.
    generic :: operator(/=) => not_eq                                           !< Operator `/=` overloading.
    generic :: operator(.compatible.) => compatible                             !< Operator `.compatible.` overloading.
    ! private methods
    procedure, pass(lhs), private :: assign_primitive !< Assign primitives.
    procedure, pass(lhs), private :: assign_real      !< Assign real to primitive.
    procedure, pass(lhs), private :: add              !< Add primitives.
    procedure, pass(lhs), private :: div              !< Divide primitives.
    procedure, pass(lhs), private :: div_integer      !< Divide primitive by integer.
    procedure, pass(lhs), private :: div_real         !< Divide primitive by real.
    procedure, pass(lhs), private :: mul              !< Multiply primitives.
    procedure, pass(lhs), private :: mul_integer      !< Multiply primitive for integer.
    procedure, pass(rhs), private :: integer_mul      !< Multiply integer for primitive.
    procedure, pass(lhs), private :: mul_real         !< Multiply primitive for real.
    procedure, pass(rhs), private :: real_mul         !< Multiply real for primitive.
    procedure, pass(lhs), private :: sub              !< Subtract primitives.
    procedure, pass(lhs), private :: eq               !< Compare (`==') primitives.
    procedure, pass(lhs), private :: not_eq           !< Compare (`/=') primitives.
    procedure, pass(lhs), private :: compatible       !< Compare (`.compatible.`) primitives.
endtype primitive_object
contains
  ! private methods
  pure subroutine assign_primitive(lhs, rhs)
  !< Assign primitives.
  class(primitive_object), intent(inout) :: lhs !< Left hand side.
  type(primitive_object),  intent(in)    :: rhs !< Right hand side.

  lhs%density = rhs%density
  lhs%velocity = rhs%velocity
  lhs%pressure = rhs%pressure
  if (allocated(rhs%partial_densities)) lhs%partial_densities = rhs%partial_densities
  endsubroutine assign_primitive

  pure subroutine assign_real(lhs, rhs)
  !< Assign real to primitive.
  class(primitive_object), intent(inout) :: lhs !< Left hand side.
  real(R8P),               intent(in)    :: rhs !< Right hand side.
  integer(I4P)                           :: d   !< Counter.

  lhs%density = rhs
  lhs%velocity = rhs
  lhs%pressure = rhs
  if (allocated(lhs%partial_densities)) then
    do d=1, size(lhs%partial_densities, dim=1)
      lhs%partial_densities(d) = rhs
    enddo
  endif
  endsubroutine assign_real

  elemental function add(lhs, rhs) result(opr)
  !< Add primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density + rhs%density
  opr%velocity = lhs%velocity + rhs%velocity
  opr%pressure = lhs%pressure + rhs%pressure
  if (lhs.compatible.rhs) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) + rhs%partial_densities(d)
    enddo
  endif
  endfunction add

  elemental function div(lhs, rhs) result(opr)
  !< Divide primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density / rhs%density
  opr%velocity = lhs%velocity / rhs%velocity
  opr%pressure = lhs%pressure / rhs%pressure
  if (lhs.compatible.rhs) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) / rhs%partial_densities(d)
    enddo
  endif
  endfunction div

  elemental function div_integer(lhs, rhs) result(opr)
  !< Divide primitive by integer.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  integer(I4P),            intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density / rhs
  opr%velocity = lhs%velocity / rhs
  opr%pressure = lhs%pressure / rhs
  if (allocated(lhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) / rhs
    enddo
  endif
  endfunction div_integer

  elemental function div_real(lhs, rhs) result(opr)
  !< Divide primitive by real.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  real(R8P),               intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density / rhs
  opr%velocity = lhs%velocity / rhs
  opr%pressure = lhs%pressure / rhs
  if (allocated(lhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) / rhs
    enddo
  endif
  endfunction div_real

  elemental function mul(lhs, rhs) result(opr)
  !< Multiply primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density * rhs%density
  opr%velocity = lhs%velocity * rhs%velocity
  opr%pressure = lhs%pressure * rhs%pressure
  if (lhs.compatible.rhs) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) * rhs%partial_densities(d)
    enddo
  endif
  endfunction mul

  elemental function mul_integer(lhs, rhs) result(opr)
  !< Multiply primitive for integer.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  integer(I4P),            intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density * rhs
  opr%velocity = lhs%velocity * rhs
  opr%pressure = lhs%pressure * rhs
  if (allocated(lhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) * rhs
    enddo
  endif
  endfunction mul_integer

  elemental function integer_mul(lhs, rhs) result(opr)
  !< Multiply integer for primitive.
  integer(I4P),            intent(in) :: lhs !< Left hand side.
  class(primitive_object), intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs * rhs%density
  opr%velocity = lhs * rhs%velocity
  opr%pressure = lhs * rhs%pressure
  if (allocated(rhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(rhs%partial_densities, dim=1)))
    do d=1, size(rhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs * rhs%partial_densities(d)
    enddo
  endif
  endfunction integer_mul

  elemental function mul_real(lhs, rhs) result(opr)
  !< Multiply primitive for real.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  real(R8P),               intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density * rhs
  opr%velocity = lhs%velocity * rhs
  opr%pressure = lhs%pressure * rhs
  if (allocated(lhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) * rhs
    enddo
  endif
  endfunction mul_real

  elemental function real_mul(lhs, rhs) result(opr)
  !< Multiply real for primitive.
  real(R8P),               intent(in) :: lhs !< Left hand side.
  class(primitive_object), intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs * rhs%density
  opr%velocity = lhs * rhs%velocity
  opr%pressure = lhs * rhs%pressure
  if (allocated(rhs%partial_densities)) then
    allocate(opr%partial_densities(1:size(rhs%partial_densities, dim=1)))
    do d=1, size(rhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs * rhs%partial_densities(d)
    enddo
  endif
  endfunction real_mul

  elemental function sub(lhs, rhs) result(opr)
  !< Subtract primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr%density = lhs%density - rhs%density
  opr%velocity = lhs%velocity - rhs%velocity
  opr%pressure = lhs%pressure - rhs%pressure
  if (lhs.compatible.rhs) then
    allocate(opr%partial_densities(1:size(lhs%partial_densities, dim=1)))
    do d=1, size(lhs%partial_densities, dim=1)
      opr%partial_densities(d) = lhs%partial_densities(d) - rhs%partial_densities(d)
    enddo
  endif
  endfunction sub

  pure function eq(lhs, rhs) result(opr)
  !< Compare (`==`) primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  logical                             :: opr !< Operator result.
  integer(I4P)                        :: d   !< Counter.

  opr = lhs%density == rhs%density
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
  endfunction eq

  pure function not_eq(lhs, rhs) result(opr)
  !< Compare (`/=`) primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  logical                             :: opr !< Operator result.

  opr = .not.(lhs%eq(rhs=rhs))
  endfunction not_eq

  pure function compatible(lhs, rhs) result(opr)
  !< Check primitives compatibility (size of partial densities).
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  logical                             :: opr !< Operator result.

  opr = allocated(lhs%partial_densities).and.allocated(rhs%partial_densities)
  if (opr) opr = size(lhs%partial_densities, dim=1) == size(rhs%partial_densities, dim=1)
  endfunction compatible
endmodule flow_primitive_object
