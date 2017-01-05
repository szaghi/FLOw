!< FLOw **primitive** class definition.
module flow_primitive_object
!< FLOw **primitive** class definition.
!<
!< **primitive** is a class that handles primitive fluid dynamic variables.
!< @note The operators of assignment (=), multiplication (*), division (/), sum (+) and subtraction (-) have been overloaded.
!< Therefore this module provides a far-complete algebra.
use flow_field_objects
use penf

implicit none
private
public :: primitive_object

type :: primitive_object
  !< FLOw **primitive** class definition.
  type(field_object_scalar)    :: density  !< Density field.
  type(field_object_vectorial) :: velocity !< Velocity field.
  type(field_object_scalar)    :: pressure !< Pressure field.
  contains
    ! public operators
    generic :: assignment(=) => assign_primitive, assign_real !< Assignment overloading.
    generic :: operator(+) => add                             !< Operator `+` overloading.
    generic :: operator(/) => div, div_real                   !< Operator `/` overloading.
    generic :: operator(*) => mul, mul_real, real_mul         !< Operator `*` overloading.
    generic :: operator(-) => sub                             !< Operator `-` overloading.
    generic :: operator(==) => eq                             !< Operator `==` overloading.
    generic :: operator(/=) => not_eq                         !< Operator `/=` overloading.
    ! private methods
    procedure, pass(lhs), private :: assign_primitive !< Assign primitives.
    procedure, pass(lhs), private :: assign_real      !< Assign real to primitive.
    procedure, pass(lhs), private :: add              !< Add primitives.
    procedure, pass(lhs), private :: div              !< Divide primitives.
    procedure, pass(lhs), private :: div_real         !< Divide primitive by real.
    procedure, pass(lhs), private :: mul              !< Multiply primitives.
    procedure, pass(lhs), private :: mul_real         !< Multiply primitive for real.
    procedure, pass(rhs), private :: real_mul         !< Multiply real for primitive.
    procedure, pass(lhs), private :: sub              !< Subtract primitives.
    procedure, pass(lhs), private :: eq               !< Compare (`==') primitives.
    procedure, pass(lhs), private :: not_eq           !< Compare (`/=') primitives.
endtype primitive_object
contains
  ! private methods
  subroutine assign_primitive(lhs, rhs)
  !< Assign primitives.
  class(primitive_object), intent(inout) :: lhs !< Left hand side.
  type(primitive_object),  intent(in)    :: rhs !< Right hand side.

  lhs%density = rhs%density
  lhs%velocity = rhs%velocity
  lhs%pressure = rhs%pressure
  endsubroutine assign_primitive

  subroutine assign_real(lhs, rhs)
  !< Assign real to primitive.
  class(primitive_object), intent(inout) :: lhs !< Left hand side.
  real(R_P),               intent(in)    :: rhs !< Right hand side.

  lhs%density = rhs
  lhs%velocity = rhs
  lhs%pressure = rhs
  endsubroutine assign_real

  function add(lhs, rhs) result(opr)
  !< Add primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density + rhs%density
  opr%velocity = lhs%velocity + rhs%velocity
  opr%pressure = lhs%pressure + rhs%pressure
  endfunction add

  function div(lhs, rhs) result(opr)
  !< Divide primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density / rhs%density
  opr%velocity = lhs%velocity / rhs%velocity
  opr%pressure = lhs%pressure / rhs%pressure
  endfunction div

  function div_real(lhs, rhs) result(opr)
  !< Divide primitive by real.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  real(R_P),               intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density / rhs
  opr%velocity = lhs%velocity / rhs
  opr%pressure = lhs%pressure / rhs
  endfunction div_real

  function mul(lhs, rhs) result(opr)
  !< Multiply primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density * rhs%density
  opr%velocity = lhs%velocity * rhs%velocity
  opr%pressure = lhs%pressure * rhs%pressure
  endfunction mul

  function mul_real(lhs, rhs) result(opr)
  !< Multiply primitive for real.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  real(R_P),               intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density * rhs
  opr%velocity = lhs%velocity * rhs
  opr%pressure = lhs%pressure * rhs
  endfunction mul_real

  function real_mul(lhs, rhs) result(opr)
  !< Multiply real for primitive.
  real(R_P),               intent(in) :: lhs !< Left hand side.
  class(primitive_object), intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs * rhs%density
  opr%velocity = lhs * rhs%velocity
  opr%pressure = lhs * rhs%pressure
  endfunction real_mul

  function sub(lhs, rhs) result(opr)
  !< Subtract primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  type(primitive_object)              :: opr !< Operator result.

  opr%density = lhs%density - rhs%density
  opr%velocity = lhs%velocity - rhs%velocity
  opr%pressure = lhs%pressure - rhs%pressure
  endfunction sub

  function eq(lhs, rhs) result(opr)
  !< Compare (`==`) primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  logical                             :: opr !< Operator result.

  opr = lhs%density == rhs%density
  if (opr) opr = lhs%velocity == rhs%velocity
  if (opr) opr = lhs%pressure == rhs%pressure
  endfunction eq

  function not_eq(lhs, rhs) result(opr)
  !< Compare (`/=`) primitives.
  class(primitive_object), intent(in) :: lhs !< Left hand side.
  type(primitive_object),  intent(in) :: rhs !< Right hand side.
  logical                             :: opr !< Operator result.

  opr = lhs%density /= rhs%density
  if (.not.opr) opr = lhs%velocity /= rhs%velocity
  if (.not.opr) opr = lhs%pressure /= rhs%pressure
  endfunction not_eq
endmodule flow_primitive_object
