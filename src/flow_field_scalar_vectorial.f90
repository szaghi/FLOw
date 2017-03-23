!< FLOw **field** scalar and vectorial concrete objects.

module flow_field_scalar_vectorial
!< FLOw **scalar field** scalar and vectorial object.

use flow_field_object, only : field_object
use penf, only : I4P, R8P
use vecfor, only : vector

implicit none
private
public :: field_scalar
public :: field_vectorial

type, extends(field_object) :: field_scalar
   !< **scalar field** object.
   real(R8P) :: field !< Scalar field.
   contains
      ! deferred methods
      procedure, pass(lhs) :: assign_field => assign_field_scalar !< Assign fields.
      procedure, pass(lhs) :: assign_real  => assign_real_scalar  !< Assign real to field.
      procedure, pass(lhs) :: add          => scalar_add_scalar   !< Add fields.
      procedure, pass(lhs) :: div          => scalar_div_scalar   !< Divide fields.
      procedure, pass(lhs) :: div_integer  => scalar_div_integer  !< Divide field by integer.
      procedure, pass(lhs) :: div_real     => scalar_div_real     !< Divide field by real.
      procedure, pass(lhs) :: mul          => scalar_mul_object   !< Multiply fields.
      procedure, pass(lhs) :: mul_integer  => scalar_mul_integer  !< Multiply field for integer.
      procedure, pass(rhs) :: integer_mul  => integer_mul_scalar  !< Multiply scalar or field.
      procedure, pass(lhs) :: mul_real     => scalar_mul_real     !< Multiply field for real.
      procedure, pass(rhs) :: real_mul     => real_mul_scalar     !< Multiply real for field.
      procedure, pass(lhs) :: sub          => scalar_sub_scalar   !< Subtract fields.
      procedure, pass(lhs) :: pow_integer  => scalar_pow_integer  !< Power field by integer.
      procedure, pass(lhs) :: pow_real     => scalar_pow_real     !< Power field by real.
      procedure, pass(lhs) :: eq           => eq_scalar           !< Compare (`==') fields.
      procedure, pass(lhs) :: not_eq       => not_eq_scalar       !< Compare (`/=') fields.
endtype field_scalar

type, extends(field_object) :: field_vectorial
   !< **vectorial field** object.
   type(vector) :: field !< Vectorial field.
   contains
      ! deferred methods
      procedure, pass(lhs) :: assign_field => assign_field_vectorial  !< Assign fields.
      procedure, pass(lhs) :: assign_real  => assign_real_vectorial   !< Assign real to field.
      procedure, pass(lhs) :: add          => vectorial_add_vectorial !< Add fields.
      procedure, pass(lhs) :: div          => vectorial_div_object    !< Divide fields.
      procedure, pass(lhs) :: div_integer  => vectorial_div_integer   !< Divide field by integer.
      procedure, pass(lhs) :: div_real     => vectorial_div_real      !< Divide field by real.
      procedure, pass(lhs) :: mul          => vectorial_mul_object    !< Multiply fields.
      procedure, pass(lhs) :: mul_integer  => vectorial_mul_integer   !< Multiply field for integer.
      procedure, pass(rhs) :: integer_mul  => integer_mul_vectorial   !< Multiply integer for field.
      procedure, pass(lhs) :: mul_real     => vectorial_mul_real      !< Multiply field for real.
      procedure, pass(rhs) :: real_mul     => real_mul_vectorial      !< Multiply real for field.
      procedure, pass(lhs) :: sub          => vectorial_sub_vectorial !< Subtract fields.
      procedure, pass(lhs) :: pow_integer  => vectorial_pow_integer   !< Power field by integer.
      procedure, pass(lhs) :: pow_real     => vectorial_pow_real      !< Power field by real.
      procedure, pass(lhs) :: eq           => eq_vectorial            !< Compare (`==') fields.
      procedure, pass(lhs) :: not_eq       => not_eq_vectorial        !< Compare (`/=') fields.
endtype field_vectorial

contains
   ! deferred public methods

   ! scalar field
   pure subroutine assign_field_scalar(lhs, rhs)
   !< Assign fields.
   class(field_scalar), intent(inout) :: lhs !< Left hand side.
   class(field_object), intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(field_scalar)
      lhs%field = rhs%field
   endselect
   endsubroutine assign_field_scalar

   pure subroutine assign_real_scalar(lhs, rhs)
   !< Assign real to field.
   class(field_scalar), intent(inout) :: lhs !< Left hand side.
   real(R8P),           intent(in)    :: rhs !< Right hand side.

   lhs%field = rhs
   endsubroutine assign_real_scalar

   pure function scalar_add_scalar(lhs, rhs) result(opr)
   !< Add fields.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      select type(rhs)
      class is(field_scalar)
         opr%field = lhs%field + rhs%field
      endselect
   endselect
   endfunction scalar_add_scalar

   pure function scalar_div_scalar(lhs, rhs) result(opr)
   !< Divide fields.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      select type(rhs)
      class is(field_scalar)
         opr%field = lhs%field / rhs%field
      endselect
   endselect
   endfunction scalar_div_scalar

   pure function scalar_div_integer(lhs, rhs) result(opr)
   !< Divide field by integer.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field / rhs
   endselect
   endfunction scalar_div_integer

   pure function scalar_div_real(lhs, rhs) result(opr)
   !< Divide field by real.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field / rhs
   endselect
   endfunction scalar_div_real

   pure function scalar_mul_object(lhs, rhs) result(opr)
   !< Multiply fields.
   !<
   !< @note The combinations accepted are:
   !<+ `field_scalar * field_vectorial => field_vectorial`
   !<+ `field_scalar * field_scalar    => field_scalar`
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   select type(rhs)
   type is(field_vectorial)
      allocate(field_vectorial :: opr)
      select type(opr)
      class is(field_vectorial)
         opr%field = lhs%field * rhs%field
      endselect
   class is(field_scalar)
      allocate(field_scalar :: opr)
      select type(opr)
      class is(field_scalar)
         opr%field = lhs%field * rhs%field
      endselect
   endselect
   endfunction scalar_mul_object

   pure function scalar_mul_integer(lhs, rhs) result(opr)
   !< Multiply field for integer.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field * rhs
   endselect
   endfunction scalar_mul_integer

   pure function integer_mul_scalar(lhs, rhs) result(opr)
   !< Multiply integer for field.
   integer(I4P),        intent(in)  :: lhs !< Left hand side.
   class(field_scalar), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs * rhs%field
   endselect
   endfunction integer_mul_scalar

   pure function scalar_mul_real(lhs, rhs) result(opr)
   !< Multiply field for real.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field * rhs
   endselect
   endfunction scalar_mul_real

   pure function real_mul_scalar(lhs, rhs) result(opr)
   !< Multiply real for field.
   real(R8P),           intent(in)  :: lhs !< Left hand side.
   class(field_scalar), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs * rhs%field
   endselect
   endfunction real_mul_scalar

   pure function scalar_sub_scalar(lhs, rhs) result(opr)
   !< Subtract fields.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      select type(rhs)
      class is(field_scalar)
         opr%field = lhs%field - rhs%field
      endselect
   endselect
   endfunction scalar_sub_scalar

   pure function scalar_pow_integer(lhs, rhs) result(opr)
   !< Power field by integer.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field ** rhs
   endselect
   endfunction scalar_pow_integer

   pure function scalar_pow_real(lhs, rhs) result(opr)
   !< Power field by real.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field ** rhs
   endselect
   endfunction scalar_pow_real

   pure function eq_scalar(lhs, rhs) result(opr)
   !< Compare (`==`) fields.
   class(field_scalar), intent(in) :: lhs !< Left hand side.
   class(field_object), intent(in) :: rhs !< Right hand side.
   logical                         :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_scalar)
      opr = lhs%field == rhs%field
   endselect
   endfunction eq_scalar

   pure function not_eq_scalar(lhs, rhs) result(opr)
   !< Compare (`/=`) fields.
   class(field_scalar), intent(in) :: lhs !< Left hand side.
   class(field_object), intent(in) :: rhs !< Right hand side.
   logical                         :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_scalar)
      opr = lhs%field /= rhs%field
   endselect
   endfunction not_eq_scalar

   ! vectorial field
   pure subroutine assign_field_vectorial(lhs, rhs)
   !< Assign fields.
   class(field_vectorial), intent(inout) :: lhs !< Left hand side.
   class(field_object),    intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(field_vectorial)
      lhs%field = rhs%field
   endselect
   endsubroutine assign_field_vectorial

   pure subroutine assign_real_vectorial(lhs, rhs)
   !< Assign real to field.
   class(field_vectorial), intent(inout) :: lhs !< Left hand side.
   real(R8P),              intent(in)    :: rhs !< Right hand side.

   lhs%field = rhs
   endsubroutine assign_real_vectorial

   pure function vectorial_add_vectorial(lhs, rhs) result(opr)
   !< Add fields.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      select type(rhs)
      class is(field_vectorial)
         opr%field = lhs%field + rhs%field
      endselect
   endselect
   endfunction vectorial_add_vectorial

   pure function vectorial_div_object(lhs, rhs) result(opr)
   !< Divide fields.
   !<
   !< @note The combinations accepted are:
   !<+ `field_vectorial / field_vectorial => field_vectorial`
   !<+ `field_vectorial / field_scalar    => field_vectorial`
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      select type(rhs)
      class is(field_vectorial)
         opr%field = lhs%field / rhs%field
      class is(field_scalar)
         opr%field = lhs%field / rhs%field
      endselect
   endselect
   endfunction vectorial_div_object

   pure function vectorial_div_integer(lhs, rhs) result(opr)
   !< Divide field by integer.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   integer(I4P),           intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field / rhs
   endselect
   endfunction vectorial_div_integer

   pure function vectorial_div_real(lhs, rhs) result(opr)
   !< Divide field by real.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   real(R8P),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field / rhs
   endselect
   endfunction vectorial_div_real

   pure function vectorial_mul_object(lhs, rhs) result(opr)
   !< Multiply fields.
   !<
   !< @note The combinations accepted are:
   !<+ `field_vectorial * field_vectorial => field_vectorial`
   !<+ `field_vectorial * field_scalar    => field_vectorial`
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      select type(rhs)
      class is(field_vectorial)
         opr%field = lhs%field * rhs%field
      class is(field_scalar)
         opr%field = lhs%field * rhs%field
      endselect
   endselect
   endfunction vectorial_mul_object

   pure function vectorial_mul_integer(lhs, rhs) result(opr)
   !< Multiply field for integer.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   integer(I4P),           intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field * rhs
   endselect
   endfunction vectorial_mul_integer

   pure function integer_mul_vectorial(lhs, rhs) result(opr)
   !< Multiply integer for field.
   integer(I4P),           intent(in) :: lhs !< Left hand side.
   class(field_vectorial), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs * rhs%field
   endselect
   endfunction integer_mul_vectorial

   pure function vectorial_mul_real(lhs, rhs) result(opr)
   !< Multiply field for real.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   real(R8P),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field * rhs
   endselect
   endfunction vectorial_mul_real

   pure function real_mul_vectorial(lhs, rhs) result(opr)
   !< Multiply real for field.
   real(R8P),              intent(in) :: lhs !< Left hand side.
   class(field_vectorial), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs * rhs%field
   endselect
   endfunction real_mul_vectorial

   pure function vectorial_sub_vectorial(lhs, rhs) result(opr)
   !< Subtract fields.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      select type(rhs)
      class is(field_vectorial)
         opr%field = lhs%field - rhs%field
      endselect
   endselect
   endfunction vectorial_sub_vectorial

   pure function vectorial_pow_integer(lhs, rhs) result(opr)
   !< Power field by integer.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   integer(I4P),           intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field%x = lhs%field%x ** rhs
      opr%field%y = lhs%field%y ** rhs
      opr%field%z = lhs%field%z ** rhs
   endselect
   endfunction vectorial_pow_integer

   pure function vectorial_pow_real(lhs, rhs) result(opr)
   !< Power field by real.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   real(R8P),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field%x = lhs%field%x ** rhs
      opr%field%y = lhs%field%y ** rhs
      opr%field%z = lhs%field%z ** rhs
   endselect
   endfunction vectorial_pow_real

   pure function eq_vectorial(lhs, rhs) result(opr)
   !< Compare (`==`) fields.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   logical                            :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_vectorial)
      opr = lhs%field == rhs%field
   endselect
   endfunction eq_vectorial

   pure function not_eq_vectorial(lhs, rhs) result(opr)
   !< Compare (`/=`) fields.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   logical                            :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_vectorial)
      opr = lhs%field /= rhs%field
   endselect
   endfunction not_eq_vectorial
endmodule flow_field_scalar_vectorial