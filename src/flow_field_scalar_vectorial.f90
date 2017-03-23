!< FLOw **field** concrete scalar and vectorial objects.

module flow_field_scalar_vectorial
!< FLOw **field** concrete scalar and vectorial objects.

use flow_field_object, only : field_object
use penf, only : I4P, R8P, str
use vecfor, only : vector

implicit none
private
public :: field_scalar
public :: field_vectorial

type, extends(field_object) :: field_scalar
   !< **Scalar field** object.
   real(R8P) :: field !< Scalar field.
   contains
      ! deferred methods
      procedure, pass(self) :: array       => array_scalar        !< Return serialized array of field.
      procedure, pass(self) :: description => description_scalar  !< Return pretty-printed object description.
      ! deferred operators
      procedure, pass(lhs)  :: assign_field => assign_field_scalar !< Operator `=`.
      procedure, pass(lhs)  :: assign_real  => assign_real_scalar  !< Operator `field = real`.
      procedure, pass(self) :: positive     => positive_scalar     !< Unary operator `+ field`.
      procedure, pass(lhs)  :: add          => scalar_add_scalar   !< Operator `+`.
      procedure, pass(lhs)  :: div          => scalar_div_scalar   !< Operator `/`.
      procedure, pass(lhs)  :: div_integer  => scalar_div_integer  !< Operator `field / integer`.
      procedure, pass(lhs)  :: div_real     => scalar_div_real     !< Operator `field / real`.
      procedure, pass(lhs)  :: mul          => scalar_mul_object   !< Operator `*`.
      procedure, pass(lhs)  :: mul_integer  => scalar_mul_integer  !< Operator `field * integer`.
      procedure, pass(rhs)  :: integer_mul  => integer_mul_scalar  !< Operator `integer * field`.
      procedure, pass(lhs)  :: mul_real     => scalar_mul_real     !< Operator `field * real`.
      procedure, pass(rhs)  :: real_mul     => real_mul_scalar     !< Operator `real * field`.
      procedure, pass(self) :: negative     => negative_scalar     !< Unary operator `- field`.
      procedure, pass(lhs)  :: sub          => scalar_sub_scalar   !< Operator `-`.
      procedure, pass(lhs)  :: pow_integer  => scalar_pow_integer  !< Operator `field ** integer`.
      procedure, pass(lhs)  :: pow_real     => scalar_pow_real     !< Operator `field ** real`.
      procedure, pass(lhs)  :: eq           => eq_scalar           !< Operator `=='.
      procedure, pass(lhs)  :: not_eq       => not_eq_scalar       !< Operator `/='.
endtype field_scalar

type, extends(field_object) :: field_vectorial
   !< **Vectorial field** object.
   type(vector) :: field !< Vectorial field.
   contains
      ! deferred methods
      procedure, pass(self) :: array       => array_vectorial         !< Return serialized array of field.
      procedure, pass(self) :: description => description_vectorial   !< Return pretty-printed object description.
      ! deferred operators
      procedure, pass(lhs)  :: assign_field => assign_field_vectorial  !< Operator `=`.
      procedure, pass(lhs)  :: assign_real  => assign_real_vectorial   !< Operator `field = real`.
      procedure, pass(self) :: positive     => positive_vectorial      !< Unary operator `+ field`.
      procedure, pass(lhs)  :: add          => vectorial_add_vectorial !< Operator `+`.
      procedure, pass(lhs)  :: div          => vectorial_div_object    !< Operator `/`.
      procedure, pass(lhs)  :: div_integer  => vectorial_div_integer   !< Operator `field / integer`.
      procedure, pass(lhs)  :: div_real     => vectorial_div_real      !< Operator `field / real`.
      procedure, pass(lhs)  :: mul          => vectorial_mul_object    !< Operator `*`.
      procedure, pass(lhs)  :: mul_integer  => vectorial_mul_integer   !< Operator `field * integer`.
      procedure, pass(rhs)  :: integer_mul  => integer_mul_vectorial   !< Operator `integer * field`.
      procedure, pass(lhs)  :: mul_real     => vectorial_mul_real      !< Operator `field * real`.
      procedure, pass(rhs)  :: real_mul     => real_mul_vectorial      !< Operator `real * field`.
      procedure, pass(self) :: negative     => negative_vectorial      !< Unary operator `- field`.
      procedure, pass(lhs)  :: sub          => vectorial_sub_vectorial !< Operator `-`.
      procedure, pass(lhs)  :: pow_integer  => vectorial_pow_integer   !< Operator `field ** integer`.
      procedure, pass(lhs)  :: pow_real     => vectorial_pow_real      !< Operator `field ** real`.
      procedure, pass(lhs)  :: eq           => eq_vectorial            !< Operator `=='.
      procedure, pass(lhs)  :: not_eq       => not_eq_vectorial        !< Operator `/='.
endtype field_vectorial

contains
   ! scalar field
   ! deferred methods
   pure function array_scalar(self) result(array_)
   !< Return serialized array of field.
   class(field_scalar), intent(in) :: self      !< Field.
   real(R8P), allocatable          :: array_(:) !< Serialized array of field.

   allocate(array_(1:1))
   array_(1) = self%field
   endfunction array_scalar

   pure function description_scalar(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(field_scalar), intent(in)           :: self             !< Primitive.
   character(*),        intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable             :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable             :: desc             !< Description.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = prefix_//'field  = '//trim(str(n=self%field))
   endfunction description_scalar

   ! deferred oprators
   elemental subroutine assign_field_scalar(lhs, rhs)
   !< Operator `=`.
   class(field_scalar), intent(inout) :: lhs !< Left hand side.
   class(field_object), intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(field_scalar)
      lhs%field = rhs%field
   endselect
   endsubroutine assign_field_scalar

   elemental subroutine assign_real_scalar(lhs, rhs)
   !< Operator `field = real`.
   class(field_scalar), intent(inout) :: lhs !< Left hand side.
   real(R8P),           intent(in)    :: rhs !< Right hand side.

   lhs%field = rhs
   endsubroutine assign_real_scalar

   function positive_scalar(self) result(opr)
   !< Unary operator `+ field`.
   class(field_scalar), intent(in)  :: self !< Primitive.
   class(field_object), allocatable :: opr  !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = + self%field
   endselect
   endfunction positive_scalar

   elemental function scalar_add_scalar(lhs, rhs) result(opr)
   !< Operator `+`.
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

   elemental function scalar_div_scalar(lhs, rhs) result(opr)
   !< Operator `/`.
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

   elemental function scalar_div_integer(lhs, rhs) result(opr)
   !< Operator `field / integer`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field / rhs
   endselect
   endfunction scalar_div_integer

   elemental function scalar_div_real(lhs, rhs) result(opr)
   !< Operator `field / real`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field / rhs
   endselect
   endfunction scalar_div_real

   elemental function scalar_mul_object(lhs, rhs) result(opr)
   !< Operator `*`.
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

   elemental function scalar_mul_integer(lhs, rhs) result(opr)
   !< Operator `field * integer`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field * rhs
   endselect
   endfunction scalar_mul_integer

   elemental function integer_mul_scalar(lhs, rhs) result(opr)
   !< Operator `integer * field`.
   integer(I4P),        intent(in)  :: lhs !< Left hand side.
   class(field_scalar), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs * rhs%field
   endselect
   endfunction integer_mul_scalar

   elemental function scalar_mul_real(lhs, rhs) result(opr)
   !< Operator `field * real`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field * rhs
   endselect
   endfunction scalar_mul_real

   elemental function real_mul_scalar(lhs, rhs) result(opr)
   !< Operator `real * field`.
   real(R8P),           intent(in)  :: lhs !< Left hand side.
   class(field_scalar), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs * rhs%field
   endselect
   endfunction real_mul_scalar

   function negative_scalar(self) result(opr)
   !< Unary operator `- field`.
   class(field_scalar), intent(in)  :: self !< Primitive.
   class(field_object), allocatable :: opr  !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = - self%field
   endselect
   endfunction negative_scalar

   elemental function scalar_sub_scalar(lhs, rhs) result(opr)
   !< Operator `-`.
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

   elemental function scalar_pow_integer(lhs, rhs) result(opr)
   !< Operator `field ** integer`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   integer(I4P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field ** rhs
   endselect
   endfunction scalar_pow_integer

   elemental function scalar_pow_real(lhs, rhs) result(opr)
   !< Operator `field ** real`.
   class(field_scalar), intent(in)  :: lhs !< Left hand side.
   real(R8P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.

   allocate(field_scalar :: opr)
   select type(opr)
   class is(field_scalar)
      opr%field = lhs%field ** rhs
   endselect
   endfunction scalar_pow_real

   elemental function eq_scalar(lhs, rhs) result(opr)
   !< Operator `=='.
   class(field_scalar), intent(in) :: lhs !< Left hand side.
   class(field_object), intent(in) :: rhs !< Right hand side.
   logical                         :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_scalar)
      opr = lhs%field == rhs%field
   endselect
   endfunction eq_scalar

   elemental function not_eq_scalar(lhs, rhs) result(opr)
   !< Operator `/='.
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
   ! deferred methods
   pure function array_vectorial(self) result(array_)
   !< Return serialized array of field.
   class(field_vectorial), intent(in) :: self      !< Field.
   real(R8P), allocatable             :: array_(:) !< Serialized array of field.

   allocate(array_(1:3))
   array_(1) = self%field%x
   array_(2) = self%field%y
   array_(3) = self%field%z
   endfunction array_vectorial

   pure function description_vectorial(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(field_vectorial), intent(in)           :: self             !< Primitive.
   character(*),           intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                :: desc             !< Description.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = prefix_//'field  = '//trim(str(n=[self%field%x, self%field%y, self%field%z]))
   endfunction description_vectorial

   ! deferred oprators
   elemental subroutine assign_field_vectorial(lhs, rhs)
   !< Operator `=`.
   class(field_vectorial), intent(inout) :: lhs !< Left hand side.
   class(field_object),    intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(field_vectorial)
      lhs%field = rhs%field
   endselect
   endsubroutine assign_field_vectorial

   elemental subroutine assign_real_vectorial(lhs, rhs)
   !< Operator `field = real`.
   class(field_vectorial), intent(inout) :: lhs !< Left hand side.
   real(R8P),              intent(in)    :: rhs !< Right hand side.

   lhs%field = rhs
   endsubroutine assign_real_vectorial

   function positive_vectorial(self) result(opr)
   !< Unary operator `+ field`.
   class(field_vectorial), intent(in) :: self !< Primitive.
   class(field_object), allocatable   :: opr  !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = + self%field
   endselect
   endfunction positive_vectorial

   elemental function vectorial_add_vectorial(lhs, rhs) result(opr)
   !< Operator `+`.
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

   elemental function vectorial_div_object(lhs, rhs) result(opr)
   !< Operator `/`.
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

   elemental function vectorial_div_integer(lhs, rhs) result(opr)
   !< Operator `field / integer`.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   integer(I4P),           intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field / rhs
   endselect
   endfunction vectorial_div_integer

   elemental function vectorial_div_real(lhs, rhs) result(opr)
   !< Operator `field / real`.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   real(R8P),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field / rhs
   endselect
   endfunction vectorial_div_real

   elemental function vectorial_mul_object(lhs, rhs) result(opr)
   !< Operator `*`.
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

   elemental function vectorial_mul_integer(lhs, rhs) result(opr)
   !< Operator `field * integer`.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   integer(I4P),           intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field * rhs
   endselect
   endfunction vectorial_mul_integer

   elemental function integer_mul_vectorial(lhs, rhs) result(opr)
   !< Operator `integer * field`.
   integer(I4P),           intent(in) :: lhs !< Left hand side.
   class(field_vectorial), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs * rhs%field
   endselect
   endfunction integer_mul_vectorial

   elemental function vectorial_mul_real(lhs, rhs) result(opr)
   !< Operator `field * real`.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   real(R8P),              intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs%field * rhs
   endselect
   endfunction vectorial_mul_real

   elemental function real_mul_vectorial(lhs, rhs) result(opr)
   !< Operator `real * field`.
   real(R8P),              intent(in) :: lhs !< Left hand side.
   class(field_vectorial), intent(in) :: rhs !< Right hand side.
   class(field_object), allocatable   :: opr !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = lhs * rhs%field
   endselect
   endfunction real_mul_vectorial

   function negative_vectorial(self) result(opr)
   !< Unary operator `+ field`.
   class(field_vectorial), intent(in) :: self !< Primitive.
   class(field_object), allocatable   :: opr  !< Operator result.

   allocate(field_vectorial :: opr)
   select type(opr)
   class is(field_vectorial)
      opr%field = - self%field
   endselect
   endfunction negative_vectorial

   elemental function vectorial_sub_vectorial(lhs, rhs) result(opr)
   !< Operator `-`.
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

   elemental function vectorial_pow_integer(lhs, rhs) result(opr)
   !< Operator `field ** integer`.
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

   elemental function vectorial_pow_real(lhs, rhs) result(opr)
   !< Operator `field ** real`.
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

   elemental function eq_vectorial(lhs, rhs) result(opr)
   !< Operator `=='.
   class(field_vectorial), intent(in) :: lhs !< Left hand side.
   class(field_object),    intent(in) :: rhs !< Right hand side.
   logical                            :: opr !< Operator result.

   opr = .false.
   select type(rhs)
   class is(field_vectorial)
      opr = lhs%field == rhs%field
   endselect
   endfunction eq_vectorial

   elemental function not_eq_vectorial(lhs, rhs) result(opr)
   !< Operator `/='.
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
