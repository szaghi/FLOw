!< FLOw **field** abstract object.

module flow_field_object
!< FLOw **field** abstract object.

use penf, only : I_P, R_P

implicit none
private
public :: field_object

type, abstract :: field_object
   !< **Field** abstract object.
   contains
      ! deferred methods
      procedure(array_interface),            pass(self), deferred :: array        !< Return serialized array of field.
      procedure(description_interface),      pass(self), deferred :: description  !< Return pretty-printed object description.
      ! deferred operators
      procedure(assign_interface),           pass(lhs),  deferred :: assign_field !< Operator `=`.
      procedure(assign_real_interface),      pass(lhs),  deferred :: assign_real  !< Operator `field = real`.
      procedure(unary_operator),             pass(self), deferred :: positive     !< Unary operator `+ field`.
      procedure(symmetric_op_interface),     pass(lhs),  deferred :: add          !< Operator `+`.
      procedure(symmetric_op_interface),     pass(lhs),  deferred :: div          !< Operator `/`.
      procedure(field_op_integer_interface), pass(lhs),  deferred :: div_integer  !< Operator `field / integer`.
      procedure(field_op_real_interface),    pass(lhs),  deferred :: div_real     !< Operator `field / real`.
      procedure(symmetric_op_interface),     pass(lhs),  deferred :: mul          !< Operator `*`.
      procedure(field_op_integer_interface), pass(lhs),  deferred :: mul_integer  !< Operator `field * integer`.
      procedure(integer_op_field_interface), pass(rhs),  deferred :: integer_mul  !< Operator `integer * field`.
      procedure(field_op_real_interface),    pass(lhs),  deferred :: mul_real     !< Operator `field * real`.
      procedure(real_op_field_interface),    pass(rhs),  deferred :: real_mul     !< Operator `real * field`.
      procedure(unary_operator),             pass(self), deferred :: negative     !< Unary operator `- field`.
      procedure(symmetric_op_interface),     pass(lhs),  deferred :: sub          !< Operator `-`.
      procedure(field_op_integer_interface), pass(lhs),  deferred :: pow_integer  !< Operator `field ** integer`.
      procedure(field_op_real_interface),    pass(lhs),  deferred :: pow_real     !< Operator `field ** real`.
      procedure(compare_interface),          pass(lhs),  deferred :: eq           !< Operator `=='.
      procedure(compare_interface),          pass(lhs),  deferred :: not_eq       !< Operator `/='.
      ! public operators
      generic :: assignment(=) => assign_field, assign_real                       !< Assignment overloading.
      generic :: operator(+) => add, positive                                     !< Operator `+` overloading.
      generic :: operator(/) => div, div_integer, div_real                        !< Operator `/` overloading.
      generic :: operator(*) => mul, mul_integer, integer_mul, real_mul, mul_real !< Operator `*` overloading.
      generic :: operator(-) => sub, negative                                     !< Operator `-` overloading.
      generic :: operator(**) => pow_integer, pow_real                            !< Operator `**` overloading.
      generic :: operator(==) => eq                                               !< Operator `/=` overloading.
      generic :: operator(/=) => not_eq                                           !< Operator `/=` overloading.
endtype field_object

abstract interface
   !< Abstract interfaces of deferred methods of [[field_object]].
   pure function array_interface(self) result(array_)
   !< Return serialized array of field.
   import :: field_object, R_P
   class(field_object), intent(in) :: self      !< Field.
   real(R_P), allocatable          :: array_(:) !< Serialized array of field.
   endfunction array_interface

   pure function description_interface(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   import :: field_object
   class(field_object), intent(in)           :: self   !< Field.
   character(*),        intent(in), optional :: prefix !< Prefixing string.
   character(len=:), allocatable             :: desc   !< Description.
   endfunction description_interface

   elemental subroutine assign_interface(lhs, rhs)
   !< Operator `=`.
   import :: field_object
   class(field_object), intent(inout) :: lhs !< Left hand side.
   class(field_object), intent(in)    :: rhs !< Right hand side.
   endsubroutine assign_interface

   elemental subroutine assign_real_interface(lhs, rhs)
   !< Operator `field = real`.
   import :: field_object, R_P
   class(field_object), intent(inout) :: lhs !< Left hand side.
   real(R_P),           intent(in)    :: rhs !< Right hand side.
   endsubroutine assign_real_interface

   function unary_operator(self) result(opr)
   !< Unary operator `.op.field`.
   import :: field_object
   class(field_object), intent(in)  :: self !< Field.
   class(field_object), allocatable :: opr  !< Operator result.
   endfunction unary_operator

   elemental function symmetric_op_interface(lhs, rhs) result(opr)
   !< Operator `field.op.field`.
   import :: field_object
   class(field_object), intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.
   endfunction symmetric_op_interface

   elemental function integer_op_field_interface(lhs, rhs) result(opr)
   !< Operator `field.op.integer`.
   import :: field_object, I_P
   integer(I_P),        intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.
   endfunction integer_op_field_interface

   elemental function field_op_integer_interface(lhs, rhs) result(opr)
   !< Operator `field.op.integer`.
   import :: field_object, I_P
   class(field_object), intent(in)  :: lhs !< Left hand side.
   integer(I_P),        intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.
   endfunction field_op_integer_interface

   elemental function field_op_real_interface(lhs, rhs) result(opr)
   !< Operator `field.op.real`.
   import :: field_object, R_P
   class(field_object), intent(in)  :: lhs !< Left hand side.
   real(R_P),           intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.
   endfunction field_op_real_interface

   elemental function real_op_field_interface(lhs, rhs) result(opr)
   !< Operator `real.op.field`.
   import :: field_object, R_P
   real(R_P),           intent(in)  :: lhs !< Left hand side.
   class(field_object), intent(in)  :: rhs !< Right hand side.
   class(field_object), allocatable :: opr !< Operator result.
   endfunction real_op_field_interface

   elemental function compare_interface(lhs, rhs) result(opr)
   !< Operator `field.compare.field'.
   import :: field_object
   class(field_object), intent(in) :: lhs !< Left hand side.
   class(field_object), intent(in) :: rhs !< Right hand side.
   logical                         :: opr !< Operator result.
   endfunction compare_interface
endinterface
endmodule flow_field_object
