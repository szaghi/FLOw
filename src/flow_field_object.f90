!< FLOw **field** (abstract) object.
module flow_field_object
!< FLOw **field** (abstract) object.
use penf

implicit none
private
public :: field_object

type, abstract :: field_object
  !< **field** (abstract) object.
  contains
    ! deferred operators methods
    procedure(abstract_assign),             pass(lhs), deferred :: assign_field !< Assign fields.
    procedure(abstract_assign_real),        pass(lhs), deferred :: assign_real  !< Assign real to field.
    procedure(abstract_simmetric_operator), pass(lhs), deferred :: add          !< Add fields.
    procedure(abstract_simmetric_operator), pass(lhs), deferred :: div          !< Divide fields.
    procedure(abstract_field_op_integer),   pass(lhs), deferred :: div_integer  !< Divide field by integer.
    procedure(abstract_field_op_real),      pass(lhs), deferred :: div_real     !< Divide field by real.
    procedure(abstract_simmetric_operator), pass(lhs), deferred :: mul          !< Multiply fields.
    procedure(abstract_field_op_integer),   pass(lhs), deferred :: mul_integer  !< Multiply field for integer.
    procedure(abstract_integer_op_field),   pass(rhs), deferred :: integer_mul  !< Multiply integer for field.
    procedure(abstract_field_op_real),      pass(lhs), deferred :: mul_real     !< Multiply field for real.
    procedure(abstract_real_op_field),      pass(rhs), deferred :: real_mul     !< Multiply real for field.
    procedure(abstract_simmetric_operator), pass(lhs), deferred :: sub          !< Subtract fields.
    procedure(abstract_field_op_integer),   pass(lhs), deferred :: pow_integer  !< Power field by integer.
    procedure(abstract_field_op_real),      pass(lhs), deferred :: pow_real     !< Power field by real.
    procedure(abstract_compare),            pass(lhs), deferred :: eq           !< Compare (`==') fields.
    procedure(abstract_compare),            pass(lhs), deferred :: not_eq       !< Compare (`/=') fields.
    ! public operators
    generic :: assignment(=) => assign_field, assign_real                       !< Assignment overloading.
    generic :: operator(+) => add                                               !< Operator `+` overloading.
    generic :: operator(/) => div, div_integer, div_real                        !< Operator `/` overloading.
    generic :: operator(*) => mul, mul_integer, integer_mul, real_mul, mul_real !< Operator `*` overloading.
    generic :: operator(-) => sub                                               !< Operator `-` overloading.
    generic :: operator(**) => pow_integer, pow_real                            !< Operator `**` overloading.
    generic :: operator(==) => eq                                               !< Operator `/=` overloading.
    generic :: operator(/=) => not_eq                                           !< Operator `/=` overloading.
endtype field_object

! deferred public methods interfaces
abstract interface
  !< Assignment overloading.
  pure subroutine abstract_assign(lhs, rhs)
  !< Assign fields.
  import :: field_object
  class(field_object), intent(inout) :: lhs !< Left hand side.
  class(field_object), intent(in)    :: rhs !< Right hand side.
  endsubroutine abstract_assign

  pure subroutine abstract_assign_real(lhs, rhs)
  !< Assign real to field.
  import :: field_object, R_P
  class(field_object), intent(inout) :: lhs !< Left hand side.
  real(R_P),           intent(in)    :: rhs !< Right hand side.
  endsubroutine abstract_assign_real
endinterface

abstract interface
  !< Symmetric operator field.op.field.
  pure function abstract_simmetric_operator(lhs, rhs) result(opr)
  !< Symmetric operator field.op.field.
  import :: field_object
  class(field_object), intent(in)  :: lhs !< Left hand side.
  class(field_object), intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable :: opr !< Operator result.
  endfunction abstract_simmetric_operator
endinterface

abstract interface
  !< Non symmetric operator integer.op.field.
  pure function abstract_integer_op_field(lhs, rhs) result(opr)
  !< Non symmetric operator integer.op.field.
  import :: field_object, I_P
  integer(I_P),        intent(in)  :: lhs !< Left hand side.
  class(field_object), intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable :: opr !< Operator result.
  endfunction abstract_integer_op_field
endinterface

abstract interface
  !< Non symmetric operator field.op.integer.
  pure function abstract_field_op_integer(lhs, rhs) result(opr)
  !< Non symmetric operator field.op.integer.
  import :: field_object, I_P
  class(field_object), intent(in)  :: lhs !< Left hand side.
  integer(I_P),        intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable :: opr !< Operator result.
  endfunction abstract_field_op_integer
endinterface

abstract interface
  !< Non symmetric operator field.op.real.
  pure function abstract_field_op_real(lhs, rhs) result(opr)
  !< Non symmetric operator field.op.real.
  import :: field_object, R_P
  class(field_object), intent(in)  :: lhs !< Left hand side.
  real(R_P),           intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable :: opr !< Operator result.
  endfunction abstract_field_op_real
endinterface

abstract interface
  !< Non symmetric operator real.op.field.
  pure function abstract_real_op_field(lhs, rhs) result(opr)
  !< Non symmetric operator real.op.field.
  import :: field_object, R_P
  real(R_P),           intent(in)  :: lhs !< Left hand side.
  class(field_object), intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable :: opr !< Operator result.
  endfunction abstract_real_op_field
endinterface

abstract interface
  !< Compare overloading.
  pure function abstract_compare(lhs, rhs) result(opr)
  !< Compare overloading.
  import :: field_object
  class(field_object), intent(in) :: lhs !< Left hand side.
  class(field_object), intent(in) :: rhs !< Right hand side.
  logical                         :: opr !< Operator result.
  endfunction abstract_compare
endinterface
endmodule flow_field_object
