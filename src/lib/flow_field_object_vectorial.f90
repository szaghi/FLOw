!< FLOw **vectorial field** object.
module flow_field_object_vectorial
!< FLOw **vectorial field** object.
use flow_field_object
use penf
use vecfor

implicit none
private
public :: field_object_vectorial

type, extends(field_object) :: field_object_vectorial
  !< **vectorial field** object.
  type(vector) :: field !< Vectorial field.
  contains
    ! deferred methods
    procedure, pass(lhs) :: assign_field !< Assign fields.
    procedure, pass(lhs) :: assign_real  !< Assign real to field.
    procedure, pass(lhs) :: add          !< Add fields.
    procedure, pass(lhs) :: div          !< Divide fields.
    procedure, pass(lhs) :: divreal      !< Divide field by real.
    procedure, pass(lhs) :: mul          !< Multiply fields.
    procedure, pass(lhs) :: mulreal      !< Multiply field for real.
    procedure, pass(rhs) :: realmul      !< Multiply real for field.
    procedure, pass(lhs) :: sub          !< Subtract fields.
    procedure, pass(lhs) :: not_eq       !< Compare (`/=') fields.
    procedure, pass(lhs) :: eq           !< Compare (`==') fields.
endtype field_object_vectorial

contains
  ! deferred public methods
  subroutine assign_field(lhs, rhs)
  !< Assign fields.
  class(field_object_vectorial), intent(inout) :: lhs !< Left hand side.
  class(field_object),           intent(in)    :: rhs !< Right hand side.

  select type(rhs)
  class is(field_object_vectorial)
    lhs%field = rhs%field
  endselect
  endsubroutine assign_field

  subroutine assign_real(lhs, rhs)
  !< Assign real to field.
  class(field_object_vectorial), intent(inout) :: lhs !< Left hand side.
  real(R_P),                     intent(in)    :: rhs !< Right hand side.

  lhs%field = rhs
  endsubroutine assign_real

  function add(lhs, rhs) result(opr)
  !< Add fields.
  class(field_object_vectorial), intent(in)  :: lhs !< Left hand side.
  class(field_object),           intent(in)  :: rhs !< Right hand side.
  class(field_object), allocatable           :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    select type(rhs)
    class is(field_object_vectorial)
      opr%field = lhs%field + rhs%field
    endselect
  endselect
  endfunction add

  function div(lhs, rhs) result(opr)
  !< Divide fields.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  class(field_object),           intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    select type(rhs)
    class is(field_object_vectorial)
      opr%field = lhs%field / rhs%field
    endselect
  endselect
  endfunction div

  function divreal(lhs, rhs) result(opr)
  !< Divide field by real.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  real(R_P),                     intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    opr%field = lhs%field / rhs
  endselect
  endfunction divreal

  function mul(lhs, rhs) result(opr)
  !< Multiply fields.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  class(field_object),           intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    select type(rhs)
    class is(field_object_vectorial)
      opr%field = lhs%field * rhs%field
    endselect
  endselect
  endfunction mul

  function sub(lhs, rhs) result(opr)
  !< Subtract fields.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  class(field_object),           intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    select type(rhs)
    class is(field_object_vectorial)
      opr%field = lhs%field - rhs%field
    endselect
  endselect
  endfunction sub

  function mulreal(lhs, rhs) result(opr)
  !< Multiply field for real.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  real(R_P),                     intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    opr%field = lhs%field * rhs
  endselect
  endfunction mulreal

  function realmul(lhs, rhs) result(opr)
  !< Multiply real for field.
  real(R_P),                     intent(in) :: lhs !< Left hand side.
  class(field_object_vectorial), intent(in) :: rhs !< Right hand side.
  class(field_object), allocatable          :: opr !< Operator result.

  allocate(field_object_vectorial :: opr)
  select type(opr)
  class is(field_object_vectorial)
    opr%field = lhs * rhs%field
  endselect
  endfunction realmul

  function not_eq(lhs, rhs) result(opr)
  !< Compare (`/=`) fields.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  class(field_object),           intent(in) :: rhs !< Right hand side.
  logical                                   :: opr !< Operator result.

  opr = .false.
  select type(rhs)
  class is(field_object_vectorial)
    opr = lhs%field /= rhs%field
  endselect
  endfunction not_eq

  function eq(lhs, rhs) result(opr)
  !< Compare (`==`) fields.
  class(field_object_vectorial), intent(in) :: lhs !< Left hand side.
  class(field_object),           intent(in) :: rhs !< Right hand side.
  logical                                   :: opr !< Operator result.

  opr = .false.
  select type(rhs)
  class is(field_object_vectorial)
    opr = lhs%field == rhs%field
  endselect
  endfunction eq
endmodule flow_field_object_vectorial
