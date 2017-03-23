!< FLOw **primitive** abstract object.

module flow_primitive_object
!< FLOw **primitive** abstract object.
!<
!< [[primitive_object]] is a class that handles primitive fluid dynamic variables.

use flow_field_object, only : field_object

implicit none
private
public :: primitive_object

type, extends(field_object), abstract :: primitive_object
  !< **Primitive** object.
endtype primitive_object
endmodule flow_primitive_object
