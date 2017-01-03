!< FLOw, Fortran fLuid Object
module flow
use flow_field_object
use flow_field_object_scalar
use flow_field_object_vectorial
use flow_primitive_object
use penf

implicit none
private
public :: field_object
public :: field_object_scalar
public :: field_object_vectorial
public :: primitive_object
! expose PENF kinds
public :: I_P, I1P, I2P, I4P, I8P, R_P, R4P, R8P, R16P
endmodule flow
