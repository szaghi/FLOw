!< FLOw, Fortran fLuid Object

module flow
!< FLOw, Fortran fLuid Object

use flow_field_object, only : field_object
use flow_field_scalar_vectorial, only : field_scalar, field_vectorial
use flow_primitive_object, only : primitive_object
use penf, only : I_P, I1P, I2P, I4P, I8P, R_P, R4P, R8P, R16P
use vecfor, only : vector

implicit none
private
public :: field_object
public :: field_scalar
public :: field_vectorial
public :: primitive_object
public :: I_P, I1P, I2P, I4P, I8P, R_P, R4P, R8P, R16P
public :: vector
endmodule flow
