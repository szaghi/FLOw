!< FLOw, Fortran fLuid Object

module flow
!< FLOw, Fortran fLuid Object

use flow_eos_compressible, only : eos_compressible
use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_field_scalar_vectorial, only : field_scalar, field_vectorial
use flow_primitive_compressible, only : primitive_compressible
use flow_primitive_compressible_multispecie, only : primitive_compressible_multispecie
use flow_primitive_object, only : primitive_object

implicit none
private
public :: eos_compressible
public :: eos_object
public :: field_object
public :: field_scalar
public :: field_vectorial
public :: primitive_compressible
public :: primitive_compressible_multispecie
public :: primitive_object
endmodule flow
