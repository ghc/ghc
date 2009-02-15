

module ArchReg (

)

where


class ArchReg reg format where
	classOfReg	:: reg    -> RegClass
	mkVReg		:: format -> VirtReg reg
	
	
