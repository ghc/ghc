

module U_list where
import UgenUtil
import Util
data U_list = U_lcons U_VOID_STAR U_list | U_lnil 

rdU_list :: _Addr -> UgnM U_list
rdU_list t
  = ioToUgnM (_ccall_ tlist t) `thenUgn` \ tag@(I# _) ->
    if tag == ``lcons'' then
	ioToUgnM (_ccall_ lhd t) `thenUgn` \ x_lhd ->
	rdU_VOID_STAR x_lhd `thenUgn` \ y_lhd ->
	ioToUgnM (_ccall_ ltl t) `thenUgn` \ x_ltl ->
	rdU_list x_ltl `thenUgn` \ y_ltl ->
	returnUgn (U_lcons y_lhd y_ltl)
    else if tag == ``lnil'' then
	returnUgn (U_lnil )
    else
	error ("rdU_list: bad tag selection:"++show tag++"\n")
