

module U_finfot where
import UgenUtil
import Util
data U_finfot = U_finfo U_stringId U_stringId 

rdU_finfot :: _Addr -> UgnM U_finfot
rdU_finfot t
  = ioToUgnM (_ccall_ tfinfot t) `thenUgn` \ tag@(I# _) ->
    if tag == ``finfo'' then
	ioToUgnM (_ccall_ fi1 t) `thenUgn` \ x_fi1 ->
	rdU_stringId x_fi1 `thenUgn` \ y_fi1 ->
	ioToUgnM (_ccall_ fi2 t) `thenUgn` \ x_fi2 ->
	rdU_stringId x_fi2 `thenUgn` \ y_fi2 ->
	returnUgn (U_finfo y_fi1 y_fi2)
    else
	error ("rdU_finfot: bad tag selection:"++show tag++"\n")
