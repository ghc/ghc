

module U_atype where
import UgenUtil
import Util

import U_list
data U_atype = U_atc U_unkId U_list U_long 

rdU_atype :: _Addr -> UgnM U_atype
rdU_atype t
  = ioToUgnM (_ccall_ tatype t) `thenUgn` \ tag@(I# _) ->
    if tag == ``atc'' then
	ioToUgnM (_ccall_ gatcid t) `thenUgn` \ x_gatcid ->
	rdU_unkId x_gatcid `thenUgn` \ y_gatcid ->
	ioToUgnM (_ccall_ gatctypel t) `thenUgn` \ x_gatctypel ->
	rdU_list x_gatctypel `thenUgn` \ y_gatctypel ->
	ioToUgnM (_ccall_ gatcline t) `thenUgn` \ x_gatcline ->
	rdU_long x_gatcline `thenUgn` \ y_gatcline ->
	returnUgn (U_atc y_gatcid y_gatctypel y_gatcline)
    else
	error ("rdU_atype: bad tag selection:"++show tag++"\n")
