

module U_entidt where
import UgenUtil
import Util

import U_list
data U_entidt = U_entid U_stringId | U_enttype U_stringId | U_enttypeall U_stringId | U_enttypecons U_stringId U_list | U_entclass U_stringId U_list | U_entmod U_stringId 

rdU_entidt :: _Addr -> UgnM U_entidt
rdU_entidt t
  = ioToUgnM (_ccall_ tentidt t) `thenUgn` \ tag@(I# _) ->
    if tag == ``entid'' then
	ioToUgnM (_ccall_ gentid t) `thenUgn` \ x_gentid ->
	rdU_stringId x_gentid `thenUgn` \ y_gentid ->
	returnUgn (U_entid y_gentid)
    else if tag == ``enttype'' then
	ioToUgnM (_ccall_ gitentid t) `thenUgn` \ x_gitentid ->
	rdU_stringId x_gitentid `thenUgn` \ y_gitentid ->
	returnUgn (U_enttype y_gitentid)
    else if tag == ``enttypeall'' then
	ioToUgnM (_ccall_ gatentid t) `thenUgn` \ x_gatentid ->
	rdU_stringId x_gatentid `thenUgn` \ y_gatentid ->
	returnUgn (U_enttypeall y_gatentid)
    else if tag == ``enttypecons'' then
	ioToUgnM (_ccall_ gctentid t) `thenUgn` \ x_gctentid ->
	rdU_stringId x_gctentid `thenUgn` \ y_gctentid ->
	ioToUgnM (_ccall_ gctentcons t) `thenUgn` \ x_gctentcons ->
	rdU_list x_gctentcons `thenUgn` \ y_gctentcons ->
	returnUgn (U_enttypecons y_gctentid y_gctentcons)
    else if tag == ``entclass'' then
	ioToUgnM (_ccall_ gcentid t) `thenUgn` \ x_gcentid ->
	rdU_stringId x_gcentid `thenUgn` \ y_gcentid ->
	ioToUgnM (_ccall_ gcentops t) `thenUgn` \ x_gcentops ->
	rdU_list x_gcentops `thenUgn` \ y_gcentops ->
	returnUgn (U_entclass y_gcentid y_gcentops)
    else if tag == ``entmod'' then
	ioToUgnM (_ccall_ gmentid t) `thenUgn` \ x_gmentid ->
	rdU_stringId x_gmentid `thenUgn` \ y_gmentid ->
	returnUgn (U_entmod y_gmentid)
    else
	error ("rdU_entidt: bad tag selection:"++show tag++"\n")
