

module U_ttype where
import UgenUtil
import Util

import U_list
data U_ttype = U_tname U_unkId U_list | U_namedtvar U_unkId | U_tllist U_ttype | U_ttuple U_list | U_tfun U_ttype U_ttype | U_context U_list U_ttype | U_unidict U_unkId U_ttype | U_unityvartemplate U_unkId | U_uniforall U_list U_ttype | U_ty_maybe_nothing | U_ty_maybe_just U_ttype 

rdU_ttype :: _Addr -> UgnM U_ttype
rdU_ttype t
  = ioToUgnM (_ccall_ tttype t) `thenUgn` \ tag@(I# _) ->
    if tag == ``tname'' then
	ioToUgnM (_ccall_ gtypeid t) `thenUgn` \ x_gtypeid ->
	rdU_unkId x_gtypeid `thenUgn` \ y_gtypeid ->
	ioToUgnM (_ccall_ gtypel t) `thenUgn` \ x_gtypel ->
	rdU_list x_gtypel `thenUgn` \ y_gtypel ->
	returnUgn (U_tname y_gtypeid y_gtypel)
    else if tag == ``namedtvar'' then
	ioToUgnM (_ccall_ gnamedtvar t) `thenUgn` \ x_gnamedtvar ->
	rdU_unkId x_gnamedtvar `thenUgn` \ y_gnamedtvar ->
	returnUgn (U_namedtvar y_gnamedtvar)
    else if tag == ``tllist'' then
	ioToUgnM (_ccall_ gtlist t) `thenUgn` \ x_gtlist ->
	rdU_ttype x_gtlist `thenUgn` \ y_gtlist ->
	returnUgn (U_tllist y_gtlist)
    else if tag == ``ttuple'' then
	ioToUgnM (_ccall_ gttuple t) `thenUgn` \ x_gttuple ->
	rdU_list x_gttuple `thenUgn` \ y_gttuple ->
	returnUgn (U_ttuple y_gttuple)
    else if tag == ``tfun'' then
	ioToUgnM (_ccall_ gtfun t) `thenUgn` \ x_gtfun ->
	rdU_ttype x_gtfun `thenUgn` \ y_gtfun ->
	ioToUgnM (_ccall_ gtarg t) `thenUgn` \ x_gtarg ->
	rdU_ttype x_gtarg `thenUgn` \ y_gtarg ->
	returnUgn (U_tfun y_gtfun y_gtarg)
    else if tag == ``context'' then
	ioToUgnM (_ccall_ gtcontextl t) `thenUgn` \ x_gtcontextl ->
	rdU_list x_gtcontextl `thenUgn` \ y_gtcontextl ->
	ioToUgnM (_ccall_ gtcontextt t) `thenUgn` \ x_gtcontextt ->
	rdU_ttype x_gtcontextt `thenUgn` \ y_gtcontextt ->
	returnUgn (U_context y_gtcontextl y_gtcontextt)
    else if tag == ``unidict'' then
	ioToUgnM (_ccall_ gunidict_clas t) `thenUgn` \ x_gunidict_clas ->
	rdU_unkId x_gunidict_clas `thenUgn` \ y_gunidict_clas ->
	ioToUgnM (_ccall_ gunidict_ty t) `thenUgn` \ x_gunidict_ty ->
	rdU_ttype x_gunidict_ty `thenUgn` \ y_gunidict_ty ->
	returnUgn (U_unidict y_gunidict_clas y_gunidict_ty)
    else if tag == ``unityvartemplate'' then
	ioToUgnM (_ccall_ gunityvartemplate t) `thenUgn` \ x_gunityvartemplate ->
	rdU_unkId x_gunityvartemplate `thenUgn` \ y_gunityvartemplate ->
	returnUgn (U_unityvartemplate y_gunityvartemplate)
    else if tag == ``uniforall'' then
	ioToUgnM (_ccall_ guniforall_tv t) `thenUgn` \ x_guniforall_tv ->
	rdU_list x_guniforall_tv `thenUgn` \ y_guniforall_tv ->
	ioToUgnM (_ccall_ guniforall_ty t) `thenUgn` \ x_guniforall_ty ->
	rdU_ttype x_guniforall_ty `thenUgn` \ y_guniforall_ty ->
	returnUgn (U_uniforall y_guniforall_tv y_guniforall_ty)
    else if tag == ``ty_maybe_nothing'' then
	returnUgn (U_ty_maybe_nothing )
    else if tag == ``ty_maybe_just'' then
	ioToUgnM (_ccall_ gty_maybe t) `thenUgn` \ x_gty_maybe ->
	rdU_ttype x_gty_maybe `thenUgn` \ y_gty_maybe ->
	returnUgn (U_ty_maybe_just y_gty_maybe)
    else
	error ("rdU_ttype: bad tag selection:"++show tag++"\n")
