

module U_binding where
import UgenUtil
import Util

import U_coresyn	( U_coresyn ) -- for interfaces only
import U_hpragma
import U_list
import U_literal	( U_literal ) -- for interfaces only
import U_ttype
data U_binding = U_tbind U_list U_ttype U_list U_list U_long U_hpragma | U_nbind U_ttype U_ttype U_long U_hpragma | U_pbind U_list U_long | U_fbind U_list U_long | U_abind U_binding U_binding | U_ibind U_list U_unkId U_ttype U_binding U_long U_hpragma | U_dbind U_list U_long | U_cbind U_list U_ttype U_binding U_long U_hpragma | U_sbind U_list U_ttype U_long U_hpragma | U_mbind U_stringId U_list U_list U_long | U_nullbind | U_import U_stringId U_list U_list U_binding U_stringId U_long | U_hiding U_stringId U_list U_list U_binding U_stringId U_long | U_vspec_uprag U_unkId U_list U_long | U_vspec_ty_and_id U_ttype U_list | U_ispec_uprag U_unkId U_ttype U_long | U_inline_uprag U_unkId U_list U_long | U_deforest_uprag U_unkId U_long | U_magicuf_uprag U_unkId U_stringId U_long | U_abstract_uprag U_unkId U_long | U_dspec_uprag U_unkId U_list U_long 

rdU_binding :: _Addr -> UgnM U_binding
rdU_binding t
  = ioToUgnM (_ccall_ tbinding t) `thenUgn` \ tag@(I# _) ->
    if tag == ``tbind'' then
	ioToUgnM (_ccall_ gtbindc t) `thenUgn` \ x_gtbindc ->
	rdU_list x_gtbindc `thenUgn` \ y_gtbindc ->
	ioToUgnM (_ccall_ gtbindid t) `thenUgn` \ x_gtbindid ->
	rdU_ttype x_gtbindid `thenUgn` \ y_gtbindid ->
	ioToUgnM (_ccall_ gtbindl t) `thenUgn` \ x_gtbindl ->
	rdU_list x_gtbindl `thenUgn` \ y_gtbindl ->
	ioToUgnM (_ccall_ gtbindd t) `thenUgn` \ x_gtbindd ->
	rdU_list x_gtbindd `thenUgn` \ y_gtbindd ->
	ioToUgnM (_ccall_ gtline t) `thenUgn` \ x_gtline ->
	rdU_long x_gtline `thenUgn` \ y_gtline ->
	ioToUgnM (_ccall_ gtpragma t) `thenUgn` \ x_gtpragma ->
	rdU_hpragma x_gtpragma `thenUgn` \ y_gtpragma ->
	returnUgn (U_tbind y_gtbindc y_gtbindid y_gtbindl y_gtbindd y_gtline y_gtpragma)
    else if tag == ``nbind'' then
	ioToUgnM (_ccall_ gnbindid t) `thenUgn` \ x_gnbindid ->
	rdU_ttype x_gnbindid `thenUgn` \ y_gnbindid ->
	ioToUgnM (_ccall_ gnbindas t) `thenUgn` \ x_gnbindas ->
	rdU_ttype x_gnbindas `thenUgn` \ y_gnbindas ->
	ioToUgnM (_ccall_ gnline t) `thenUgn` \ x_gnline ->
	rdU_long x_gnline `thenUgn` \ y_gnline ->
	ioToUgnM (_ccall_ gnpragma t) `thenUgn` \ x_gnpragma ->
	rdU_hpragma x_gnpragma `thenUgn` \ y_gnpragma ->
	returnUgn (U_nbind y_gnbindid y_gnbindas y_gnline y_gnpragma)
    else if tag == ``pbind'' then
	ioToUgnM (_ccall_ gpbindl t) `thenUgn` \ x_gpbindl ->
	rdU_list x_gpbindl `thenUgn` \ y_gpbindl ->
	ioToUgnM (_ccall_ gpline t) `thenUgn` \ x_gpline ->
	rdU_long x_gpline `thenUgn` \ y_gpline ->
	returnUgn (U_pbind y_gpbindl y_gpline)
    else if tag == ``fbind'' then
	ioToUgnM (_ccall_ gfbindl t) `thenUgn` \ x_gfbindl ->
	rdU_list x_gfbindl `thenUgn` \ y_gfbindl ->
	ioToUgnM (_ccall_ gfline t) `thenUgn` \ x_gfline ->
	rdU_long x_gfline `thenUgn` \ y_gfline ->
	returnUgn (U_fbind y_gfbindl y_gfline)
    else if tag == ``abind'' then
	ioToUgnM (_ccall_ gabindfst t) `thenUgn` \ x_gabindfst ->
	rdU_binding x_gabindfst `thenUgn` \ y_gabindfst ->
	ioToUgnM (_ccall_ gabindsnd t) `thenUgn` \ x_gabindsnd ->
	rdU_binding x_gabindsnd `thenUgn` \ y_gabindsnd ->
	returnUgn (U_abind y_gabindfst y_gabindsnd)
    else if tag == ``ibind'' then
	ioToUgnM (_ccall_ gibindc t) `thenUgn` \ x_gibindc ->
	rdU_list x_gibindc `thenUgn` \ y_gibindc ->
	ioToUgnM (_ccall_ gibindid t) `thenUgn` \ x_gibindid ->
	rdU_unkId x_gibindid `thenUgn` \ y_gibindid ->
	ioToUgnM (_ccall_ gibindi t) `thenUgn` \ x_gibindi ->
	rdU_ttype x_gibindi `thenUgn` \ y_gibindi ->
	ioToUgnM (_ccall_ gibindw t) `thenUgn` \ x_gibindw ->
	rdU_binding x_gibindw `thenUgn` \ y_gibindw ->
	ioToUgnM (_ccall_ giline t) `thenUgn` \ x_giline ->
	rdU_long x_giline `thenUgn` \ y_giline ->
	ioToUgnM (_ccall_ gipragma t) `thenUgn` \ x_gipragma ->
	rdU_hpragma x_gipragma `thenUgn` \ y_gipragma ->
	returnUgn (U_ibind y_gibindc y_gibindid y_gibindi y_gibindw y_giline y_gipragma)
    else if tag == ``dbind'' then
	ioToUgnM (_ccall_ gdbindts t) `thenUgn` \ x_gdbindts ->
	rdU_list x_gdbindts `thenUgn` \ y_gdbindts ->
	ioToUgnM (_ccall_ gdline t) `thenUgn` \ x_gdline ->
	rdU_long x_gdline `thenUgn` \ y_gdline ->
	returnUgn (U_dbind y_gdbindts y_gdline)
    else if tag == ``cbind'' then
	ioToUgnM (_ccall_ gcbindc t) `thenUgn` \ x_gcbindc ->
	rdU_list x_gcbindc `thenUgn` \ y_gcbindc ->
	ioToUgnM (_ccall_ gcbindid t) `thenUgn` \ x_gcbindid ->
	rdU_ttype x_gcbindid `thenUgn` \ y_gcbindid ->
	ioToUgnM (_ccall_ gcbindw t) `thenUgn` \ x_gcbindw ->
	rdU_binding x_gcbindw `thenUgn` \ y_gcbindw ->
	ioToUgnM (_ccall_ gcline t) `thenUgn` \ x_gcline ->
	rdU_long x_gcline `thenUgn` \ y_gcline ->
	ioToUgnM (_ccall_ gcpragma t) `thenUgn` \ x_gcpragma ->
	rdU_hpragma x_gcpragma `thenUgn` \ y_gcpragma ->
	returnUgn (U_cbind y_gcbindc y_gcbindid y_gcbindw y_gcline y_gcpragma)
    else if tag == ``sbind'' then
	ioToUgnM (_ccall_ gsbindids t) `thenUgn` \ x_gsbindids ->
	rdU_list x_gsbindids `thenUgn` \ y_gsbindids ->
	ioToUgnM (_ccall_ gsbindid t) `thenUgn` \ x_gsbindid ->
	rdU_ttype x_gsbindid `thenUgn` \ y_gsbindid ->
	ioToUgnM (_ccall_ gsline t) `thenUgn` \ x_gsline ->
	rdU_long x_gsline `thenUgn` \ y_gsline ->
	ioToUgnM (_ccall_ gspragma t) `thenUgn` \ x_gspragma ->
	rdU_hpragma x_gspragma `thenUgn` \ y_gspragma ->
	returnUgn (U_sbind y_gsbindids y_gsbindid y_gsline y_gspragma)
    else if tag == ``mbind'' then
	ioToUgnM (_ccall_ gmbindmodn t) `thenUgn` \ x_gmbindmodn ->
	rdU_stringId x_gmbindmodn `thenUgn` \ y_gmbindmodn ->
	ioToUgnM (_ccall_ gmbindimp t) `thenUgn` \ x_gmbindimp ->
	rdU_list x_gmbindimp `thenUgn` \ y_gmbindimp ->
	ioToUgnM (_ccall_ gmbindren t) `thenUgn` \ x_gmbindren ->
	rdU_list x_gmbindren `thenUgn` \ y_gmbindren ->
	ioToUgnM (_ccall_ gmline t) `thenUgn` \ x_gmline ->
	rdU_long x_gmline `thenUgn` \ y_gmline ->
	returnUgn (U_mbind y_gmbindmodn y_gmbindimp y_gmbindren y_gmline)
    else if tag == ``nullbind'' then
	returnUgn (U_nullbind )
    else if tag == ``import'' then
	ioToUgnM (_ccall_ giebindmod t) `thenUgn` \ x_giebindmod ->
	rdU_stringId x_giebindmod `thenUgn` \ y_giebindmod ->
	ioToUgnM (_ccall_ giebindexp t) `thenUgn` \ x_giebindexp ->
	rdU_list x_giebindexp `thenUgn` \ y_giebindexp ->
	ioToUgnM (_ccall_ giebindren t) `thenUgn` \ x_giebindren ->
	rdU_list x_giebindren `thenUgn` \ y_giebindren ->
	ioToUgnM (_ccall_ giebinddef t) `thenUgn` \ x_giebinddef ->
	rdU_binding x_giebinddef `thenUgn` \ y_giebinddef ->
	ioToUgnM (_ccall_ giebindfile t) `thenUgn` \ x_giebindfile ->
	rdU_stringId x_giebindfile `thenUgn` \ y_giebindfile ->
	ioToUgnM (_ccall_ giebindline t) `thenUgn` \ x_giebindline ->
	rdU_long x_giebindline `thenUgn` \ y_giebindline ->
	returnUgn (U_import y_giebindmod y_giebindexp y_giebindren y_giebinddef y_giebindfile y_giebindline)
    else if tag == ``hiding'' then
	ioToUgnM (_ccall_ gihbindmod t) `thenUgn` \ x_gihbindmod ->
	rdU_stringId x_gihbindmod `thenUgn` \ y_gihbindmod ->
	ioToUgnM (_ccall_ gihbindexp t) `thenUgn` \ x_gihbindexp ->
	rdU_list x_gihbindexp `thenUgn` \ y_gihbindexp ->
	ioToUgnM (_ccall_ gihbindren t) `thenUgn` \ x_gihbindren ->
	rdU_list x_gihbindren `thenUgn` \ y_gihbindren ->
	ioToUgnM (_ccall_ gihbinddef t) `thenUgn` \ x_gihbinddef ->
	rdU_binding x_gihbinddef `thenUgn` \ y_gihbinddef ->
	ioToUgnM (_ccall_ gihbindfile t) `thenUgn` \ x_gihbindfile ->
	rdU_stringId x_gihbindfile `thenUgn` \ y_gihbindfile ->
	ioToUgnM (_ccall_ gihbindline t) `thenUgn` \ x_gihbindline ->
	rdU_long x_gihbindline `thenUgn` \ y_gihbindline ->
	returnUgn (U_hiding y_gihbindmod y_gihbindexp y_gihbindren y_gihbinddef y_gihbindfile y_gihbindline)
    else if tag == ``vspec_uprag'' then
	ioToUgnM (_ccall_ gvspec_id t) `thenUgn` \ x_gvspec_id ->
	rdU_unkId x_gvspec_id `thenUgn` \ y_gvspec_id ->
	ioToUgnM (_ccall_ gvspec_tys t) `thenUgn` \ x_gvspec_tys ->
	rdU_list x_gvspec_tys `thenUgn` \ y_gvspec_tys ->
	ioToUgnM (_ccall_ gvspec_line t) `thenUgn` \ x_gvspec_line ->
	rdU_long x_gvspec_line `thenUgn` \ y_gvspec_line ->
	returnUgn (U_vspec_uprag y_gvspec_id y_gvspec_tys y_gvspec_line)
    else if tag == ``vspec_ty_and_id'' then
	ioToUgnM (_ccall_ gvspec_ty t) `thenUgn` \ x_gvspec_ty ->
	rdU_ttype x_gvspec_ty `thenUgn` \ y_gvspec_ty ->
	ioToUgnM (_ccall_ gvspec_tyid t) `thenUgn` \ x_gvspec_tyid ->
	rdU_list x_gvspec_tyid `thenUgn` \ y_gvspec_tyid ->
	returnUgn (U_vspec_ty_and_id y_gvspec_ty y_gvspec_tyid)
    else if tag == ``ispec_uprag'' then
	ioToUgnM (_ccall_ gispec_clas t) `thenUgn` \ x_gispec_clas ->
	rdU_unkId x_gispec_clas `thenUgn` \ y_gispec_clas ->
	ioToUgnM (_ccall_ gispec_ty t) `thenUgn` \ x_gispec_ty ->
	rdU_ttype x_gispec_ty `thenUgn` \ y_gispec_ty ->
	ioToUgnM (_ccall_ gispec_line t) `thenUgn` \ x_gispec_line ->
	rdU_long x_gispec_line `thenUgn` \ y_gispec_line ->
	returnUgn (U_ispec_uprag y_gispec_clas y_gispec_ty y_gispec_line)
    else if tag == ``inline_uprag'' then
	ioToUgnM (_ccall_ ginline_id t) `thenUgn` \ x_ginline_id ->
	rdU_unkId x_ginline_id `thenUgn` \ y_ginline_id ->
	ioToUgnM (_ccall_ ginline_howto t) `thenUgn` \ x_ginline_howto ->
	rdU_list x_ginline_howto `thenUgn` \ y_ginline_howto ->
	ioToUgnM (_ccall_ ginline_line t) `thenUgn` \ x_ginline_line ->
	rdU_long x_ginline_line `thenUgn` \ y_ginline_line ->
	returnUgn (U_inline_uprag y_ginline_id y_ginline_howto y_ginline_line)
    else if tag == ``deforest_uprag'' then
	ioToUgnM (_ccall_ gdeforest_id t) `thenUgn` \ x_gdeforest_id ->
	rdU_unkId x_gdeforest_id `thenUgn` \ y_gdeforest_id ->
	ioToUgnM (_ccall_ gdeforest_line t) `thenUgn` \ x_gdeforest_line ->
	rdU_long x_gdeforest_line `thenUgn` \ y_gdeforest_line ->
	returnUgn (U_deforest_uprag y_gdeforest_id y_gdeforest_line)
    else if tag == ``magicuf_uprag'' then
	ioToUgnM (_ccall_ gmagicuf_id t) `thenUgn` \ x_gmagicuf_id ->
	rdU_unkId x_gmagicuf_id `thenUgn` \ y_gmagicuf_id ->
	ioToUgnM (_ccall_ gmagicuf_str t) `thenUgn` \ x_gmagicuf_str ->
	rdU_stringId x_gmagicuf_str `thenUgn` \ y_gmagicuf_str ->
	ioToUgnM (_ccall_ gmagicuf_line t) `thenUgn` \ x_gmagicuf_line ->
	rdU_long x_gmagicuf_line `thenUgn` \ y_gmagicuf_line ->
	returnUgn (U_magicuf_uprag y_gmagicuf_id y_gmagicuf_str y_gmagicuf_line)
    else if tag == ``abstract_uprag'' then
	ioToUgnM (_ccall_ gabstract_id t) `thenUgn` \ x_gabstract_id ->
	rdU_unkId x_gabstract_id `thenUgn` \ y_gabstract_id ->
	ioToUgnM (_ccall_ gabstract_line t) `thenUgn` \ x_gabstract_line ->
	rdU_long x_gabstract_line `thenUgn` \ y_gabstract_line ->
	returnUgn (U_abstract_uprag y_gabstract_id y_gabstract_line)
    else if tag == ``dspec_uprag'' then
	ioToUgnM (_ccall_ gdspec_id t) `thenUgn` \ x_gdspec_id ->
	rdU_unkId x_gdspec_id `thenUgn` \ y_gdspec_id ->
	ioToUgnM (_ccall_ gdspec_tys t) `thenUgn` \ x_gdspec_tys ->
	rdU_list x_gdspec_tys `thenUgn` \ y_gdspec_tys ->
	ioToUgnM (_ccall_ gdspec_line t) `thenUgn` \ x_gdspec_line ->
	rdU_long x_gdspec_line `thenUgn` \ y_gdspec_line ->
	returnUgn (U_dspec_uprag y_gdspec_id y_gdspec_tys y_gdspec_line)
    else
	error ("rdU_binding: bad tag selection:"++show tag++"\n")
