

module U_coresyn where
import UgenUtil
import Util

import U_list
import U_literal
import U_ttype
data U_coresyn = U_cobinder U_unkId U_ttype | U_colit U_literal | U_colocal U_coresyn | U_cononrec U_coresyn U_coresyn | U_corec U_list | U_corec_pair U_coresyn U_coresyn | U_covar U_coresyn | U_coliteral U_literal | U_cocon U_coresyn U_list U_list | U_coprim U_coresyn U_list U_list | U_colam U_list U_coresyn | U_cotylam U_list U_coresyn | U_coapp U_coresyn U_list | U_cotyapp U_coresyn U_ttype | U_cocase U_coresyn U_coresyn | U_colet U_coresyn U_coresyn | U_coscc U_coresyn U_coresyn | U_coalg_alts U_list U_coresyn | U_coalg_alt U_coresyn U_list U_coresyn | U_coprim_alts U_list U_coresyn | U_coprim_alt U_literal U_coresyn | U_conodeflt | U_cobinddeflt U_coresyn U_coresyn | U_co_primop U_stringId | U_co_ccall U_stringId U_long U_list U_ttype | U_co_casm U_literal U_long U_list U_ttype | U_co_preludedictscc U_coresyn | U_co_alldictscc U_hstring U_hstring U_coresyn | U_co_usercc U_hstring U_hstring U_hstring U_coresyn U_coresyn | U_co_autocc U_coresyn U_hstring U_hstring U_coresyn U_coresyn | U_co_dictcc U_coresyn U_hstring U_hstring U_coresyn U_coresyn | U_co_scc_noncaf | U_co_scc_caf | U_co_scc_nondupd | U_co_scc_dupd | U_co_id U_stringId | U_co_orig_id U_stringId U_stringId | U_co_sdselid U_unkId U_unkId | U_co_classopid U_unkId U_unkId | U_co_defmid U_unkId U_unkId | U_co_dfunid U_unkId U_ttype | U_co_constmid U_unkId U_unkId U_ttype | U_co_specid U_coresyn U_list | U_co_wrkrid U_coresyn 

rdU_coresyn :: _Addr -> UgnM U_coresyn
rdU_coresyn t
  = ioToUgnM (_ccall_ tcoresyn t) `thenUgn` \ tag@(I# _) ->
    if tag == ``cobinder'' then
	ioToUgnM (_ccall_ gcobinder_v t) `thenUgn` \ x_gcobinder_v ->
	rdU_unkId x_gcobinder_v `thenUgn` \ y_gcobinder_v ->
	ioToUgnM (_ccall_ gcobinder_ty t) `thenUgn` \ x_gcobinder_ty ->
	rdU_ttype x_gcobinder_ty `thenUgn` \ y_gcobinder_ty ->
	returnUgn (U_cobinder y_gcobinder_v y_gcobinder_ty)
    else if tag == ``colit'' then
	ioToUgnM (_ccall_ gcolit t) `thenUgn` \ x_gcolit ->
	rdU_literal x_gcolit `thenUgn` \ y_gcolit ->
	returnUgn (U_colit y_gcolit)
    else if tag == ``colocal'' then
	ioToUgnM (_ccall_ gcolocal_v t) `thenUgn` \ x_gcolocal_v ->
	rdU_coresyn x_gcolocal_v `thenUgn` \ y_gcolocal_v ->
	returnUgn (U_colocal y_gcolocal_v)
    else if tag == ``cononrec'' then
	ioToUgnM (_ccall_ gcononrec_b t) `thenUgn` \ x_gcononrec_b ->
	rdU_coresyn x_gcononrec_b `thenUgn` \ y_gcononrec_b ->
	ioToUgnM (_ccall_ gcononrec_rhs t) `thenUgn` \ x_gcononrec_rhs ->
	rdU_coresyn x_gcononrec_rhs `thenUgn` \ y_gcononrec_rhs ->
	returnUgn (U_cononrec y_gcononrec_b y_gcononrec_rhs)
    else if tag == ``corec'' then
	ioToUgnM (_ccall_ gcorec t) `thenUgn` \ x_gcorec ->
	rdU_list x_gcorec `thenUgn` \ y_gcorec ->
	returnUgn (U_corec y_gcorec)
    else if tag == ``corec_pair'' then
	ioToUgnM (_ccall_ gcorec_b t) `thenUgn` \ x_gcorec_b ->
	rdU_coresyn x_gcorec_b `thenUgn` \ y_gcorec_b ->
	ioToUgnM (_ccall_ gcorec_rhs t) `thenUgn` \ x_gcorec_rhs ->
	rdU_coresyn x_gcorec_rhs `thenUgn` \ y_gcorec_rhs ->
	returnUgn (U_corec_pair y_gcorec_b y_gcorec_rhs)
    else if tag == ``covar'' then
	ioToUgnM (_ccall_ gcovar t) `thenUgn` \ x_gcovar ->
	rdU_coresyn x_gcovar `thenUgn` \ y_gcovar ->
	returnUgn (U_covar y_gcovar)
    else if tag == ``coliteral'' then
	ioToUgnM (_ccall_ gcoliteral t) `thenUgn` \ x_gcoliteral ->
	rdU_literal x_gcoliteral `thenUgn` \ y_gcoliteral ->
	returnUgn (U_coliteral y_gcoliteral)
    else if tag == ``cocon'' then
	ioToUgnM (_ccall_ gcocon_con t) `thenUgn` \ x_gcocon_con ->
	rdU_coresyn x_gcocon_con `thenUgn` \ y_gcocon_con ->
	ioToUgnM (_ccall_ gcocon_tys t) `thenUgn` \ x_gcocon_tys ->
	rdU_list x_gcocon_tys `thenUgn` \ y_gcocon_tys ->
	ioToUgnM (_ccall_ gcocon_args t) `thenUgn` \ x_gcocon_args ->
	rdU_list x_gcocon_args `thenUgn` \ y_gcocon_args ->
	returnUgn (U_cocon y_gcocon_con y_gcocon_tys y_gcocon_args)
    else if tag == ``coprim'' then
	ioToUgnM (_ccall_ gcoprim_op t) `thenUgn` \ x_gcoprim_op ->
	rdU_coresyn x_gcoprim_op `thenUgn` \ y_gcoprim_op ->
	ioToUgnM (_ccall_ gcoprim_tys t) `thenUgn` \ x_gcoprim_tys ->
	rdU_list x_gcoprim_tys `thenUgn` \ y_gcoprim_tys ->
	ioToUgnM (_ccall_ gcoprim_args t) `thenUgn` \ x_gcoprim_args ->
	rdU_list x_gcoprim_args `thenUgn` \ y_gcoprim_args ->
	returnUgn (U_coprim y_gcoprim_op y_gcoprim_tys y_gcoprim_args)
    else if tag == ``colam'' then
	ioToUgnM (_ccall_ gcolam_vars t) `thenUgn` \ x_gcolam_vars ->
	rdU_list x_gcolam_vars `thenUgn` \ y_gcolam_vars ->
	ioToUgnM (_ccall_ gcolam_body t) `thenUgn` \ x_gcolam_body ->
	rdU_coresyn x_gcolam_body `thenUgn` \ y_gcolam_body ->
	returnUgn (U_colam y_gcolam_vars y_gcolam_body)
    else if tag == ``cotylam'' then
	ioToUgnM (_ccall_ gcotylam_tvs t) `thenUgn` \ x_gcotylam_tvs ->
	rdU_list x_gcotylam_tvs `thenUgn` \ y_gcotylam_tvs ->
	ioToUgnM (_ccall_ gcotylam_body t) `thenUgn` \ x_gcotylam_body ->
	rdU_coresyn x_gcotylam_body `thenUgn` \ y_gcotylam_body ->
	returnUgn (U_cotylam y_gcotylam_tvs y_gcotylam_body)
    else if tag == ``coapp'' then
	ioToUgnM (_ccall_ gcoapp_fun t) `thenUgn` \ x_gcoapp_fun ->
	rdU_coresyn x_gcoapp_fun `thenUgn` \ y_gcoapp_fun ->
	ioToUgnM (_ccall_ gcoapp_args t) `thenUgn` \ x_gcoapp_args ->
	rdU_list x_gcoapp_args `thenUgn` \ y_gcoapp_args ->
	returnUgn (U_coapp y_gcoapp_fun y_gcoapp_args)
    else if tag == ``cotyapp'' then
	ioToUgnM (_ccall_ gcotyapp_e t) `thenUgn` \ x_gcotyapp_e ->
	rdU_coresyn x_gcotyapp_e `thenUgn` \ y_gcotyapp_e ->
	ioToUgnM (_ccall_ gcotyapp_t t) `thenUgn` \ x_gcotyapp_t ->
	rdU_ttype x_gcotyapp_t `thenUgn` \ y_gcotyapp_t ->
	returnUgn (U_cotyapp y_gcotyapp_e y_gcotyapp_t)
    else if tag == ``cocase'' then
	ioToUgnM (_ccall_ gcocase_s t) `thenUgn` \ x_gcocase_s ->
	rdU_coresyn x_gcocase_s `thenUgn` \ y_gcocase_s ->
	ioToUgnM (_ccall_ gcocase_alts t) `thenUgn` \ x_gcocase_alts ->
	rdU_coresyn x_gcocase_alts `thenUgn` \ y_gcocase_alts ->
	returnUgn (U_cocase y_gcocase_s y_gcocase_alts)
    else if tag == ``colet'' then
	ioToUgnM (_ccall_ gcolet_bind t) `thenUgn` \ x_gcolet_bind ->
	rdU_coresyn x_gcolet_bind `thenUgn` \ y_gcolet_bind ->
	ioToUgnM (_ccall_ gcolet_body t) `thenUgn` \ x_gcolet_body ->
	rdU_coresyn x_gcolet_body `thenUgn` \ y_gcolet_body ->
	returnUgn (U_colet y_gcolet_bind y_gcolet_body)
    else if tag == ``coscc'' then
	ioToUgnM (_ccall_ gcoscc_scc t) `thenUgn` \ x_gcoscc_scc ->
	rdU_coresyn x_gcoscc_scc `thenUgn` \ y_gcoscc_scc ->
	ioToUgnM (_ccall_ gcoscc_body t) `thenUgn` \ x_gcoscc_body ->
	rdU_coresyn x_gcoscc_body `thenUgn` \ y_gcoscc_body ->
	returnUgn (U_coscc y_gcoscc_scc y_gcoscc_body)
    else if tag == ``coalg_alts'' then
	ioToUgnM (_ccall_ gcoalg_alts t) `thenUgn` \ x_gcoalg_alts ->
	rdU_list x_gcoalg_alts `thenUgn` \ y_gcoalg_alts ->
	ioToUgnM (_ccall_ gcoalg_deflt t) `thenUgn` \ x_gcoalg_deflt ->
	rdU_coresyn x_gcoalg_deflt `thenUgn` \ y_gcoalg_deflt ->
	returnUgn (U_coalg_alts y_gcoalg_alts y_gcoalg_deflt)
    else if tag == ``coalg_alt'' then
	ioToUgnM (_ccall_ gcoalg_con t) `thenUgn` \ x_gcoalg_con ->
	rdU_coresyn x_gcoalg_con `thenUgn` \ y_gcoalg_con ->
	ioToUgnM (_ccall_ gcoalg_bs t) `thenUgn` \ x_gcoalg_bs ->
	rdU_list x_gcoalg_bs `thenUgn` \ y_gcoalg_bs ->
	ioToUgnM (_ccall_ gcoalg_rhs t) `thenUgn` \ x_gcoalg_rhs ->
	rdU_coresyn x_gcoalg_rhs `thenUgn` \ y_gcoalg_rhs ->
	returnUgn (U_coalg_alt y_gcoalg_con y_gcoalg_bs y_gcoalg_rhs)
    else if tag == ``coprim_alts'' then
	ioToUgnM (_ccall_ gcoprim_alts t) `thenUgn` \ x_gcoprim_alts ->
	rdU_list x_gcoprim_alts `thenUgn` \ y_gcoprim_alts ->
	ioToUgnM (_ccall_ gcoprim_deflt t) `thenUgn` \ x_gcoprim_deflt ->
	rdU_coresyn x_gcoprim_deflt `thenUgn` \ y_gcoprim_deflt ->
	returnUgn (U_coprim_alts y_gcoprim_alts y_gcoprim_deflt)
    else if tag == ``coprim_alt'' then
	ioToUgnM (_ccall_ gcoprim_lit t) `thenUgn` \ x_gcoprim_lit ->
	rdU_literal x_gcoprim_lit `thenUgn` \ y_gcoprim_lit ->
	ioToUgnM (_ccall_ gcoprim_rhs t) `thenUgn` \ x_gcoprim_rhs ->
	rdU_coresyn x_gcoprim_rhs `thenUgn` \ y_gcoprim_rhs ->
	returnUgn (U_coprim_alt y_gcoprim_lit y_gcoprim_rhs)
    else if tag == ``conodeflt'' then
	returnUgn (U_conodeflt )
    else if tag == ``cobinddeflt'' then
	ioToUgnM (_ccall_ gcobinddeflt_v t) `thenUgn` \ x_gcobinddeflt_v ->
	rdU_coresyn x_gcobinddeflt_v `thenUgn` \ y_gcobinddeflt_v ->
	ioToUgnM (_ccall_ gcobinddeflt_rhs t) `thenUgn` \ x_gcobinddeflt_rhs ->
	rdU_coresyn x_gcobinddeflt_rhs `thenUgn` \ y_gcobinddeflt_rhs ->
	returnUgn (U_cobinddeflt y_gcobinddeflt_v y_gcobinddeflt_rhs)
    else if tag == ``co_primop'' then
	ioToUgnM (_ccall_ gco_primop t) `thenUgn` \ x_gco_primop ->
	rdU_stringId x_gco_primop `thenUgn` \ y_gco_primop ->
	returnUgn (U_co_primop y_gco_primop)
    else if tag == ``co_ccall'' then
	ioToUgnM (_ccall_ gco_ccall t) `thenUgn` \ x_gco_ccall ->
	rdU_stringId x_gco_ccall `thenUgn` \ y_gco_ccall ->
	ioToUgnM (_ccall_ gco_ccall_may_gc t) `thenUgn` \ x_gco_ccall_may_gc ->
	rdU_long x_gco_ccall_may_gc `thenUgn` \ y_gco_ccall_may_gc ->
	ioToUgnM (_ccall_ gco_ccall_arg_tys t) `thenUgn` \ x_gco_ccall_arg_tys ->
	rdU_list x_gco_ccall_arg_tys `thenUgn` \ y_gco_ccall_arg_tys ->
	ioToUgnM (_ccall_ gco_ccall_res_ty t) `thenUgn` \ x_gco_ccall_res_ty ->
	rdU_ttype x_gco_ccall_res_ty `thenUgn` \ y_gco_ccall_res_ty ->
	returnUgn (U_co_ccall y_gco_ccall y_gco_ccall_may_gc y_gco_ccall_arg_tys y_gco_ccall_res_ty)
    else if tag == ``co_casm'' then
	ioToUgnM (_ccall_ gco_casm t) `thenUgn` \ x_gco_casm ->
	rdU_literal x_gco_casm `thenUgn` \ y_gco_casm ->
	ioToUgnM (_ccall_ gco_casm_may_gc t) `thenUgn` \ x_gco_casm_may_gc ->
	rdU_long x_gco_casm_may_gc `thenUgn` \ y_gco_casm_may_gc ->
	ioToUgnM (_ccall_ gco_casm_arg_tys t) `thenUgn` \ x_gco_casm_arg_tys ->
	rdU_list x_gco_casm_arg_tys `thenUgn` \ y_gco_casm_arg_tys ->
	ioToUgnM (_ccall_ gco_casm_res_ty t) `thenUgn` \ x_gco_casm_res_ty ->
	rdU_ttype x_gco_casm_res_ty `thenUgn` \ y_gco_casm_res_ty ->
	returnUgn (U_co_casm y_gco_casm y_gco_casm_may_gc y_gco_casm_arg_tys y_gco_casm_res_ty)
    else if tag == ``co_preludedictscc'' then
	ioToUgnM (_ccall_ gco_preludedictscc_dupd t) `thenUgn` \ x_gco_preludedictscc_dupd ->
	rdU_coresyn x_gco_preludedictscc_dupd `thenUgn` \ y_gco_preludedictscc_dupd ->
	returnUgn (U_co_preludedictscc y_gco_preludedictscc_dupd)
    else if tag == ``co_alldictscc'' then
	ioToUgnM (_ccall_ gco_alldictscc_m t) `thenUgn` \ x_gco_alldictscc_m ->
	rdU_hstring x_gco_alldictscc_m `thenUgn` \ y_gco_alldictscc_m ->
	ioToUgnM (_ccall_ gco_alldictscc_g t) `thenUgn` \ x_gco_alldictscc_g ->
	rdU_hstring x_gco_alldictscc_g `thenUgn` \ y_gco_alldictscc_g ->
	ioToUgnM (_ccall_ gco_alldictscc_dupd t) `thenUgn` \ x_gco_alldictscc_dupd ->
	rdU_coresyn x_gco_alldictscc_dupd `thenUgn` \ y_gco_alldictscc_dupd ->
	returnUgn (U_co_alldictscc y_gco_alldictscc_m y_gco_alldictscc_g y_gco_alldictscc_dupd)
    else if tag == ``co_usercc'' then
	ioToUgnM (_ccall_ gco_usercc_n t) `thenUgn` \ x_gco_usercc_n ->
	rdU_hstring x_gco_usercc_n `thenUgn` \ y_gco_usercc_n ->
	ioToUgnM (_ccall_ gco_usercc_m t) `thenUgn` \ x_gco_usercc_m ->
	rdU_hstring x_gco_usercc_m `thenUgn` \ y_gco_usercc_m ->
	ioToUgnM (_ccall_ gco_usercc_g t) `thenUgn` \ x_gco_usercc_g ->
	rdU_hstring x_gco_usercc_g `thenUgn` \ y_gco_usercc_g ->
	ioToUgnM (_ccall_ gco_usercc_dupd t) `thenUgn` \ x_gco_usercc_dupd ->
	rdU_coresyn x_gco_usercc_dupd `thenUgn` \ y_gco_usercc_dupd ->
	ioToUgnM (_ccall_ gco_usercc_cafd t) `thenUgn` \ x_gco_usercc_cafd ->
	rdU_coresyn x_gco_usercc_cafd `thenUgn` \ y_gco_usercc_cafd ->
	returnUgn (U_co_usercc y_gco_usercc_n y_gco_usercc_m y_gco_usercc_g y_gco_usercc_dupd y_gco_usercc_cafd)
    else if tag == ``co_autocc'' then
	ioToUgnM (_ccall_ gco_autocc_i t) `thenUgn` \ x_gco_autocc_i ->
	rdU_coresyn x_gco_autocc_i `thenUgn` \ y_gco_autocc_i ->
	ioToUgnM (_ccall_ gco_autocc_m t) `thenUgn` \ x_gco_autocc_m ->
	rdU_hstring x_gco_autocc_m `thenUgn` \ y_gco_autocc_m ->
	ioToUgnM (_ccall_ gco_autocc_g t) `thenUgn` \ x_gco_autocc_g ->
	rdU_hstring x_gco_autocc_g `thenUgn` \ y_gco_autocc_g ->
	ioToUgnM (_ccall_ gco_autocc_dupd t) `thenUgn` \ x_gco_autocc_dupd ->
	rdU_coresyn x_gco_autocc_dupd `thenUgn` \ y_gco_autocc_dupd ->
	ioToUgnM (_ccall_ gco_autocc_cafd t) `thenUgn` \ x_gco_autocc_cafd ->
	rdU_coresyn x_gco_autocc_cafd `thenUgn` \ y_gco_autocc_cafd ->
	returnUgn (U_co_autocc y_gco_autocc_i y_gco_autocc_m y_gco_autocc_g y_gco_autocc_dupd y_gco_autocc_cafd)
    else if tag == ``co_dictcc'' then
	ioToUgnM (_ccall_ gco_dictcc_i t) `thenUgn` \ x_gco_dictcc_i ->
	rdU_coresyn x_gco_dictcc_i `thenUgn` \ y_gco_dictcc_i ->
	ioToUgnM (_ccall_ gco_dictcc_m t) `thenUgn` \ x_gco_dictcc_m ->
	rdU_hstring x_gco_dictcc_m `thenUgn` \ y_gco_dictcc_m ->
	ioToUgnM (_ccall_ gco_dictcc_g t) `thenUgn` \ x_gco_dictcc_g ->
	rdU_hstring x_gco_dictcc_g `thenUgn` \ y_gco_dictcc_g ->
	ioToUgnM (_ccall_ gco_dictcc_dupd t) `thenUgn` \ x_gco_dictcc_dupd ->
	rdU_coresyn x_gco_dictcc_dupd `thenUgn` \ y_gco_dictcc_dupd ->
	ioToUgnM (_ccall_ gco_dictcc_cafd t) `thenUgn` \ x_gco_dictcc_cafd ->
	rdU_coresyn x_gco_dictcc_cafd `thenUgn` \ y_gco_dictcc_cafd ->
	returnUgn (U_co_dictcc y_gco_dictcc_i y_gco_dictcc_m y_gco_dictcc_g y_gco_dictcc_dupd y_gco_dictcc_cafd)
    else if tag == ``co_scc_noncaf'' then
	returnUgn (U_co_scc_noncaf )
    else if tag == ``co_scc_caf'' then
	returnUgn (U_co_scc_caf )
    else if tag == ``co_scc_nondupd'' then
	returnUgn (U_co_scc_nondupd )
    else if tag == ``co_scc_dupd'' then
	returnUgn (U_co_scc_dupd )
    else if tag == ``co_id'' then
	ioToUgnM (_ccall_ gco_id t) `thenUgn` \ x_gco_id ->
	rdU_stringId x_gco_id `thenUgn` \ y_gco_id ->
	returnUgn (U_co_id y_gco_id)
    else if tag == ``co_orig_id'' then
	ioToUgnM (_ccall_ gco_orig_id_m t) `thenUgn` \ x_gco_orig_id_m ->
	rdU_stringId x_gco_orig_id_m `thenUgn` \ y_gco_orig_id_m ->
	ioToUgnM (_ccall_ gco_orig_id_n t) `thenUgn` \ x_gco_orig_id_n ->
	rdU_stringId x_gco_orig_id_n `thenUgn` \ y_gco_orig_id_n ->
	returnUgn (U_co_orig_id y_gco_orig_id_m y_gco_orig_id_n)
    else if tag == ``co_sdselid'' then
	ioToUgnM (_ccall_ gco_sdselid_c t) `thenUgn` \ x_gco_sdselid_c ->
	rdU_unkId x_gco_sdselid_c `thenUgn` \ y_gco_sdselid_c ->
	ioToUgnM (_ccall_ gco_sdselid_sc t) `thenUgn` \ x_gco_sdselid_sc ->
	rdU_unkId x_gco_sdselid_sc `thenUgn` \ y_gco_sdselid_sc ->
	returnUgn (U_co_sdselid y_gco_sdselid_c y_gco_sdselid_sc)
    else if tag == ``co_classopid'' then
	ioToUgnM (_ccall_ gco_classopid_c t) `thenUgn` \ x_gco_classopid_c ->
	rdU_unkId x_gco_classopid_c `thenUgn` \ y_gco_classopid_c ->
	ioToUgnM (_ccall_ gco_classopid_o t) `thenUgn` \ x_gco_classopid_o ->
	rdU_unkId x_gco_classopid_o `thenUgn` \ y_gco_classopid_o ->
	returnUgn (U_co_classopid y_gco_classopid_c y_gco_classopid_o)
    else if tag == ``co_defmid'' then
	ioToUgnM (_ccall_ gco_defmid_c t) `thenUgn` \ x_gco_defmid_c ->
	rdU_unkId x_gco_defmid_c `thenUgn` \ y_gco_defmid_c ->
	ioToUgnM (_ccall_ gco_defmid_op t) `thenUgn` \ x_gco_defmid_op ->
	rdU_unkId x_gco_defmid_op `thenUgn` \ y_gco_defmid_op ->
	returnUgn (U_co_defmid y_gco_defmid_c y_gco_defmid_op)
    else if tag == ``co_dfunid'' then
	ioToUgnM (_ccall_ gco_dfunid_c t) `thenUgn` \ x_gco_dfunid_c ->
	rdU_unkId x_gco_dfunid_c `thenUgn` \ y_gco_dfunid_c ->
	ioToUgnM (_ccall_ gco_dfunid_ty t) `thenUgn` \ x_gco_dfunid_ty ->
	rdU_ttype x_gco_dfunid_ty `thenUgn` \ y_gco_dfunid_ty ->
	returnUgn (U_co_dfunid y_gco_dfunid_c y_gco_dfunid_ty)
    else if tag == ``co_constmid'' then
	ioToUgnM (_ccall_ gco_constmid_c t) `thenUgn` \ x_gco_constmid_c ->
	rdU_unkId x_gco_constmid_c `thenUgn` \ y_gco_constmid_c ->
	ioToUgnM (_ccall_ gco_constmid_op t) `thenUgn` \ x_gco_constmid_op ->
	rdU_unkId x_gco_constmid_op `thenUgn` \ y_gco_constmid_op ->
	ioToUgnM (_ccall_ gco_constmid_ty t) `thenUgn` \ x_gco_constmid_ty ->
	rdU_ttype x_gco_constmid_ty `thenUgn` \ y_gco_constmid_ty ->
	returnUgn (U_co_constmid y_gco_constmid_c y_gco_constmid_op y_gco_constmid_ty)
    else if tag == ``co_specid'' then
	ioToUgnM (_ccall_ gco_specid_un t) `thenUgn` \ x_gco_specid_un ->
	rdU_coresyn x_gco_specid_un `thenUgn` \ y_gco_specid_un ->
	ioToUgnM (_ccall_ gco_specid_tys t) `thenUgn` \ x_gco_specid_tys ->
	rdU_list x_gco_specid_tys `thenUgn` \ y_gco_specid_tys ->
	returnUgn (U_co_specid y_gco_specid_un y_gco_specid_tys)
    else if tag == ``co_wrkrid'' then
	ioToUgnM (_ccall_ gco_wrkrid_un t) `thenUgn` \ x_gco_wrkrid_un ->
	rdU_coresyn x_gco_wrkrid_un `thenUgn` \ y_gco_wrkrid_un ->
	returnUgn (U_co_wrkrid y_gco_wrkrid_un)
    else
	error ("rdU_coresyn: bad tag selection:"++show tag++"\n")
