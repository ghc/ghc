

module U_hpragma where
import UgenUtil
import Util

import U_coresyn
import U_list
import U_literal	( U_literal )	-- ditto
import U_ttype		( U_ttype )	-- interface only
data U_hpragma = U_no_pragma | U_idata_pragma U_list U_list | U_itype_pragma | U_iclas_pragma U_list | U_iclasop_pragma U_hpragma U_hpragma | U_iinst_simpl_pragma U_stringId U_hpragma | U_iinst_const_pragma U_stringId U_hpragma U_list | U_igen_pragma U_hpragma U_hpragma U_hpragma U_hpragma U_hpragma U_list | U_iarity_pragma U_numId | U_iupdate_pragma U_stringId | U_ideforest_pragma | U_istrictness_pragma U_hstring U_hpragma | U_imagic_unfolding_pragma U_stringId | U_iunfolding_pragma U_hpragma U_coresyn | U_iunfold_always | U_iunfold_if_args U_numId U_numId U_stringId U_numId | U_iname_pragma_pr U_unkId U_hpragma | U_itype_pragma_pr U_list U_numId U_hpragma | U_idata_pragma_4s U_list 

rdU_hpragma :: _Addr -> UgnM U_hpragma
rdU_hpragma t
  = ioToUgnM (_ccall_ thpragma t) `thenUgn` \ tag@(I# _) ->
    if tag == ``no_pragma'' then
	returnUgn (U_no_pragma )
    else if tag == ``idata_pragma'' then
	ioToUgnM (_ccall_ gprag_data_constrs t) `thenUgn` \ x_gprag_data_constrs ->
	rdU_list x_gprag_data_constrs `thenUgn` \ y_gprag_data_constrs ->
	ioToUgnM (_ccall_ gprag_data_specs t) `thenUgn` \ x_gprag_data_specs ->
	rdU_list x_gprag_data_specs `thenUgn` \ y_gprag_data_specs ->
	returnUgn (U_idata_pragma y_gprag_data_constrs y_gprag_data_specs)
    else if tag == ``itype_pragma'' then
	returnUgn (U_itype_pragma )
    else if tag == ``iclas_pragma'' then
	ioToUgnM (_ccall_ gprag_clas t) `thenUgn` \ x_gprag_clas ->
	rdU_list x_gprag_clas `thenUgn` \ y_gprag_clas ->
	returnUgn (U_iclas_pragma y_gprag_clas)
    else if tag == ``iclasop_pragma'' then
	ioToUgnM (_ccall_ gprag_dsel t) `thenUgn` \ x_gprag_dsel ->
	rdU_hpragma x_gprag_dsel `thenUgn` \ y_gprag_dsel ->
	ioToUgnM (_ccall_ gprag_defm t) `thenUgn` \ x_gprag_defm ->
	rdU_hpragma x_gprag_defm `thenUgn` \ y_gprag_defm ->
	returnUgn (U_iclasop_pragma y_gprag_dsel y_gprag_defm)
    else if tag == ``iinst_simpl_pragma'' then
	ioToUgnM (_ccall_ gprag_imod_simpl t) `thenUgn` \ x_gprag_imod_simpl ->
	rdU_stringId x_gprag_imod_simpl `thenUgn` \ y_gprag_imod_simpl ->
	ioToUgnM (_ccall_ gprag_dfun_simpl t) `thenUgn` \ x_gprag_dfun_simpl ->
	rdU_hpragma x_gprag_dfun_simpl `thenUgn` \ y_gprag_dfun_simpl ->
	returnUgn (U_iinst_simpl_pragma y_gprag_imod_simpl y_gprag_dfun_simpl)
    else if tag == ``iinst_const_pragma'' then
	ioToUgnM (_ccall_ gprag_imod_const t) `thenUgn` \ x_gprag_imod_const ->
	rdU_stringId x_gprag_imod_const `thenUgn` \ y_gprag_imod_const ->
	ioToUgnM (_ccall_ gprag_dfun_const t) `thenUgn` \ x_gprag_dfun_const ->
	rdU_hpragma x_gprag_dfun_const `thenUgn` \ y_gprag_dfun_const ->
	ioToUgnM (_ccall_ gprag_constms t) `thenUgn` \ x_gprag_constms ->
	rdU_list x_gprag_constms `thenUgn` \ y_gprag_constms ->
	returnUgn (U_iinst_const_pragma y_gprag_imod_const y_gprag_dfun_const y_gprag_constms)
    else if tag == ``igen_pragma'' then
	ioToUgnM (_ccall_ gprag_arity t) `thenUgn` \ x_gprag_arity ->
	rdU_hpragma x_gprag_arity `thenUgn` \ y_gprag_arity ->
	ioToUgnM (_ccall_ gprag_update t) `thenUgn` \ x_gprag_update ->
	rdU_hpragma x_gprag_update `thenUgn` \ y_gprag_update ->
	ioToUgnM (_ccall_ gprag_deforest t) `thenUgn` \ x_gprag_deforest ->
	rdU_hpragma x_gprag_deforest `thenUgn` \ y_gprag_deforest ->
	ioToUgnM (_ccall_ gprag_strictness t) `thenUgn` \ x_gprag_strictness ->
	rdU_hpragma x_gprag_strictness `thenUgn` \ y_gprag_strictness ->
	ioToUgnM (_ccall_ gprag_unfolding t) `thenUgn` \ x_gprag_unfolding ->
	rdU_hpragma x_gprag_unfolding `thenUgn` \ y_gprag_unfolding ->
	ioToUgnM (_ccall_ gprag_specs t) `thenUgn` \ x_gprag_specs ->
	rdU_list x_gprag_specs `thenUgn` \ y_gprag_specs ->
	returnUgn (U_igen_pragma y_gprag_arity y_gprag_update y_gprag_deforest y_gprag_strictness y_gprag_unfolding y_gprag_specs)
    else if tag == ``iarity_pragma'' then
	ioToUgnM (_ccall_ gprag_arity_val t) `thenUgn` \ x_gprag_arity_val ->
	rdU_numId x_gprag_arity_val `thenUgn` \ y_gprag_arity_val ->
	returnUgn (U_iarity_pragma y_gprag_arity_val)
    else if tag == ``iupdate_pragma'' then
	ioToUgnM (_ccall_ gprag_update_val t) `thenUgn` \ x_gprag_update_val ->
	rdU_stringId x_gprag_update_val `thenUgn` \ y_gprag_update_val ->
	returnUgn (U_iupdate_pragma y_gprag_update_val)
    else if tag == ``ideforest_pragma'' then
	returnUgn (U_ideforest_pragma )
    else if tag == ``istrictness_pragma'' then
	ioToUgnM (_ccall_ gprag_strict_spec t) `thenUgn` \ x_gprag_strict_spec ->
	rdU_hstring x_gprag_strict_spec `thenUgn` \ y_gprag_strict_spec ->
	ioToUgnM (_ccall_ gprag_strict_wrkr t) `thenUgn` \ x_gprag_strict_wrkr ->
	rdU_hpragma x_gprag_strict_wrkr `thenUgn` \ y_gprag_strict_wrkr ->
	returnUgn (U_istrictness_pragma y_gprag_strict_spec y_gprag_strict_wrkr)
    else if tag == ``imagic_unfolding_pragma'' then
	ioToUgnM (_ccall_ gprag_magic_str t) `thenUgn` \ x_gprag_magic_str ->
	rdU_stringId x_gprag_magic_str `thenUgn` \ y_gprag_magic_str ->
	returnUgn (U_imagic_unfolding_pragma y_gprag_magic_str)
    else if tag == ``iunfolding_pragma'' then
	ioToUgnM (_ccall_ gprag_unfold_guide t) `thenUgn` \ x_gprag_unfold_guide ->
	rdU_hpragma x_gprag_unfold_guide `thenUgn` \ y_gprag_unfold_guide ->
	ioToUgnM (_ccall_ gprag_unfold_core t) `thenUgn` \ x_gprag_unfold_core ->
	rdU_coresyn x_gprag_unfold_core `thenUgn` \ y_gprag_unfold_core ->
	returnUgn (U_iunfolding_pragma y_gprag_unfold_guide y_gprag_unfold_core)
    else if tag == ``iunfold_always'' then
	returnUgn (U_iunfold_always )
    else if tag == ``iunfold_if_args'' then
	ioToUgnM (_ccall_ gprag_unfold_if_t_args t) `thenUgn` \ x_gprag_unfold_if_t_args ->
	rdU_numId x_gprag_unfold_if_t_args `thenUgn` \ y_gprag_unfold_if_t_args ->
	ioToUgnM (_ccall_ gprag_unfold_if_v_args t) `thenUgn` \ x_gprag_unfold_if_v_args ->
	rdU_numId x_gprag_unfold_if_v_args `thenUgn` \ y_gprag_unfold_if_v_args ->
	ioToUgnM (_ccall_ gprag_unfold_if_con_args t) `thenUgn` \ x_gprag_unfold_if_con_args ->
	rdU_stringId x_gprag_unfold_if_con_args `thenUgn` \ y_gprag_unfold_if_con_args ->
	ioToUgnM (_ccall_ gprag_unfold_if_size t) `thenUgn` \ x_gprag_unfold_if_size ->
	rdU_numId x_gprag_unfold_if_size `thenUgn` \ y_gprag_unfold_if_size ->
	returnUgn (U_iunfold_if_args y_gprag_unfold_if_t_args y_gprag_unfold_if_v_args y_gprag_unfold_if_con_args y_gprag_unfold_if_size)
    else if tag == ``iname_pragma_pr'' then
	ioToUgnM (_ccall_ gprag_name_pr1 t) `thenUgn` \ x_gprag_name_pr1 ->
	rdU_unkId x_gprag_name_pr1 `thenUgn` \ y_gprag_name_pr1 ->
	ioToUgnM (_ccall_ gprag_name_pr2 t) `thenUgn` \ x_gprag_name_pr2 ->
	rdU_hpragma x_gprag_name_pr2 `thenUgn` \ y_gprag_name_pr2 ->
	returnUgn (U_iname_pragma_pr y_gprag_name_pr1 y_gprag_name_pr2)
    else if tag == ``itype_pragma_pr'' then
	ioToUgnM (_ccall_ gprag_type_pr1 t) `thenUgn` \ x_gprag_type_pr1 ->
	rdU_list x_gprag_type_pr1 `thenUgn` \ y_gprag_type_pr1 ->
	ioToUgnM (_ccall_ gprag_type_pr2 t) `thenUgn` \ x_gprag_type_pr2 ->
	rdU_numId x_gprag_type_pr2 `thenUgn` \ y_gprag_type_pr2 ->
	ioToUgnM (_ccall_ gprag_type_pr3 t) `thenUgn` \ x_gprag_type_pr3 ->
	rdU_hpragma x_gprag_type_pr3 `thenUgn` \ y_gprag_type_pr3 ->
	returnUgn (U_itype_pragma_pr y_gprag_type_pr1 y_gprag_type_pr2 y_gprag_type_pr3)
    else if tag == ``idata_pragma_4s'' then
	ioToUgnM (_ccall_ gprag_data_spec t) `thenUgn` \ x_gprag_data_spec ->
	rdU_list x_gprag_data_spec `thenUgn` \ y_gprag_data_spec ->
	returnUgn (U_idata_pragma_4s y_gprag_data_spec)
    else
	error ("rdU_hpragma: bad tag selection:"++show tag++"\n")
