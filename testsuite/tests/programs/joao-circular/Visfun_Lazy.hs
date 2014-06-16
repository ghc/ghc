module Visfun_Lazy where
import Data_Lazy
import LrcPrelude
--
-- Lazy Circular Evaluator Functions
--
lrcEval = visit_P
visit_P (C_RootProd_1 t_Defs ) x_pw = (x_code , x_errlst , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = x_fmts_2
      x_typeerrors = x_typeerrors_1
      x_errlst = x_errlst_1
      x_code = ((((:) C_Data_1 (gen_data_mem x_declsout_1))++x_declscode_1)++(((:) C_Cod_1 (genCodeMainFun ))++x_code_1))
      x_infun_1 = 0
      x_env_1 = x_declsout_1
      x_declsin_1 = (lrc_empty_map (C_EmptyEntry_1 ))
      x_pw_2 = x_pw
      t_uu_pp = (C_Best_1 x_uu_pp_1)
      (x_code_1 , x_declscode_1 , x_declsout_1 , x_errlst_1 , x_outfun_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Defs t_Defs x_declsin_1 x_env_1 x_infun_1
      (x_fmts_2 ) = visit_PPRoot t_uu_pp x_pw_2
visit_Defs (C_Defs2_1 t_Def t_Defs_2 ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_infun_2 = x_outfun_1
      x_env_2 = x_env
      x_declsin_2 = x_declsout_1
      x_uu_pp = (C_Above_1 x_uu_pp_1 x_uu_pp_2)
      x_typeerrors = (x_typeerrors_1++x_typeerrors_2)
      x_outfun = x_outfun_2
      x_errlst = (x_errlst_1++x_errlst_2)
      x_declsout = x_declsout_2
      x_declscode = (x_declscode_1++x_declscode_2)
      x_code = (x_code_1++x_code_2)
      x_infun_1 = x_infun
      x_env_1 = x_env
      x_declsin_1 = x_declsin
      (x_code_1 , x_declscode_1 , x_declsout_1 , x_errlst_1 , x_outfun_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Def t_Def x_declsin_1 x_env_1 x_infun_1
      (x_code_2 , x_declscode_2 , x_declsout_2 , x_errlst_2 , x_outfun_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Defs t_Defs_2 x_declsin_2 x_env_2 x_infun_2
visit_Defs (C_NoDefs_1 ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Empty_1 )
      x_typeerrors = ([] )
      x_outfun = x_infun
      x_errlst = ([] )
      x_declsout = x_declsin
      x_declscode = ([] )
      x_code = ([] )
visit_Def (C_Arraydecl_1 t_Type t_Name t_INT ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (ppType t_Type) (C_Beside_1 (C_Text_1 " = ") (C_Beside_1 (ppName t_Name) (C_Text_1 (("["++(lrc_INTtoSTR t_INT))++"]")))))
      x_typeerrors = ([] )
      x_outfun = x_infun
      x_errlst = (if (lrc_map_in t_Name x_declsin) then ((:) (C_E_Name_AD_1 t_Name) ([] )) else ([] ))
      x_declsout = (if (lrc_map_in t_Name x_declsin) then x_declsin else (lrc_map_single_update t_Name (C_Consarray_1 t_Type t_INT x_infun) x_declsin))
      x_declscode = ((:) (C_Var_1 (C_Ident_1 ((nametoSTR t_Name)++(lrc_INTtoSTR x_infun))) t_INT t_Type) ([] ))
      x_code = ([] )
visit_Def (C_Declfunc_1 t_Type t_Name t_FormPars t_Stats ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_4 = 1
      x_infun_4 = (x_infun+1)
      x_fid_4 = t_Name
      x_envi_4 = x_env
      x_uu_pp = (func_def_a_la_c (ppType t_Type) (ppName t_Name) x_uu_pp_3 x_uu_pp_4)
      x_typeerrors = x_typeerrors_4
      x_outfun = (x_infun+1)
      x_errlst = (x_err_aux++x_errlst_4)
      x_declsout = (if (lrc_map_in t_Name x_declsin) then x_declsin else (lrc_map_single_update t_Name (C_Consfunc_1 t_Type x_infun x_lst_params_3) x_declsin))
      x_declscode = (((:) (C_Var_1 (C_Ident_1 ((nametoSTR t_Name)++(lrc_INTtoSTR x_infun))) 1 t_Type) (gen_code_func x_infun_4 x_lst_params_3))++x_declscode_4)
      x_code = (((:) (C_ALabel_1 t_Name) x_code_4)++((:) (C_Ret_1 ) []))
      x_error = (if (lrc_map_in t_Name x_declsin) then "" else " <- id already defined!")
      x_err_aux = (if (lrc_map_in t_Name x_declsin) then ((:) (C_E_Name_AD_1 t_Name) x_errlst_3) else x_errlst_3)
      (x_errlst_3 , x_lst_params_3 , x_uu_pp_3 ) = visit_FormPars t_FormPars
      (x_code_4 , x_declscode_4 , x_envo_4 , x_errlst_4 , x_nlabelo_4 , x_typeerrors_4 , x_uu_pp_4 ) = visit_Stats t_Stats x_envi_4 x_fid_4 x_infun_4 x_nlabeli_4
visit_Def (C_Declfunc_header_1 t_Type t_Name t_FormPars t_Stats ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_4 = 1
      x_infun_4 = (x_infun+1)
      x_fid_4 = t_Name
      x_envi_4 = x_env
      x_uu_pp = (C_Empty_1 )
      x_typeerrors = x_typeerrors_4
      x_outfun = (x_infun+1)
      x_errlst = (x_err_aux++x_errlst_4)
      x_declsout = (if (lrc_map_in t_Name x_declsin) then x_declsin else (lrc_map_single_update t_Name (C_Consfunc_1 t_Type x_infun x_lst_params_3) x_declsin))
      x_declscode = (((:) (C_Var_1 (C_Ident_1 ((nametoSTR t_Name)++(lrc_INTtoSTR x_infun))) 1 t_Type) (gen_code_func x_infun_4 x_lst_params_3))++x_declscode_4)
      x_code = (((:) (C_ALabel_1 t_Name) x_code_4)++((:) (C_Ret_1 ) []))
      x_error = (if (lrc_map_in t_Name x_declsin) then "" else " <- id already defined!")
      x_err_aux = (if (lrc_map_in t_Name x_declsin) then ((:) (C_E_Name_AD_1 t_Name) x_errlst_3) else x_errlst_3)
      (x_errlst_3 , x_lst_params_3 , x_uu_pp_3 ) = visit_FormPars t_FormPars
      (x_code_4 , x_declscode_4 , x_envo_4 , x_errlst_4 , x_nlabelo_4 , x_typeerrors_4 , x_uu_pp_4 ) = visit_Stats t_Stats x_envi_4 x_fid_4 x_infun_4 x_nlabeli_4
visit_Def (C_Declfunc_header_novar_1 t_Type t_Name t_FormPars t_Stats ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_4 = 1
      x_infun_4 = (x_infun+1)
      x_fid_4 = t_Name
      x_envi_4 = x_env
      x_uu_pp = (C_Empty_1 )
      x_typeerrors = x_typeerrors_4
      x_outfun = (x_infun+1)
      x_errlst = (x_err_aux++x_errlst_4)
      x_declsout = (if (lrc_map_in t_Name x_declsin) then x_declsin else (lrc_map_single_update t_Name (C_Consfunc_1 t_Type x_infun x_lst_params_3) x_declsin))
      x_declscode = (((:) (C_Var_1 (C_Ident_1 ((nametoSTR t_Name)++(lrc_INTtoSTR x_infun))) 1 t_Type) (gen_code_func x_infun_4 x_lst_params_3))++x_declscode_4)
      x_code = (((:) (C_ALabel_1 t_Name) x_code_4)++((:) (C_Ret_1 ) []))
      x_error = (if (lrc_map_in t_Name x_declsin) then "" else " <- id already defined!")
      x_err_aux = (if (lrc_map_in t_Name x_declsin) then ((:) (C_E_Name_AD_1 t_Name) x_errlst_3) else x_errlst_3)
      (x_errlst_3 , x_lst_params_3 , x_uu_pp_3 ) = visit_FormPars t_FormPars
      (x_code_4 , x_declscode_4 , x_envo_4 , x_errlst_4 , x_nlabelo_4 , x_typeerrors_4 , x_uu_pp_4 ) = visit_Stats t_Stats x_envi_4 x_fid_4 x_infun_4 x_nlabeli_4
visit_Def (C_Vardecl_1 t_Type t_Name ) x_declsin x_env x_infun = (x_code , x_declscode , x_declsout , x_errlst , x_outfun , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (ppType t_Type) (C_Beside_1 (C_Text_1 " = ") (ppName t_Name)))
      x_typeerrors = ([] )
      x_outfun = x_infun
      x_errlst = (if (lrc_map_in t_Name x_declsin) then ((:) (C_E_Name_AD_1 t_Name) ([] )) else ([] ))
      x_declsout = (if (lrc_map_in t_Name x_declsin) then x_declsin else (lrc_map_single_update t_Name (C_Consvar_1 t_Type x_infun) x_declsin))
      x_declscode = ((:) (C_Var_1 (C_Ident_1 ((nametoSTR t_Name)++(lrc_INTtoSTR x_infun))) 1 t_Type) ([] ))
      x_code = ([] )
visit_Type (C_Booltype_1 ) = ()
visit_Type (C_Chartype_1 ) = ()
visit_Type (C_Errortype_1 ) = ()
visit_Type (C_Inttype_1 ) = ()
visit_Type (C_Realtype_1 ) = ()
visit_Name (C_Ident_1 t_STR ) = ()
visit_FormPars (C_Emptyformpars_1 ) = (x_errlst , x_lst_params , x_uu_pp )
  where
      x_uu_pp = (C_Text_1 "")
      x_lst_params = ([] )
      x_errlst = ([] )
visit_FormPars (C_Lstformpars_1 t_FormPar t_FormPars_2 ) = (x_errlst , x_lst_params , x_uu_pp )
  where
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 ",") x_uu_pp_2))
      x_lst_params = (x_one_param_1++x_lst_params_2)
      x_errlst = (if (isinlst x_parname_1 x_lst_params_2) then ((:) (C_E_FormParam_AD_1 x_parname_1) x_errlst_2) else x_errlst_2)
      (x_one_param_1 , x_parname_1 , x_uu_pp_1 ) = visit_FormPar t_FormPar
      (x_errlst_2 , x_lst_params_2 , x_uu_pp_2 ) = visit_FormPars t_FormPars_2
visit_FormPar (C_Declformpar_1 t_Type t_Name ) = (x_one_param , x_parname , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (ppType t_Type) (C_Beside_1 (C_Text_1 " ") (ppName t_Name)))
      x_parname = t_Name
      x_one_param = ((:) (C_AParam_1 t_Type t_Name) ([] ))
visit_Stats (C_Emptystat_1 ) x_envi x_fid x_infun x_nlabeli = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Empty_1 )
      x_typeerrors = ([] )
      x_nlabelo = x_nlabeli
      x_errlst = ([] )
      x_envo = x_envi
      x_declscode = ([] )
      x_code = []
visit_Stats (C_Lststats_1 t_Stat t_Stats_2 ) x_envi x_fid x_infun x_nlabeli = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_2 = x_nlabelo_1
      x_infun_2 = x_infun
      x_fid_2 = x_fid
      x_envi_2 = x_envo_1
      x_uu_pp = (C_Above_1 (C_Beside_1 x_uu_pp_1 (C_Text_1 ";")) x_uu_pp_2)
      x_typeerrors = (x_typeerrors_1++x_typeerrors_2)
      x_nlabelo = x_nlabelo_2
      x_errlst = (x_errlst_1++x_errlst_2)
      x_envo = x_envo_2
      x_declscode = (x_declscode_1++x_declscode_2)
      x_code = (x_code_1++x_code_2)
      x_upw_Stats_infun_1 = x_infun
      x_nlabeli_1 = x_nlabeli
      x_infun_1 = x_infun
      x_fid_1 = x_fid
      x_envi_1 = x_envi
      (x_code_1 , x_declscode_1 , x_envo_1 , x_errlst_1 , x_nlabelo_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Stat t_Stat x_envi_1 x_fid_1 x_infun_1 x_nlabeli_1 x_upw_Stats_infun_1
      (x_code_2 , x_declscode_2 , x_envo_2 , x_errlst_2 , x_nlabelo_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Stats t_Stats_2 x_envi_2 x_fid_2 x_infun_2 x_nlabeli_2
visit_Stat (C_ArrAssign_1 t_ArrayUse t_Exp ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 x_uu_pp_1 (C_Beside_1 (C_Text_1 "[") (C_Beside_1 x_uu_pp_2 (C_Beside_1 (C_Text_1 "] = ") x_uu_pp_2))))
      x_typeerrors = (if (typecheck x_an_1 x_type_2 x_fid x_envi) then x_typeerrors_2 else ((:) (C_E_T_DT_1 x_an_1) x_typeerrors_2))
      x_nlabelo = x_nlabeli
      x_errlst = (x_errlst_1++x_errlst_2)
      x_envo = x_envi
      x_declscode = ([] )
      x_code = (x_code_1++x_code_2)
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_envi
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_envi
      (x_an_1 , x_code_1 , x_errlst_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_ArrayUse t_ArrayUse x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Stat (C_Assign_1 t_Name t_Exp ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (ppName t_Name) (C_Beside_1 (C_Text_1 " = ") x_uu_pp_2))
      x_typeerrors = (if (x_typerr==(C_NoTypeError_1 )) then x_typeerrors_2 else ((:) x_typerr x_typeerrors_2))
      x_nlabelo = x_nlabeli
      x_errlst = (if (isinenv t_Name x_fid x_envi) then x_errlst_2 else ((:) (C_E_Name_ND_1 t_Name) x_errlst_2))
      x_envo = x_envi
      x_declscode = ([] )
      x_code = (((:) (C_Pusha_1 t_Name x_infun) x_code_2)++((:) (C_Store_1 ) ([] )))
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_envi
      x_typerr = (assign_InfType t_Name x_type_2 x_fid x_envi)
      x_error = (if (isinenv t_Name x_fid x_envi) then ([] ) else ((:) (C_E_Name_ND_1 t_Name) ([] )))
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Stat (C_Funccall_1 t_Name t_ActPars ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (funccall (ppName t_Name) x_uu_pp_2)
      x_typeerrors = (checkactparams t_Name x_act_params_2 x_envi)
      x_nlabelo = x_nlabeli
      x_errlst = (if (lrc_map_in t_Name x_envi) then x_errlst_2 else ((:) (C_E_Fun_ND_1 t_Name) x_errlst_2))
      x_envo = x_envi
      x_declscode = ([] )
      x_code = (gen_code_func_inv t_Name x_envi x_code_2)
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_envi
      x_error = (if (lrc_map_in t_Name x_envi) then ([] ) else ((:) (C_E_Fun_ND_1 t_Name) ([] )))
      (x_act_params_2 , x_code_2 , x_errlst_2 , x_uu_pp_2 ) = visit_ActPars t_ActPars x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Stat (C_If_t_e_1 t_Exp t_Stats t_Stats_2 ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_3 = (x_nlabeli+1)
      x_infun_3 = x_infun
      x_fid_3 = x_fid
      x_envi_3 = x_envi
      x_nlabeli_2 = x_nlabelo_3
      x_infun_2 = x_infun
      x_fid_2 = x_fid
      x_envi_2 = x_envi
      x_uu_pp = (if_t_ePP x_uu_pp_1 x_uu_pp_2 x_uu_pp_3)
      x_typeerrors = (if (x_typerr==(C_NoTypeError_1 )) then ((x_typeerrors_1++x_typeerrors_2)++x_typeerrors_3) else ((:) x_typerr ((x_typeerrors_1++x_typeerrors_2)++x_typeerrors_3)))
      x_nlabelo = x_nlabelo_2
      x_errlst = ((x_errlst_1++x_errlst_2)++x_errlst_3)
      x_envo = x_envi
      x_declscode = (x_declscode_2++x_declscode_3)
      x_code = (gen_code_ite x_nlabeli x_code_1 x_code_2 x_code_3)
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_envi
      x_typerr = (if (isboolexp x_type_1) then (C_NoTypeError_1 ) else (C_E_T_if_t_e_1 ))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_declscode_2 , x_envo_2 , x_errlst_2 , x_nlabelo_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Stats t_Stats x_envi_2 x_fid_2 x_infun_2 x_nlabeli_2
      (x_code_3 , x_declscode_3 , x_envo_3 , x_errlst_3 , x_nlabelo_3 , x_typeerrors_3 , x_uu_pp_3 ) = visit_Stats t_Stats_2 x_envi_3 x_fid_3 x_infun_3 x_nlabeli_3
visit_Stat (C_Input_1 t_Name ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (C_Text_1 "input ") (ppName t_Name))
      x_typeerrors = ([] )
      x_nlabelo = x_nlabeli
      x_errlst = (if (isinenv t_Name x_fid x_envi) then ([] ) else ((:) (C_E_Name_ND_1 t_Name) ([] )))
      x_envo = x_envi
      x_declscode = ([] )
      x_code = ((:) (C_Pusha_1 t_Name x_upw_Stats_infun) ((:) (C_IIn_1 ) ((:) (C_Store_1 ) ([] ))))
visit_Stat (C_LocalDecl_1 t_Type t_Name ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (ppType t_Type) (C_Beside_1 (C_Text_1 " ") (ppName t_Name)))
      x_typeerrors = ([] )
      x_nlabelo = x_nlabeli
      x_errlst = (if (lrc_map_in t_Name x_envi) then ((:) (C_E_Loc_Name_AD_1 t_Name) ([] )) else ([] ))
      x_envo = (if (lrc_map_in t_Name x_envi) then x_envi else (lrc_map_single_update t_Name (C_Consvar_1 t_Type x_infun) x_envi))
      x_declscode = ((:) (C_Var_1 t_Name x_infun t_Type) ([] ))
      x_code = ([] )
visit_Stat (C_Print_1 t_Exp ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Beside_1 (C_Text_1 "print ") x_uu_pp_1)
      x_typeerrors = x_typeerrors_1
      x_nlabelo = x_nlabeli
      x_errlst = x_errlst_1
      x_envo = x_envi
      x_declscode = ([] )
      x_code = (x_code_1++((:) (C_IOut_1 ) ([] )))
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_envi
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Stat (C_While_1 t_Exp t_Stats ) x_envi x_fid x_infun x_nlabeli x_upw_Stats_infun = (x_code , x_declscode , x_envo , x_errlst , x_nlabelo , x_typeerrors , x_uu_pp )
  where
      x_nlabeli_2 = (x_nlabeli+1)
      x_infun_2 = x_infun
      x_fid_2 = x_fid
      x_envi_2 = x_envi
      x_uu_pp = (whilePP (C_Text_1 "while ") x_uu_pp_1 x_uu_pp_2)
      x_typeerrors = (if (x_typerr==(C_NoTypeError_1 )) then (x_typeerrors_1++x_typeerrors_2) else ((:) x_typerr (x_typeerrors_1++x_typeerrors_2)))
      x_nlabelo = x_nlabelo_2
      x_errlst = (x_errlst_1++x_errlst_2)
      x_envo = x_envi
      x_declscode = x_declscode_2
      x_code = (gen_code_while x_nlabeli x_code_1 x_code_2)
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_envi
      x_typerr = (if (isboolexp x_type_1) then (C_NoTypeError_1 ) else (C_E_T_while_1 ))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_declscode_2 , x_envo_2 , x_errlst_2 , x_nlabelo_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Stats t_Stats x_envi_2 x_fid_2 x_infun_2 x_nlabeli_2
visit_ArrayUse (C_ArrayInd_1 t_Name t_Exp ) x_env x_fid x_upw_Stats_infun = (x_an , x_code , x_errlst , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_uu_pp = (C_Beside_1 (ppName t_Name) (C_Beside_1 (C_Text_1 "[") (C_Beside_1 x_uu_pp_2 (C_Text_1 "]"))))
      x_typeerrors = (if (isinttype x_type_2) then x_typeerrors_2 else ((:) (C_E_T_IndArrNotInt_1 ) x_typeerrors_2))
      x_errlst = (if x_noerror then x_errlst_2 else ((:) (C_E_Name_ND_1 t_Name) x_errlst_2))
      x_code = x_code_2
      x_an = t_Name
      x_noerror = (isinenv t_Name x_fid x_env)
      x_error = (if x_noerror then ([] ) else ((:) (C_E_Name_ND_1 t_Name) ([] )))
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_AddExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " + ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 1 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Add_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_AndExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " && ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 2 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_And_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_DivExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " / ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 1 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Div_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_EqExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " == ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 2 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Eq_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_Factor_1 t_Fac ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = x_uu_pp_1
      x_typeerrors = x_typeerrors_1
      x_type = x_type_1
      x_errlst = x_errlst_1
      x_code = x_code_1
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Fac t_Fac x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Exp (C_GTExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " > ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 2 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Gt_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_LTExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " < ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 2 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Lt_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_MinExp_1 t_Exp_2 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (C_Beside_1 (C_Text_1 " - ") x_uu_pp_1)
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_NotArithExp_1 ) x_typeerrors_1) else x_typeerrors_1)
      x_type = (if (isaritmexp x_type_1) then x_type_1 else (C_Errortype_1 ))
      x_errlst = x_errlst_1
      x_code = (x_code_1++((:) (C_Minus_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Exp (C_MulExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " * ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 1 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Mul_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_NotExp_1 t_Exp_2 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (C_Beside_1 (C_Text_1 " ! ") x_uu_pp_1)
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_NotBooleanExp_1 ) x_typeerrors_1) else x_typeerrors_1)
      x_type = (if (isboolexp x_type_1) then x_type_1 else (C_Errortype_1 ))
      x_errlst = x_errlst_1
      x_code = (x_code_1++((:) (C_Not_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Exp (C_OrExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " || ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 2 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Or_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Exp (C_SubExp_1 t_Exp_2 t_Exp_3 ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_uu_pp = (hv x_uu_pp_1 (C_Beside_1 (C_Text_1 " - ") x_uu_pp_2))
      x_typeerrors = (if (x_type==(C_Errortype_1 )) then ((:) (C_E_T_BOP_1 ) (x_typeerrors_1++x_typeerrors_2)) else (x_typeerrors_1++x_typeerrors_2))
      x_type = (infType 1 x_type_1 x_type_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((x_code_1++x_code_2)++((:) (C_Sub_1 ) ([] )))
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp_2 x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_code_2 , x_errlst_2 , x_type_2 , x_typeerrors_2 , x_uu_pp_2 ) = visit_Exp t_Exp_3 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Fac (C_ArrayConst_1 t_ArrayUse ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = x_uu_pp_1
      x_typeerrors = x_typeerrors_1
      x_type = (gettype x_an_1 x_fid x_env)
      x_errlst = x_errlst_1
      x_code = x_code_1
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      (x_an_1 , x_code_1 , x_errlst_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_ArrayUse t_ArrayUse x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Fac (C_BoolConst_1 t_BOOL ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Text_1 (if t_BOOL then "true" else "false") )
      x_typeerrors = ([] )
      x_type = (C_Booltype_1 )
      x_errlst = ([] )
      x_code = ((:) (C_Pushb_1 t_BOOL) ([] ))
visit_Fac (C_CNIdent_1 t_Name ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (ppName t_Name)
      x_typeerrors = ([] )
      x_type = (gettype t_Name x_fid x_env)
      x_errlst = (if (isinenv t_Name x_fid x_env) then ([] ) else ((:) (C_E_Name_ND_1 t_Name) ([] )))
      x_code = ((:) (C_Pusha_1 t_Name x_upw_Stats_infun) ((:) (C_Load_1 ) ([] )))
visit_Fac (C_Expr_1 t_Exp ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = x_uu_pp_1
      x_typeerrors = x_typeerrors_1
      x_type = x_type_1
      x_errlst = x_errlst_1
      x_code = x_code_1
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp x_env_1 x_fid_1 x_upw_Stats_infun_1
visit_Fac (C_Funcinv_1 t_Name t_ActPars ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (funccall (ppName t_Name) x_uu_pp_2)
      x_typeerrors = (checkactparams t_Name x_act_params_2 x_env)
      x_type = (gettype t_Name x_fid x_env)
      x_errlst = (x_error++x_errlst_2)
      x_code = (gen_code_func_inv t_Name x_env x_code_2)
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_noerror = (lrc_map_in t_Name x_env)
      x_error = (if x_noerror then ([] ) else ((:) (C_E_Fun_ND_1 t_Name) ([] )))
      (x_act_params_2 , x_code_2 , x_errlst_2 , x_uu_pp_2 ) = visit_ActPars t_ActPars x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_Fac (C_IntConst_1 t_INT ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Text_1 (lrc_INTtoSTR t_INT))
      x_typeerrors = ([] )
      x_type = (C_Inttype_1 )
      x_errlst = ([] )
      x_code = ((:) (C_Pushi_1 t_INT) ([] ))
visit_Fac (C_RealConst_1 t_REAL ) x_env x_fid x_upw_Stats_infun = (x_code , x_errlst , x_type , x_typeerrors , x_uu_pp )
  where
      x_uu_pp = (C_Text_1 (lrc_REALtoSTR t_REAL))
      x_typeerrors = ([] )
      x_type = (C_Realtype_1 )
      x_errlst = ([] )
      x_code = ((:) (C_Pushr_1 t_REAL) ([] ))
visit_ActPars (C_Emptyactpars_1 ) x_env x_fid x_upw_Stats_infun = (x_act_params , x_code , x_errlst , x_uu_pp )
  where
      x_uu_pp = (C_Empty_1 )
      x_errlst = ([] )
      x_code = ([] )
      x_act_params = ([] )
visit_ActPars (C_Lstactpars_1 t_Exp t_ActPars_2 ) x_env x_fid x_upw_Stats_infun = (x_act_params , x_code , x_errlst , x_uu_pp )
  where
      x_upw_Stats_infun_1 = x_upw_Stats_infun
      x_fid_1 = x_fid
      x_env_1 = x_env
      x_upw_Stats_infun_2 = x_upw_Stats_infun
      x_fid_2 = x_fid
      x_env_2 = x_env
      x_uu_pp = (hv x_uu_pp_1 x_uu_pp_2)
      x_errlst = (x_errlst_1++x_errlst_2)
      x_code = ((:) x_code_1 x_code_2)
      x_act_params = ((:) x_type_1 x_act_params_2)
      (x_code_1 , x_errlst_1 , x_type_1 , x_typeerrors_1 , x_uu_pp_1 ) = visit_Exp t_Exp x_env_1 x_fid_1 x_upw_Stats_infun_1
      (x_act_params_2 , x_code_2 , x_errlst_2 , x_uu_pp_2 ) = visit_ActPars t_ActPars_2 x_env_2 x_fid_2 x_upw_Stats_infun_2
visit_PPRoot (C_All_1 t_PPS ) x_pw = (x_fmts )
  where
      x_frame_1 = (C_F_1 x_pw x_pw)
      x_fmts = (eq_all_fmts x_pw x_fmts_1)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS x_frame_1
visit_PPRoot (C_Best_1 t_PPS ) x_pw = (x_fmts )
  where
      x_frame_1 = (C_F_1 x_pw x_pw)
      x_fmts = (eq_best_fmts x_pw x_fmts_1)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS x_frame_1
visit_PPS :: PPS -> T_Frame -> (Bool, T_Formats, INT, Integer, Integer)
visit_PPS (C_Above_1 t_PPS_2 t_PPS_3 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_2 = x_frame
      x_frame_1 = x_frame
      x_minw = (max x_minw_1 x_minw_2)
      x_minll = x_minll_2
      x_maxh = (x_maxh_1+x_maxh_2)
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = ((x_error_1||x_error_2)||x_aerror)
      x_fe = (set_fmts_above x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2)
      x_aerror = (snd_Pair_T_Formats x_fe)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS_2 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_PPS t_PPS_3 x_frame_2
visit_PPS (C_Apply_1 t_PPC t_PPSArgs ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_reqs_2 = x_reqs_1
      x_minw = (set_var_apply x_error_cond x_lem x_minw_1)
      x_minll = (set_var_apply x_error_cond x_lem x_minll_1)
      x_maxh = (set_var_apply x_error_cond 1 x_maxh_1)
      x_fmts = (eq_set_fmts_apply x_error_cond x_error_msg x_fmts_1)
      x_error = x_l_error
      x_frame_1 = x_frame
      x_fillmins_1 = x_mins_2
      x_fillfmts_1 = x_fmts_2
      x_fillerrs_1 = x_error_2
      x_lem = (fromIntegral (length x_error_msg))
      x_l_error = (if x_error_cond then True else x_error_1)
      x_error_msg = (set_error_msg x_numpars_1 x_len_2)
      x_error_cond = (x_numpars_1/=x_len_2)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fmts_2 , x_len_2 , x_mins_2 ) = visit_PPSArgs t_PPSArgs x_reqs_2
visit_PPS (C_Beside_1 t_PPS_2 t_PPS_3 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_2 = (narrow_frame x_minll_1 x_frame)
      x_frame_1 = (narrow_ll x_minw_2 x_frame)
      x_minw = (max x_minw_1 (x_minll_1+x_minw_2))
      x_minll = (x_minll_1+x_minll_2)
      x_maxh = (beside_height x_maxh_1 x_maxh_2)
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = (x_l_error||x_berror)
      x_l_error = (x_error_1||x_error_2)
      x_fe = (set_fmts_beside x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2 x_frame x_l_error)
      x_berror = (snd_Pair_T_Formats x_fe)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS_2 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_PPS t_PPS_3 x_frame_2
visit_PPS (C_Dup_1 t_PPS_2 t_PPS_3 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_2 = x_frame
      x_frame_1 = x_frame
      x_minw = x_l_minw
      x_minll = (min x_minll_1 x_minll_2)
      x_maxh = (max x_maxh_1 x_maxh_2)
      x_fmts = (sem_fmts_dup x_fmts_1 x_fmts_2 x_error_1 x_error_2 x_l_minw)
      x_error = x_l_error
      x_l_minw = (min x_minw_1 x_minw_2)
      x_l_error = (x_error_1 && x_error_2)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS_2 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_PPS t_PPS_3 x_frame_2
visit_PPS (C_Empty_1 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_minw = 0
      x_minll = 0
      x_maxh = 0
      x_fmts = (set_fmts_empty )
      x_error = False
visit_PPS (C_FillBlock_1 t_INT t_FillList ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_minw = x_minw_2
      x_minll = x_minll_2
      x_maxh = x_maxh_2
      x_fmts = (set_fmts_fillblock t_INT x_fmts_2 x_l_pw)
      x_error = (((t_INT<0)||(t_INT>x_l_pw))||x_error_2)
      x_pw_2 = t_INT
      x_minwi_2 = 0
      x_minlli_2 = 0
      x_maxhi_2 = 0
      x_frame_2 = (C_F_1 t_INT t_INT)
      x_fmtsi_2 = (empty_fmts )
      x_errori_2 = False
      x_l_pw = case x_frame of { (C_F_1 w _) -> w }
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_FillList t_FillList x_errori_2 x_fmtsi_2 x_frame_2 x_maxhi_2 x_minlli_2 x_minwi_2 x_pw_2
visit_PPS (C_Filla_1 t_FillList ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_minw = x_minw_1
      x_minll = x_minll_1
      x_maxh = x_maxh_1
      x_fmts = (eq_set_fmts_fill x_fmts_1)
      x_error = x_error_1
      x_pw_1 = x_l_pw
      x_minwi_1 = 0
      x_minlli_1 = 0
      x_maxhi_1 = 0
      x_frame_1 = (C_F_1 x_l_pw x_l_pw)
      x_fmtsi_1 = (empty_fmts )
      x_errori_1 = False
      x_l_pw = case x_frame of { (C_F_1 w _) -> w }
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_FillList t_FillList x_errori_1 x_fmtsi_1 x_frame_1 x_maxhi_1 x_minlli_1 x_minwi_1 x_pw_1
visit_PPS (C_Indent_1 t_INT t_PPS_2 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_2 = (narrow_frame t_INT x_frame)
      x_minw = x_l_minw
      x_minll = (t_INT+x_minll_2)
      x_maxh = x_maxh_2
      x_fmts = (set_fmts_indent t_INT x_fmts_2 x_pw x_l_minw x_frame x_error_2)
      x_error = (((t_INT<0)||(t_INT>x_pw))||x_error_2)
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_l_minw = (t_INT+x_minw_2)
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_PPS t_PPS_2 x_frame_2
visit_PPS (C_Join_1 t_PPS_2 ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_1 = x_frame
      x_minw = x_minw_1
      x_minll = x_minll_1
      x_maxh = x_maxh_1
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = (x_error_1||x_jerror)
      x_jerror = (snd_Pair_T_Formats x_fe)
      x_fe = (set_fmts_join x_fmts_1 x_error_1)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS_2 x_frame_1
visit_PPS (C_Text_1 t_STR ) x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_minw = x_l_minw
      x_minll = x_l_minw
      x_maxh = 1
      x_fmts = (set_fmts_text t_STR x_l_minw x_l_error)
      x_error = x_l_error
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_l_minw = (fromIntegral (length t_STR))
      x_l_error = (x_l_minw>x_pw)
visit_PPC (C_AboveC_1 t_PPC_2 t_PPC_3 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_frame_2 = x_frame
      x_fillmins_2 = (snd_Pair_Lst_T_Mins x_i)
      x_fillfmts_2 = (snd_Pair_Lst_T_Fmts x_m)
      x_fillerrs_2 = (snd_Pair_Lst_T_Errs x_e)
      x_frame_1 = x_frame
      x_fillmins_1 = (fst_Pair_Lst_T_Mins x_i)
      x_fillfmts_1 = (fst_Pair_Lst_T_Fmts x_m)
      x_fillerrs_1 = (fst_Pair_Lst_T_Errs x_e)
      x_reqs = (x_reqs_1++x_reqs_2)
      x_numpars = (x_numpars_1+x_numpars_2)
      x_minw = (max x_minw_1 x_minw_2)
      x_minll = x_minll_2
      x_maxh = (x_maxh_1+x_maxh_2)
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = ((x_error_1||x_error_2)||x_aerror)
      x_m = (splitAt_T_Fmts (fromIntegral x_numpars_1) x_fillfmts)
      x_i = (splitAt_T_Mins (fromIntegral x_numpars_1) x_fillmins)
      x_fe = (set_fmts_above x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2)
      x_e = (splitAt_T_Errs (fromIntegral x_numpars_1) x_fillerrs)
      x_aerror = (snd_Pair_T_Formats x_fe)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC_2 x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 , x_numpars_2 , x_reqs_2 ) = visit_PPC t_PPC_3 x_fillerrs_2 x_fillfmts_2 x_fillmins_2 x_frame_2
visit_PPC (C_ApplyC_1 t_PPC_2 t_PPCArgs ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_ireqs_2 = x_reqs_1
      x_ifillmins_2 = x_fillmins
      x_ifillfmts_2 = x_fillfmts
      x_ifillerrs_2 = x_fillerrs
      x_frame_1 = x_frame
      x_fillmins_1 = x_fillmins_2
      x_fillfmts_1 = x_fmts_2
      x_fillerrs_1 = x_error_2
      x_reqs = x_reqs_2
      x_numpars = x_numpars_2
      x_minw = (set_var_apply x_error_cond x_lem x_minw_1)
      x_minll = (set_var_apply x_error_cond x_lem x_minll_1)
      x_maxh = (set_var_apply x_error_cond 1 x_maxh_1)
      x_fmts = (eq_set_fmts_apply x_error_cond x_error_msg x_fmts_1)
      x_error = x_l_error
      x_lem = (fromIntegral (length x_error_msg))
      x_l_error = (if x_error_cond then True else x_error_1)
      x_error_msg = (set_error_msg x_numpars_1 x_len_2)
      x_error_cond = (x_numpars_2/=x_len_2)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC_2 x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fillmins_2 , x_fmts_2 , x_len_2 , x_numpars_2 , x_reqs_2 ) = visit_PPCArgs t_PPCArgs x_ifillerrs_2 x_ifillfmts_2 x_ifillmins_2 x_ireqs_2
visit_PPC (C_BesideC_1 t_PPC_2 t_PPC_3 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_frame_2 = (narrow_frame x_minll_1 x_frame)
      x_fillmins_2 = (snd_Pair_Lst_T_Mins x_i)
      x_fillfmts_2 = (snd_Pair_Lst_T_Fmts x_m)
      x_fillerrs_2 = (snd_Pair_Lst_T_Errs x_e)
      x_frame_1 = (narrow_ll x_minw_2 x_frame)
      x_fillmins_1 = (fst_Pair_Lst_T_Mins x_i)
      x_fillfmts_1 = (fst_Pair_Lst_T_Fmts x_m)
      x_fillerrs_1 = (fst_Pair_Lst_T_Errs x_e)
      x_reqs = (x_reqs_1++x_reqs_2)
      x_numpars = (x_numpars_1+x_numpars_2)
      x_minw = (max x_minw_1 (x_minll_1+x_minw_2))
      x_minll = (x_minll_1+x_minll_2)
      x_maxh = (beside_height x_maxh_1 x_maxh_2)
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = (x_l_error||x_berror)
      x_m = (splitAt_T_Fmts (fromIntegral x_numpars_1) x_fillfmts)
      x_l_error = (x_error_1||x_error_2)
      x_i = (splitAt_T_Mins (fromIntegral x_numpars_1) x_fillmins)
      x_fe = (set_fmts_beside x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2 x_frame x_l_error)
      x_e = (splitAt_T_Errs (fromIntegral x_numpars_1) x_fillerrs)
      x_berror = (snd_Pair_T_Formats x_fe)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC_2 x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 , x_numpars_2 , x_reqs_2 ) = visit_PPC t_PPC_3 x_fillerrs_2 x_fillfmts_2 x_fillmins_2 x_frame_2
visit_PPC (C_DupC_1 t_PPC_2 t_PPC_3 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_frame_2 = x_frame
      x_fillmins_2 = x_fillmins
      x_fillfmts_2 = x_fillfmts
      x_fillerrs_2 = x_fillerrs
      x_frame_1 = x_frame
      x_fillmins_1 = x_fillmins
      x_fillfmts_1 = x_fillfmts
      x_fillerrs_1 = x_fillerrs
      x_reqs = (eq_DupC x_reqs_1 x_reqs_2)
      x_numpars = x_numpars_1
      x_minw = x_l_minw
      x_minll = (min x_minll_1 x_minll_2)
      x_maxh = (max x_maxh_1 x_maxh_2)
      x_fmts = (sem_fmts_cdup x_fmts_1 x_fmts_2 x_error_1 x_error_2 x_numpars_1 x_numpars_2 x_l_minw x_error_msg)
      x_error = x_l_error
      x_l_minw = (min x_minw_1 x_minw_2)
      x_l_error = ((x_numpars_1/=x_numpars_2)||(x_error_1 && x_error_2))
      x_error_msg = (eq_set_error_msg x_numpars_1 x_numpars_2)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC_2 x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 , x_numpars_2 , x_reqs_2 ) = visit_PPC t_PPC_3 x_fillerrs_2 x_fillfmts_2 x_fillmins_2 x_frame_2
visit_PPC (C_IndentC_1 t_INT t_PPC_2 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_frame_2 = (narrow_frame t_INT x_frame)
      x_fillmins_2 = x_fillmins
      x_fillfmts_2 = x_fillfmts
      x_fillerrs_2 = x_fillerrs
      x_reqs = x_reqs_2
      x_numpars = x_numpars_2
      x_minw = x_l_minw
      x_minll = (t_INT+x_minll_2)
      x_maxh = x_maxh_2
      x_fmts = (set_fmts_indent t_INT x_fmts_2 x_pw x_l_minw x_frame x_error_2)
      x_error = (((t_INT<0)||(t_INT>x_pw))||x_error_2)
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_l_minw = (t_INT+x_minw_2)
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 , x_numpars_2 , x_reqs_2 ) = visit_PPC t_PPC_2 x_fillerrs_2 x_fillfmts_2 x_fillmins_2 x_frame_2
visit_PPC (C_JoinC_1 t_PPC_2 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_frame_1 = x_frame
      x_fillmins_1 = x_fillmins
      x_fillfmts_1 = x_fillfmts
      x_fillerrs_1 = x_fillerrs
      x_reqs = x_reqs_1
      x_numpars = x_numpars_1
      x_minw = x_minw_1
      x_minll = x_minll_1
      x_maxh = x_maxh_1
      x_fmts = (fst_Pair_T_Formats x_fe)
      x_error = (x_error_1||x_jerror)
      x_jerror = (snd_Pair_T_Formats x_fe)
      x_fe = (set_fmts_join x_fmts_1 x_error_1)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC_2 x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
visit_PPC (C_ParC_1 ) x_fillerrs x_fillfmts x_fillmins x_frame = (x_error , x_fmts , x_maxh , x_minll , x_minw , x_numpars , x_reqs )
  where
      x_reqs = ((:) x_frame ([] ))
      x_numpars = 1
      x_minw = (fst_t x_l_m)
      x_minll = (snd_t x_l_m)
      x_maxh = (third_t x_l_m)
      x_fmts = (head_T_Fmts x_fillfmts)
      x_error = x_l_error
      x_l_m = (head_T_Mins x_fillmins)
      x_l_error = (head_T_Errs x_fillerrs)
visit_PPCArgs (C_ConsPPCArgs_1 t_PPC t_PPCArgs_2 ) x_ifillerrs x_ifillfmts x_ifillmins x_ireqs = (x_error , x_fillmins , x_fmts , x_len , x_numpars , x_reqs )
  where
      x_ireqs_2 = (tail_T_Reqs x_ireqs)
      x_ifillmins_2 = (snd_Pair_Lst_T_Mins x_i)
      x_ifillfmts_2 = (snd_Pair_Lst_T_Fmts x_m)
      x_ifillerrs_2 = (snd_Pair_Lst_T_Errs x_e)
      x_reqs = (x_reqs_1++x_reqs_2)
      x_numpars = (x_numpars_1+x_numpars_2)
      x_len = (x_len_2+1)
      x_fmts = ((:) x_fmts_1 x_fmts_2)
      x_fillmins = ((:) (C_Triple_1 x_minw_1 x_minll_1 x_maxh_1) x_fillmins_2)
      x_error = ((:) x_error_1 x_error_2)
      x_frame_1 = (head_T_Reqs x_ireqs)
      x_fillmins_1 = (fst_Pair_Lst_T_Mins x_i)
      x_fillfmts_1 = (fst_Pair_Lst_T_Fmts x_m)
      x_fillerrs_1 = (fst_Pair_Lst_T_Errs x_e)
      x_m = (splitAt_T_Fmts (fromIntegral x_numpars_1) x_ifillfmts)
      x_i = (splitAt_T_Mins (fromIntegral x_numpars_1) x_ifillmins)
      x_e = (splitAt_T_Errs (fromIntegral x_numpars_1) x_ifillerrs)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 , x_numpars_1 , x_reqs_1 ) = visit_PPC t_PPC x_fillerrs_1 x_fillfmts_1 x_fillmins_1 x_frame_1
      (x_error_2 , x_fillmins_2 , x_fmts_2 , x_len_2 , x_numpars_2 , x_reqs_2 ) = visit_PPCArgs t_PPCArgs_2 x_ifillerrs_2 x_ifillfmts_2 x_ifillmins_2 x_ireqs_2
visit_PPCArgs (C_NilPPCArgs_1 ) x_ifillerrs x_ifillfmts x_ifillmins x_ireqs = (x_error , x_fillmins , x_fmts , x_len , x_numpars , x_reqs )
  where
      x_reqs = ([] )
      x_numpars = 0
      x_len = 0
      x_fmts = ([] )
      x_fillmins = ([] )
      x_error = ([] )


visit_PPSArgs :: PPSArgs -> [T_Frame] -> (T_Errs, T_Fmts, Integer, T_Mins)
-- This function's most general type is
-- visit_PPSArgs :: forall a. (Integral a, Show a)
--               => PPSArgs -> [T_Frame] -> (T_Errs, T_Fmts, a, T_Mins)
-- But in the same mutually recusive group is visit_PPS whose type becomes
-- visit_PPS :: forall a. (Integral a, Show a)
--           => PPS -> T_Frame -> (Bool, T_Formats, INT, INT, INT)
-- which GHC now (rightfully) rejects that as ambiguous, even though
-- acutally default resolution will allow it at call sites,
-- So I've added a type signature 
-- SLPJ July 2012

visit_PPSArgs (C_ConsArgs_1 t_PPS t_PPSArgs_2 ) x_reqs = (x_error , x_fmts , x_len , x_mins )
  where
      x_reqs_2 = (tail_T_Reqs x_reqs)
      x_mins = ((:) (C_Triple_1 x_minw_1 x_minll_1 x_maxh_1) x_mins_2)
      x_len = (x_len_2+1)
      x_fmts = ((:) x_fmts_1 x_fmts_2)
      x_error = ((:) x_error_1 x_error_2)
      x_frame_1 = (head_T_Reqs x_reqs)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS x_frame_1
      (x_error_2 , x_fmts_2 , x_len_2 , x_mins_2 ) = visit_PPSArgs t_PPSArgs_2 x_reqs_2
visit_PPSArgs (C_NilArgs_1 ) x_reqs = (x_error , x_fmts , x_len , x_mins )
  where
      x_mins = ([] )
      x_len = 0
      x_fmts = ([] )
      x_error = ([] )
visit_FillList (C_ConsFillList_1 t_PPS t_FillList_2 ) x_errori x_fmtsi x_frame x_maxhi x_minlli x_minwi x_pw = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_frame_1 = x_frame
      x_pw_2 = x_pw
      x_minwi_2 = x_minwi
      x_minlli_2 = ((x_minlli+x_minwi)+x_minw_1)
      x_maxhi_2 = (cons_height x_maxh_1 x_maxhi True)
      x_frame_2 = x_frame
      x_fmtsi_2 = (fst_Pair_Formats x_fe)
      x_errori_2 = (x_errori||x_ferror)
      x_minw = x_minw_2
      x_minll = x_minll_2
      x_maxh = x_maxh_2
      x_fmts = x_fmts_2
      x_error = (x_error_2||x_error_1)
      x_newll = (x_minlli+x_minw_1)
      x_ferror = (snd_Pair_Formats x_fe)
      x_fe = (set_fmts_filllist x_fmtsi x_fmts_1 x_maxhi x_maxh_1 x_frame x_avail)
      x_avail = ((x_pw-x_newll)>=0)
      (x_error_1 , x_fmts_1 , x_maxh_1 , x_minll_1 , x_minw_1 ) = visit_PPS t_PPS x_frame_1
      (x_error_2 , x_fmts_2 , x_maxh_2 , x_minll_2 , x_minw_2 ) = visit_FillList t_FillList_2 x_errori_2 x_fmtsi_2 x_frame_2 x_maxhi_2 x_minlli_2 x_minwi_2 x_pw_2
visit_FillList (C_NilFillList_1 ) x_errori x_fmtsi x_frame x_maxhi x_minlli x_minwi x_pw = (x_error , x_fmts , x_maxh , x_minll , x_minw )
  where
      x_minw = x_minwi
      x_minll = x_minlli
      x_maxh = x_maxhi
      x_fmts = x_fmtsi
      x_error = x_errori
above_fmt u l =
 case u of { (C_Elem_1 uh ul uw ut) -> case l of { (C_Elem_1 lh ll lw lt) -> (C_Elem_1 (uh+lh) ll (max uw lw) (ut++lt)) } }
above_fmts u l =
 case u of { ( [] ) -> ([] ) ; ( (:) u2 us ) -> case l of { ( [] ) -> ([] ) ; ( (:) l2 ls ) -> let { (utw) = (total_w u2) ; (ltw) = (total_w l2) ; (fe) = (above_fmt u2 l2) } in (if (utw>=ltw) then ((:) fe (above_fmts us l)) else ((:) fe (above_fmts u ls))) } }
actformparams actparams lfps =
 case actparams of { ( (:) t1 ts ) -> case lfps of { ( (:) p ls ) -> case p of { (C_AParam_1 t2 n) -> (if (t1==t2) then (actformparams ts ls) else ((:) (C_E_T_ActParam_1 ) (actformparams ts ls))) } ; ( [] ) -> ((:) (C_E_T_ActParam_1 ) ([] )) } ; ( [] ) -> case lfps of { ( (:) p ls ) -> ((:) (C_E_T_ActParam_1 ) ([] )) ; ( [] ) -> ([] ) } }
add_glue i ls =
 case ls of { ( [] ) -> ([] ) ; ( (:) s ss ) -> ((:) ((repeatCHAR ' ' i)++s) (add_glue i ss)) }
afmt_txt string =
 (C_AFormat_1 (text_fmts string))
allf fs =
 case fs of { ( [] ) -> "" ; ( (:) f fs2 ) -> ((txtstr f 0 "\n\n")++(best fs2)) }
assign_InfType n exptype fid env =
 let { (desttype) = (gettype n fid env) } in (if ((coersibleTypes desttype exptype)==(C_Errortype_1 )) then (C_E_T_NC_1 desttype exptype) else (C_NoTypeError_1 ))
asts i =
 (if (i==0) then "" else (if (i==1) then "*" else ((":"++(repeatCHAR '*' (fromIntegral (i-2))))++"*>")) )
beside_fmt l r =
 case l of { (C_Elem_1 lh ll lw lt) -> case r of { (C_Elem_1 rh rl rw rt) -> (C_Elem_1 ((lh+rh)-1) (ll+rl) (max lw (ll+rw)) (join ll lt rt)) } }
beside_fmts f l r =
 case f of { (C_F_1 pw _) -> (foldLeft pw l r) }
beside_height lh rh =
 (if ((lh==0)||(rh==0)) then 0 else 1)
best fs =
 case fs of { ( [] ) -> "" ; ( (:) f fs2 ) -> (txtstr f 0 "") }
blockStats sts =
 (C_Above_1 (C_Beside_1 (C_Text_1 "{") (C_Indent_1 2 sts)) (C_Text_1 "}"))
checkactparams s actpars env =
 case (lrc_map_application s env) of { (C_Consfunc_1 t i lfp) -> (actformparams actpars lfp) ; _ -> ([] ) }
choose_ab_beside_fmts avail fa fb f =
 (if avail then (beside_fmts f fa fb) else (above_fmts fa fb))
choose_ab_error_beside avail fa fb f =
 (if avail then (error_beside fa fb) else (above_fmts fa fb))
coersibleTypes t1 t2 =
 (if (t1==t2) then t1 else case t1 of { (C_Realtype_1 ) -> case t2 of { (C_Inttype_1 ) -> t1 ; _ -> (C_Errortype_1 ) } ; _ -> (C_Errortype_1 ) } )
cons_height pph acth avail =
 (if (acth==0) then (if (pph>0) then 1 else 0) else (acth+(if avail then 0 else 1) ))
dispf fs =
 case fs of { ( [] ) -> "" ; ( (:) f fs2 ) -> (txtstr f 0 "") }
dropWhileFormatsNotFit i fs =
 case fs of { ( [] ) -> ([] ) ; ( (:) f fs2 ) -> (if (notFits i f) then (dropWhileFormatsNotFit i fs2) else fs) }
dropWhileFormatsTooWide i l r =
 case r of { ( [] ) -> ([] ) ; ( (:) f fs ) -> (if (tooWide i l f) then (dropWhileFormatsTooWide i l fs) else r) }
empty_fmts =
 ([] )
enclosedBy o b c =
 (C_Beside_1 (C_Text_1 o) (C_Beside_1 b (C_Text_1 c)))
eq_DupC r1 r2 =
 (zipwith_max r1 r2)
eq_all_fmts pw fmts =
 (allf (set_fmts_render pw fmts))
eq_best_fmts pw fmts =
 (best (set_fmts_render pw fmts))
eq_disp pw fmts =
 (dispf (set_fmts_render pw fmts))
eq_set_error_msg apars bpars =
 (((("<Error: incorrect choice expression. #pars left "++(lrc_INTtoSTR apars))++" /= #pars right ")++(lrc_INTtoSTR bpars))++">")
eq_set_fmts_apply error msg fmts =
 (set_fmts_apply error (C_AFormat_1 (text_fmts msg)) fmts)
eq_set_fmts_fill fmts =
 (C_AFormat_1 fmts)
error_beside fs1 fs2 =
 ([] )
error_indent i fs =
 (map_indent_fmt i fs)
foldLeft pw l r =
 case l of { ( [] ) -> ([] ) ; ( (:) f fs ) -> (map_With_fmts f (dropWhileFormatsTooWide pw f r)) }
fst_Pair_Formats f =
 case f of { (C_C_Pair_Formats_1 a b) -> a }
fst_Pair_Lst_T_Errs t =
 case t of { (C_CPair_Lst_T_Errs_1 a _) -> a }
fst_Pair_Lst_T_Fmts t =
 case t of { (C_CPair_Lst_T_Fmts_1 a _) -> a }
fst_Pair_Lst_T_Mins t =
 case t of { (C_CPair_Lst_T_Mins_1 a _) -> a }
fst_Pair_T_Formats f =
 case f of { (C_C_Pair_T_Formats_1 a b) -> a }
fst_t t =
 case t of { (C_Triple_1 pw _ _) -> pw }
func_def_a_la_c ty name formpars body =
 (C_Above_1 (C_Beside_1 name (C_Beside_1 (C_Text_1 "(") (C_Beside_1 formpars (C_Beside_1 (C_Text_1 ") : ") ty)))) (blockStats body))
funccall name pars =
 (C_Beside_1 name (enclosedBy "(" pars ")"))
genCodeMainFun =
 ((:) (C_Call_1 (C_Ident_1 "main")) ((:) (C_Halt_1 ) ([] )))
genCodeParamsFunc i lsttp cap =
 case lsttp of { ( [] ) -> ([] ) ; ( (:) fp fps ) -> case cap of { ( [] ) -> ([] ) ; ( (:) c cs ) -> case fp of { (C_AParam_1 t n) -> (((:) (C_Pusha_1 n i) c)++((:) (C_Store_1 ) (genCodeParamsFunc i fps cs))) } } }
gen_code_func nf lst =
 case lst of { ( [] ) -> ([] ) ; ( (:) apar ls ) -> case apar of { (C_AParam_1 t n) -> ((:) (C_Var_1 n nf t) (gen_code_func nf ls)) } }
gen_code_func_inv fn env cap =
 case (lrc_map_application fn env) of { (C_Consfunc_1 t i lstp) -> ((genCodeParamsFunc i lstp cap)++((:) (C_Call_1 fn) ([] ))) ; _ -> ([] ) }
gen_code_ite i e c1 c2 =
 (((e++((:) (C_Jumpf_1 (C_Ident_1 ("else_"++(lrc_INTtoSTR i)))) c1))++((:) (C_Jumpf_1 (C_Ident_1 ("end_if_"++(lrc_INTtoSTR i)))) ((:) (C_ALabel_1 (C_Ident_1 ("else_"++(lrc_INTtoSTR i)))) c2)))++((:) (C_ALabel_1 (C_Ident_1 ("end_if_"++(lrc_INTtoSTR i)))) ([] )))
gen_code_while i e c1 =
 ((((:) (C_ALabel_1 (C_Ident_1 ("while_"++(lrc_INTtoSTR i)))) e)++((:) (C_Jumpf_1 (C_Ident_1 ("end_while_"++(lrc_INTtoSTR i)))) c1))++((:) (C_Jump_1 (C_Ident_1 ("while_"++(lrc_INTtoSTR i)))) ((:) (C_ALabel_1 (C_Ident_1 ("end_while_"++(lrc_INTtoSTR i)))) ([] ))))
gen_data_mem e =
 ([] )
get_fmts fs =
 case fs of { (C_AFormat_1 a) -> a ; (C_TFormats_1 _ _ _ _) -> (text_fmts "<Error: can\'t dup a dup") }
gettype id fid env =
 case (lrc_map_application id env) of { (C_Consvar_1 t i) -> t ; (C_Consarray_1 t i i2) -> t ; (C_Consfunc_1 t i l) -> t ; _ -> case (lrc_map_application fid env) of { (C_Consfunc_1 t i lstp) -> (gettypefp id lstp) ; _ -> (C_Errortype_1 ) } }
gettypefp id lstfp =
 case lstfp of { ( (:) p ls ) -> case p of { (C_AParam_1 t n) -> (if (id==n) then t else (gettypefp id ls)) } ; ( [] ) -> (C_Errortype_1 ) }
head_T_Errs t =
 case t of { ( (:) s _ ) -> s }
head_T_Fmts t =
 case t of { ( (:) s _ ) -> s }
head_T_Mins t =
 case t of { ( (:) s _ ) -> s }
head_T_Reqs t =
 case t of { ( (:) f _ ) -> f }
hv a b =
 (C_Join_1 (C_Dup_1 (C_Beside_1 a b) (C_Above_1 a b)))
hvc a b =
 (C_DupC_1 (C_BesideC_1 a b) (C_AboveC_1 a b))
if_t_ePP exp stats1 stats2 =
 (hv (hv (hv (C_Text_1 "if") (C_Beside_1 (C_Text_1 "(") (C_Beside_1 exp (C_Text_1 ")")))) (hv (C_Text_1 "then") (blockStats stats1))) (hv (C_Text_1 "else") (blockStats stats2)))
indent_fmt i f =
 case f of { (C_Elem_1 dh dl dw dt) -> (C_Elem_1 dh (i+dl) (i+dw) (add_glue i dt)) }
indent_fmts f i fs =
 case f of { (C_F_1 pw _) -> (map_indent_fmt i (dropWhileFormatsNotFit (pw-i) fs)) }
infType op t1 t2 =
 case op of { 1 -> (coersibleTypes t1 t2) ; 2 -> (if (t1==t2) then (C_Booltype_1 ) else (C_Errortype_1 )) ; _ -> (C_Errortype_1 ) }
initLst_Str l =
 case l of { ( [] ) -> ([] ) ; ( (:) x ( [] ) ) -> ([] ) ; ( (:) x ls ) -> ((:) x (initLst_Str ls)) }
isaritmexp t =
 case t of { (C_Inttype_1 ) -> True ; _ -> False }
isboolexp t =
 case t of { (C_Booltype_1 ) -> True ; _ -> False }
isinenv n fid env =
 (if (lrc_map_in n env) then True else case (lrc_map_application fid env) of { (C_Consfunc_1 t i lstp) -> (isinlst n lstp) ; _ -> False } )
isinlst n l =
 case l of { ( [] ) -> False ; ( (:) apar ls ) -> case apar of { (C_AParam_1 t n2) -> (if (n==n2) then True else (isinlst n ls)) } }
isinttype t =
 case t of { (C_Inttype_1 ) -> True ; _ -> False }
join i lt rt =
 case rt of { ( [] ) -> lt ; ( (:) x rt2 ) -> (((initLst_Str lt)++((:) ((lastLst_Str lt)++x) ([] )))++(add_glue i rt2)) }
lastLst_Str l =
 case l of { ( (:) x ( [] ) ) -> x ; ( (:) _ ls ) -> (lastLst_Str ls) }
last_w f =
 case f of { (C_Elem_1 _ pll _ _) -> pll }
map_With_fmts f fs =
 case fs of { ( [] ) -> ([] ) ; ( (:) f2 fs2 ) -> ((:) (beside_fmt f f2) (map_With_fmts f fs2)) }
map_indent_fmt i fs =
 case fs of { ( [] ) -> ([] ) ; ( (:) f fs2 ) -> ((:) (indent_fmt i f) (map_indent_fmt i fs2)) }
max_T_Frame a b =
 case a of { (C_F_1 w _) -> case b of { (C_F_1 z _) -> (if (w>z) then a else b) } }
merge xs ys =
 case xs of { ( [] ) -> ys ; ( (:) x xs2 ) -> case ys of { ( [] ) -> xs ; ( (:) y ys2 ) -> (if (x==y) then ((:) x (merge xs2 ys2)) else (if (x<y) then ((:) x (merge xs2 ys)) else ((:) y (merge xs ys2))) ) } }
nametoSTR n =
 case n of { (C_Ident_1 s) -> s }
narrow_frame i f =
 case f of { (C_F_1 s l) -> (C_F_1 (s-i) (l-i)) }
narrow_ll i f =
 case f of { (C_F_1 s l) -> (C_F_1 s (l-i)) }
newlines ls =
 case ls of { ( (:) x ( [] ) ) -> x ; ( (:) x xs ) -> ((x++"\n")++(newlines xs)) }
notFits delta f =
 case f of { (C_Elem_1 _ _ dw _) -> (dw>delta) }
nullFormats fs =
 case fs of { ( [] ) -> True ; _ -> False }
ppName n =
 case n of { (C_Ident_1 s) -> (C_Text_1 s) }
ppType t =
 case t of { (C_Inttype_1 ) -> (C_Text_1 "Int") ; (C_Realtype_1 ) -> (C_Text_1 "Real") ; (C_Booltype_1 ) -> (C_Text_1 "Bool") ; (C_Chartype_1 ) -> (C_Text_1 "Char") ; (C_Errortype_1 ) -> (C_Text_1 "Error??") }
s2fmt s = let { (l) = (fromIntegral (length s)) } in (C_Elem_1 1 l l ((:) s ([] )))
sem_fmts_cdup a b ae be anpars bnpars min_w mesg =
 (if (anpars/=bnpars) then (afmt_txt mesg) else (sem_fmts_dup a b ae be min_w))
sem_fmts_dup afs bfs ae be minw =
 (if (ae && be) then (afmt_txt (asts minw)) else (C_TFormats_1 (get_fmts afs) (get_fmts bfs) ae be))
set_error_msg numpars len =
 (((("<Error: incorrect apply expression. #pars "++(lrc_INTtoSTR numpars))++" /= #args ")++(lrc_INTtoSTR len))++">")
set_fmts_ab_above fs gs uh lh etxt =
 case fs of { (C_AFormat_1 ffmts) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_AFormat_1 (set_fmts_abovea ffmts gfmts uh lh)) False) ; (C_TFormats_1 a bs ae be) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_abovea ffmts a uh lh) (set_fmts_abovea ffmts bs uh lh) ae be) False) } ; (C_TFormats_1 a bs ae be) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_abovea a gfmts uh lh) (set_fmts_abovea bs gfmts uh lh) ae be) False) ; _ -> (C_C_Pair_T_Formats_1 (afmt_txt etxt) True) } }
set_fmts_ab_beside fs gs uh lh frame error etxt =
 case fs of { (C_AFormat_1 ffmts) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_AFormat_1 (set_fmts_besidea ffmts gfmts uh lh frame error)) False) ; (C_TFormats_1 a bs ae be) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_besidea ffmts a uh lh frame error) (set_fmts_besidea ffmts bs uh lh frame error) ae be) False) } ; (C_TFormats_1 a bs ae be) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_besidea a gfmts uh lh frame error) (set_fmts_besidea bs gfmts uh lh frame error) ae be) False) ; _ -> (C_C_Pair_T_Formats_1 (afmt_txt etxt) True) } }
set_fmts_above us ls uh lh =
 (set_fmts_ab_above us ls uh lh "<Error: can\'t above two pairs")
set_fmts_abovea a bs uh lh =
 (if (uh==0) then bs else (if (lh==0) then a else (above_fmts a bs)) )
set_fmts_apply error msg fmts =
 (if error then msg else fmts)
set_fmts_beside ls rs lh rh frame error =
 (set_fmts_ab_beside ls rs lh rh frame error "<Error: can\'t beside two pairs")
set_fmts_besidea ls rs lh rh frame error =
 (if (lh==0) then rs else (if (rh==0) then ls else (if error then (error_beside ls rs) else (beside_fmts frame ls rs)) ) )
set_fmts_empty =
 (C_AFormat_1 (empty_fmts ))
set_fmts_fillblock i fmts w =
 (if (i<0) then (afmt_txt "<Error: negative page width in fillblock>") else (if (i>w) then (afmt_txt (asts i)) else (C_AFormat_1 fmts)) )
set_fmts_filllist af bf ah bh f avail =
 case bf of { (C_AFormat_1 ns) -> (if (ah==0) then (C_C_Pair_Formats_1 ns False) else (if (bh==0) then (C_C_Pair_Formats_1 af False) else (if (bh<=1) then (C_C_Pair_Formats_1 (choose_ab_beside_fmts avail af ns f) False) else (C_C_Pair_Formats_1 (choose_ab_error_beside avail af (text_fmts "<Error: element in fill higher than 1>") f) True)) ) ) ; _ -> (C_C_Pair_Formats_1 (set_fmts_filllista (text_fmts "<Error: element in fill list is a pair>") af ah bh avail f) True) }
set_fmts_filllista fs afmts ah nh avail f =
 (if (ah==0) then afmts else (if (nh==0) then fs else (choose_ab_error_beside avail fs afmts f)) )
set_fmts_indent i f pw minw frame error =
 (if (i<0) then (afmt_txt "<Error: negative indentation>") else (if (i>pw) then (afmt_txt (asts minw)) else (if error then (set_fmts_indent_error_indent i f) else (set_fmts_indent_indent_fmts i f frame)) ) )
set_fmts_indent_error_indent i fmts =
 case fmts of { (C_AFormat_1 fs) -> (C_AFormat_1 (error_indent i fs)) ; (C_TFormats_1 a bs ae be) -> (C_TFormats_1 (error_indent i a) (error_indent i bs) ae be) }
set_fmts_indent_indent_fmts i fmts frame =
 case fmts of { (C_AFormat_1 fs) -> (C_AFormat_1 (indent_fmts frame i fs)) ; (C_TFormats_1 a bs ae be) -> (C_TFormats_1 (indent_fmts frame i a) (indent_fmts frame i bs) ae be) }
set_fmts_join fs err =
 case fs of { (C_AFormat_1 f) -> (if err then (C_C_Pair_T_Formats_1 fs err) else (C_C_Pair_T_Formats_1 (afmt_txt "<Error: can\'t join a single result>") True)) ; (C_TFormats_1 a bs ae be) -> let { (aa) = (if be then (if (nullFormats a) then bs else a) else (if ae then (if (nullFormats bs) then a else bs) else (merge a bs)) ) } in (C_C_Pair_T_Formats_1 (C_AFormat_1 aa) False) }
set_fmts_render pw fmts =
 (if (pw<0) then (text_fmts "<Error: negative page width >") else case fmts of { (C_AFormat_1 fmt) -> fmt ; _ -> (text_fmts "<Error: can\'t render a pair>") } )
set_fmts_text string minw error =
 (afmt_txt (if error then (asts minw) else string) )
set_var_apply error lem min =
 (if error then lem else min)
snd_Pair_Formats f =
 case f of { (C_C_Pair_Formats_1 a b) -> b }
snd_Pair_Lst_T_Errs t =
 case t of { (C_CPair_Lst_T_Errs_1 _ a) -> a }
snd_Pair_Lst_T_Fmts t =
 case t of { (C_CPair_Lst_T_Fmts_1 _ a) -> a }
snd_Pair_Lst_T_Mins t =
 case t of { (C_CPair_Lst_T_Mins_1 _ a) -> a }
snd_Pair_T_Formats f =
 case f of { (C_C_Pair_T_Formats_1 a b) -> b }
snd_t t =
 case t of { (C_Triple_1 _ pll _) -> pll }
splitAt_T_Errs i tm =
 (if (i<=0) then (C_CPair_Lst_T_Errs_1 ([] ) tm) else case tm of { ( [] ) -> (C_CPair_Lst_T_Errs_1 ([] ) ([] )) ; ( (:) x xs ) -> let { (a) = (splitAt_T_Errs (i-1) xs) ; (xs2) = (fst_Pair_Lst_T_Errs a) ; (xs22) = (snd_Pair_Lst_T_Errs a) } in (C_CPair_Lst_T_Errs_1 ((:) x xs2) xs22) } )
splitAt_T_Fmts i tm =
 (if (i<=0) then (C_CPair_Lst_T_Fmts_1 ([] ) tm) else case tm of { ( [] ) -> (C_CPair_Lst_T_Fmts_1 ([] ) ([] )) ; ( (:) x xs ) -> let { (a) = (splitAt_T_Fmts (i-1) xs) ; (xs2) = (fst_Pair_Lst_T_Fmts a) ; (xs22) = (snd_Pair_Lst_T_Fmts a) } in (C_CPair_Lst_T_Fmts_1 ((:) x xs2) xs22) } )
splitAt_T_Mins i tm =
 (if (i<=0) then (C_CPair_Lst_T_Mins_1 ([] ) tm) else case tm of { ( [] ) -> (C_CPair_Lst_T_Mins_1 ([] ) ([] )) ; ( (:) x xs ) -> let { (a) = (splitAt_T_Mins (i-1) xs) ; (xs2) = (fst_Pair_Lst_T_Mins a) ; (xs22) = (snd_Pair_Lst_T_Mins a) } in (C_CPair_Lst_T_Mins_1 ((:) x xs2) xs22) } )
tail_T_Reqs t =
 case t of { ( (:) _ tr ) -> tr }
text_fmts s =
 ((:) (s2fmt s) ([] ))
third_t t =
 case t of { (C_Triple_1 _ _ ph) -> ph }
tooWide pw x y =
 ((max (total_w x) ((last_w x)+(total_w y)))>pw)
total_w f =
 case f of { (C_Elem_1 _ _ w _) -> w }
txtstr f n s =
 case f of { (C_Elem_1 dh dl dw dt) -> ((newlines (add_glue n dt))++s) }
typecheck s exptype fid env =
 (if (exptype==(gettype s fid env)) then True else False)
whilePP bg exp stats =
 (C_Apply_1 (C_JoinC_1 (hvc (C_ParC_1 ) (C_JoinC_1 (hvc (C_ParC_1 ) C_ParC_1)))) (C_ConsArgs_1 bg (C_ConsArgs_1 exp (C_ConsArgs_1 stats (C_NilArgs_1 )))))
whilePPOLD exp stats =
 (hv (hv (C_Text_1 "while") (C_Beside_1 (C_Text_1 "(") (C_Beside_1 exp (C_Text_1 ")")))) (blockStats stats))
zipwith_max l1 l2 =
 case l1 of { ( [] ) -> l2 ; ( (:) l11 l11s ) -> case l2 of { ( [] ) -> l1 ; ( (:) l22 l22s ) -> ((:) (max_T_Frame l11 l22) (zipwith_max l11s l22s)) } }
