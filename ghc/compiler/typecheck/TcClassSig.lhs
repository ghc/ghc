%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassSig]{Typecheck a class signature}

\begin{code}
#include "HsVersions.h"

module TcClassSig ( tcClassSigs ) where

import TcMonad		-- typechecking monadic machinery
import AbsSyn		-- the stuff being typechecked

import AbsUniType
import CE		( CE(..) )
import E		( mkE, getE_TCE, getE_CE, nullGVE, unitGVE, plusGVE, GVE(..), E )
import Errors		( methodTypeLacksTyVarErr, confusedNameErr )
import Id		( mkDefaultMethodId, mkClassOpId, IdInfo )
import IdInfo
import InstEnv		( InstTemplate )
import TCE		( TCE(..), UniqFM )
import TVE		( TVE(..) )
import TcPolyType	( tcPolyType )
import TcPragmas	( tcClassOpPragmas )
import Util
\end{code}

\begin{code}
tcClassSigs :: E -> TVE -> Class    	-- Knot tying only!
	    -> (ClassOp -> SpecEnv)	-- Ditto; the spec info for the class ops
	    -> TyVarTemplate	 	-- The class type variable, used for error check only
	    -> [Name]			-- Names with default methods
	    -> [RenamedClassOpSig]
	    -> Baby_TcM ([ClassOp],	-- class ops
		         GVE,		-- env for looking up the class ops
		         [Id],		-- selector ids
		         [Id])		-- default-method ids

tcClassSigs e tve rec_clas rec_classop_spec_fn clas_tyvar defm_names sigs
  = mapB_Tc tc_sig sigs	`thenB_Tc` \ stuff ->
    let
	(ops, op_gves, sel_ids, defm_ids) = unzip4 stuff
    in
    returnB_Tc (ops, foldr plusGVE nullGVE op_gves, sel_ids, defm_ids)
  where
    rec_ce  = getE_CE  e
    rec_tce = getE_TCE e

    tc_sig (ClassOpSig name@(ClassOpName op_uniq clas_name op_name tag) poly_ty pragmas src_loc)
      = addSrcLocB_Tc src_loc				 (
	tcPolyType rec_ce rec_tce tve poly_ty	`thenB_Tc` \ local_ty ->
	let
	    (local_tyvar_tmpls, theta, tau) = splitType local_ty
	    full_theta       = (rec_clas, (mkTyVarTemplateTy clas_tyvar)) : theta
	    full_tyvar_tmpls = clas_tyvar : local_tyvar_tmpls
	    global_ty        = mkForallTy full_tyvar_tmpls (mkRhoTy full_theta tau)
	    class_op         = mkClassOp op_name tag local_ty

	    not_elem = isn'tIn "tcClassSigs"
	in
	    -- Check that the class type variable is mentioned
	checkB_Tc (clas_tyvar `not_elem` extractTyVarTemplatesFromTy local_ty)
		(methodTypeLacksTyVarErr clas_tyvar (_UNPK_ op_name) src_loc) `thenB_Tc_`

	    -- Munch the pragmas, building a suitable default-method
	    -- Id from the details found there.
	getUniqueB_Tc			`thenB_Tc` \ d_uniq ->

	fixB_Tc ( \ ~(rec_op_id, rec_defm_id) ->
	    tcClassOpPragmas e{-fake_E-}
		global_ty
		rec_op_id rec_defm_id
		(rec_classop_spec_fn class_op)
		pragmas		`thenB_Tc` \ (op_info, defm_info) ->
	    let
		-- the default method is error "No default ..." if there is no
		-- default method code or the imported default method is bottoming.

		error_defm = if isLocallyDefined clas_name then
				 name `notElem` defm_names 
			     else 
				 bottomIsGuaranteed (getInfo defm_info)
	    in
	    returnB_Tc (
	      mkClassOpId      op_uniq rec_clas class_op global_ty op_info,
	      mkDefaultMethodId d_uniq rec_clas class_op error_defm global_ty defm_info
	    )

	) `thenB_Tc` \ (selector_id, default_method_id) ->

	returnB_Tc (class_op, unitGVE name selector_id, selector_id, default_method_id)
    	)

    tc_sig (ClassOpSig name _ _ src_loc)
      = failB_Tc (confusedNameErr
		    "Bad name on a class-method signature (a Prelude name?)"
		    name src_loc)
\end{code}
