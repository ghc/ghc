%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIfaceSig ( tcInterfaceSigs, tcVar, tcCoreExpr, tcCoreLamBndrs ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), IfaceSig(..) )
import TcMonad
import TcMonoType	( tcHsType, tcHsTypeKind, 
				-- NB: all the tyars in interface files are kinded,
				-- so tcHsType will do the Right Thing without
				-- having to mess about with zonking
			  tcExtendTyVarScope
			)
import TcEnv		( ValueEnv, tcExtendTyVarEnv, 
			  tcExtendGlobalValEnv, tcSetValueEnv,
			  tcLookupValueMaybe,
			  explicitLookupValue, badCon, badPrimOp, valueEnvIds
			)
import TcType		( TcKind, kindToTcKind )

import RnHsSyn		( RenamedHsDecl )
import HsCore
import Literal		( Literal(..) )
import CoreSyn
import CoreUtils	( exprType )
import CoreUnfold
import CoreLint		( lintUnfolding )
import WorkWrap		( mkWrapper )
import PrimOp		( PrimOp(..), CCall(..), CCallTarget(..) )

import Id		( Id, mkId, mkVanillaId,
			  isDataConWrapId_maybe
			)
import MkId		( mkCCallOpId )
import IdInfo
import DataCon		( dataConSig, dataConArgTys )
import Type		( mkSynTy, mkTyVarTys, splitAlgTyConApp, splitFunTys, unUsgTy )
import Var		( mkTyVar, tyVarKind )
import VarEnv
import Name		( Name, NamedThing(..), isLocallyDefined )
import Unique		( rationalTyConKey )
import TysWiredIn	( integerTy, stringTy )
import Demand		( wwLazy )
import ErrUtils		( pprBagOfErrors )
import Maybes		( maybeToBool, MaybeErr(..) )
import Outputable	
import Util		( zipWithEqual )
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: ValueEnv		-- Envt to use when checking unfoldings
		-> [RenamedHsDecl]	-- Ignore non-sig-decls in these decls
		-> TcM s [Id]
		

tcInterfaceSigs unf_env decls
  = listTc [ do_one name ty id_infos src_loc
	   | SigD (IfaceSig name ty id_infos src_loc) <- decls]
  where
    in_scope_vars = filter isLocallyDefined (valueEnvIds unf_env)

    do_one name ty id_infos src_loc
      = tcAddSrcLoc src_loc 		 		$	
	tcAddErrCtxt (ifaceSigCtxt name)		$
	tcHsType ty					`thenTc` \ sigma_ty ->
	tcIdInfo unf_env in_scope_vars name 
		 sigma_ty vanillaIdInfo id_infos	`thenTc` \ id_info ->
	returnTc (mkId name sigma_ty id_info)
\end{code}

\begin{code}
tcIdInfo unf_env in_scope_vars name ty info info_ins
  = foldlTc tcPrag vanillaIdInfo info_ins
  where
    tcPrag info (HsArity arity) = returnTc (info `setArityInfo`  arity)
    tcPrag info (HsUpdate upd)  = returnTc (info `setUpdateInfo` upd)
    tcPrag info (HsNoCafRefs)   = returnTc (info `setCafInfo`	 NoCafRefs)
    tcPrag info HsCprInfo       = returnTc (info `setCprInfo`	 ReturnsCPR)

    tcPrag info (HsUnfold inline_prag expr)
	= tcPragExpr unf_env name in_scope_vars expr 	`thenNF_Tc` \ maybe_expr' ->
	  let
		-- maybe_expr doesn't get looked at if the unfolding
		-- is never inspected; so the typecheck doesn't even happen
		unfold_info = case maybe_expr' of
				Nothing    -> noUnfolding
				Just expr' -> mkTopUnfolding expr' 
		info1 = info `setUnfoldingInfo` unfold_info
		info2 = info1 `setInlinePragInfo` inline_prag
	  in
	  returnTc info2

    tcPrag info (HsStrictness (HsStrictnessInfo (demands,bot_result)))
	= returnTc (info `setStrictnessInfo` StrictnessInfo demands bot_result)

    tcPrag info (HsWorker nm)
	= tcWorkerInfo unf_env ty info nm
\end{code}

\begin{code}
tcWorkerInfo unf_env ty info worker_name
  | not (hasArity arity_info)
  = pprPanic "Worker with no arity info" (ppr worker_name)
 
  | otherwise
  = uniqSMToTcM (mkWrapper ty arity demands res_bot cpr_info) `thenNF_Tc` \ wrap_fn ->
    let
	-- Watch out! We can't pull on unf_env too eagerly!
	info' = case explicitLookupValue unf_env worker_name of
			Just worker_id -> info `setUnfoldingInfo`  mkTopUnfolding (wrap_fn worker_id)
                                               `setWorkerInfo`     HasWorker worker_id arity

    			Nothing        -> pprTrace "tcWorkerInfo failed:" (ppr worker_name) info
    in
    returnTc info'
  where
	-- We are relying here on arity, cpr and strictness info always appearing 
	-- before worker info,  fingers crossed ....
      arity_info = arityInfo info
      arity      = arityLowerBound arity_info
      cpr_info   = cprInfo info
      (demands, res_bot)    = case strictnessInfo info of
			 	StrictnessInfo d r -> (d,r)
				_                  -> (take arity (repeat wwLazy),False)	-- Noncommittal
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr unf_env name in_scope_vars expr
  = tcDelay unf_env doc $
	tcCoreExpr expr		`thenTc` \ core_expr' ->

		-- Check for type consistency in the unfolding
	tcGetSrcLoc		`thenNF_Tc` \ src_loc -> 
	case lintUnfolding src_loc in_scope_vars core_expr' of
	  Nothing       -> returnTc core_expr'
	  Just fail_msg -> failWithTc ((doc <+> text "failed Lint") $$ fail_msg)
  where
    doc = text "unfolding of" <+> ppr name

tcDelay :: ValueEnv -> SDoc -> TcM s a -> NF_TcM s (Maybe a)
tcDelay unf_env doc thing_inside
  = forkNF_Tc (
	recoverNF_Tc bad_value (
		tcSetValueEnv unf_env thing_inside	`thenTc` \ r ->
		returnTc (Just r)
    ))			
  where
	-- The trace tells what wasn't available, for the benefit of
	-- compiler hackers who want to improve it!
    bad_value = getErrsTc		`thenNF_Tc` \ (warns,errs) ->
		returnNF_Tc (pprTrace "Failed:" 
				   	 (hang doc 4 (pprBagOfErrors errs))
					 Nothing)
\end{code}


Variables in unfoldings
~~~~~~~~~~~~~~~~~~~~~~~
****** Inside here we use only the Global environment, even for locally bound variables.
****** Why? Because we know all the types and want to bind them to real Ids.

\begin{code}
tcVar :: Name -> TcM s Id
tcVar name
  = tcLookupValueMaybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of {
	Just id -> returnTc id;
	Nothing -> failWithTc (noDecl name)
    }

noDecl name = hsep [ptext SLIT("Warning: no binding for"), ppr name]
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM s CoreExpr

tcCoreExpr (UfType ty)
  = tcHsTypeKind ty	`thenTc` \ (_, ty') ->
	-- It might not be of kind type
    returnTc (Type ty')

tcCoreExpr (UfVar name)
  = tcVar name 	`thenTc` \ id ->
    returnTc (Var id)

tcCoreExpr (UfLit lit)
  = returnTc (Lit lit)

-- The dreaded lit-lits are also similar, except here the type
-- is read in explicitly rather than being implicit
tcCoreExpr (UfLitLit lit ty)
  = tcHsType ty		`thenTc` \ ty' ->
    returnTc (Lit (MachLitLit lit ty'))

tcCoreExpr (UfCCall cc ty)
  = tcHsType ty 	`thenTc` \ ty' ->
    tcGetUnique		`thenNF_Tc` \ u ->
    returnTc (Var (mkCCallOpId u cc ty'))

tcCoreExpr (UfTuple name args) 
  = tcVar name			`thenTc` \ con_id ->
    mapTc tcCoreExpr args	`thenTc` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . unUsgTy . exprType) args' ++ args'
    in
    returnTc (mkApps (Var con_id) con_args)

tcCoreExpr (UfLam bndr body)
  = tcCoreLamBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenTc` \ body' ->
    returnTc (Lam bndr' body')

tcCoreExpr (UfApp fun arg)
  = tcCoreExpr fun		`thenTc` \ fun' ->
    tcCoreExpr arg		`thenTc` \ arg' ->
    returnTc (App fun' arg')

tcCoreExpr (UfCase scrut case_bndr alts) 
  = tcCoreExpr scrut					`thenTc` \ scrut' ->
    let
	scrut_ty = exprType scrut'
	case_bndr' = mkVanillaId case_bndr scrut_ty
    in
    tcExtendGlobalValEnv [case_bndr']	$
    mapTc (tcCoreAlt scrut_ty) alts	`thenTc` \ alts' ->
    returnTc (Case scrut' case_bndr' alts')

tcCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = tcCoreExpr rhs		`thenTc` \ rhs' ->
    tcCoreValBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenTc` \ body' ->
    returnTc (Let (NonRec bndr' rhs') body')

tcCoreExpr (UfLet (UfRec pairs) body)
  = tcCoreValBndrs bndrs	$ \ bndrs' ->
    mapTc tcCoreExpr rhss	`thenTc` \ rhss' ->
    tcCoreExpr body		`thenTc` \ body' ->
    returnTc (Let (Rec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs

tcCoreExpr (UfNote note expr) 
  = tcCoreExpr expr		`thenTc` \ expr' ->
    case note of
	UfCoerce to_ty -> tcHsType to_ty	`thenTc` \ to_ty' ->
			  returnTc (Note (Coerce (unUsgTy to_ty')
                                                 (unUsgTy (exprType expr'))) expr')
	UfInlineCall   -> returnTc (Note InlineCall expr')
	UfInlineMe     -> returnTc (Note InlineMe   expr')
	UfSCC cc       -> returnTc (Note (SCC cc)   expr')

tcCoreNote (UfSCC cc)   = returnTc (SCC cc)
tcCoreNote UfInlineCall = returnTc InlineCall 
\end{code}

\begin{code}
tcCoreLamBndr (UfValBinder name ty) thing_inside
  = tcHsType ty			`thenTc` \ ty' ->
    let
	id = mkVanillaId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreLamBndr (UfTyBinder name kind) thing_inside
  = let
	tyvar = mkTyVar name kind
    in
    tcExtendTyVarEnv [tyvar] (thing_inside tyvar)
    
tcCoreLamBndrs []     thing_inside = thing_inside []
tcCoreLamBndrs (b:bs) thing_inside
  = tcCoreLamBndr b	$ \ b' ->
    tcCoreLamBndrs bs	$ \ bs' ->
    thing_inside (b':bs')

tcCoreValBndr (UfValBinder name ty) thing_inside
  = tcHsType ty			`thenTc` \ ty' ->
    let
	id = mkVanillaId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapTc tcHsType tys			`thenTc` \ tys' ->
    let
	ids = zipWithEqual "tcCoreValBndr" mkVanillaId names tys'
    in
    tcExtendGlobalValEnv ids $
    thing_inside ids
  where
    names = [name | UfValBinder name _  <- bndrs]
    tys   = [ty   | UfValBinder _    ty <- bndrs]
\end{code}    

\begin{code}
tcCoreAlt scrut_ty (UfDefault, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenTc` \ rhs' ->
    returnTc (DEFAULT, [], rhs')
  
tcCoreAlt scrut_ty (UfLitAlt lit, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenTc` \ rhs' ->
    returnTc (LitAlt lit, [], rhs')

tcCoreAlt scrut_ty (UfLitLitAlt str ty, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenTc` \ rhs' ->
    tcHsType ty			`thenTc` \ ty' ->
    returnTc (LitAlt (MachLitLit str ty'), [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcCoreAlt scrut_ty (UfDataAlt con_name, names, rhs)
  = tcVar con_name		`thenTc` \ con_id ->
    let
	con			= case isDataConWrapId_maybe con_id of
					Just con -> con
					Nothing  -> pprPanic "tcCoreAlt" (ppr con_id)

	(main_tyvars, _, ex_tyvars, _, _, _) = dataConSig con

	(tycon, inst_tys, cons) = splitAlgTyConApp scrut_ty
	ex_tyvars'		= [mkTyVar name (tyVarKind tv) | (name,tv) <- names `zip` ex_tyvars] 
	ex_tys'			= mkTyVarTys ex_tyvars'
	arg_tys			= dataConArgTys con (inst_tys ++ ex_tys')
	id_names		= drop (length ex_tyvars) names
	arg_ids
#ifdef DEBUG
		| length id_names /= length arg_tys
		= pprPanic "tcCoreAlts" (ppr (con_name, names, rhs) $$
					 (ppr main_tyvars <+> ppr ex_tyvars) $$
					 ppr arg_tys)
		| otherwise
#endif
		= zipWithEqual "tcCoreAlts" mkVanillaId id_names arg_tys
    in
    ASSERT( con `elem` cons && length inst_tys == length main_tyvars )
    tcExtendTyVarEnv ex_tyvars'			$
    tcExtendGlobalValEnv arg_ids		$
    tcCoreExpr rhs					`thenTc` \ rhs' ->
    returnTc (DataAlt con, ex_tyvars' ++ arg_ids, rhs')
\end{code}

\begin{code}
ifaceSigCtxt sig_name
  = hsep [ptext SLIT("In an interface-file signature for"), ppr sig_name]
\end{code}

