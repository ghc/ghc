%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIfaceSig ( tcInterfaceSigs, tcDelay, tcVar, tcCoreExpr, tcCoreLamBndrs ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..), HsTupCon(..) )
import TcMonad
import TcMonoType	( tcIfaceType )
import TcEnv		( RecTcEnv, tcExtendTyVarEnv, 
			  tcExtendGlobalValEnv, tcSetEnv, tcEnvIds,
			  tcLookupGlobal_maybe, tcLookupRecId_maybe
			)

import RnHsSyn		( RenamedTyClDecl )
import HsCore
import Literal		( Literal(..) )
import CoreSyn
import CoreUtils	( exprType )
import CoreUnfold
import CoreLint		( lintUnfolding )
import WorkWrap		( mkWrapper )

import Id		( Id, mkVanillaGlobal, mkLocalId, idName, isDataConWrapId_maybe )
import Module		( Module )
import MkId		( mkFCallId )
import IdInfo
import TyCon		( tyConDataCons )
import DataCon		( DataCon, dataConId, dataConSig, dataConArgTys )
import Type		( mkTyVarTys, splitTyConApp )
import TysWiredIn	( tupleCon )
import Var		( mkTyVar, tyVarKind )
import Name		( Name, nameIsLocalOrFrom )
import ErrUtils		( pprBagOfErrors )
import Outputable	
import Util		( zipWithEqual )
import HscTypes		( TyThing(..) )
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: RecTcEnv		-- Envt to use when checking unfoldings
		-> Module		-- This module
		-> [RenamedTyClDecl]	-- Ignore non-sig-decls in these decls
		-> TcM [Id]
		

tcInterfaceSigs unf_env mod decls
  = listTc [ do_one name ty id_infos src_loc
	   | IfaceSig {tcdName = name, tcdType = ty, tcdIdInfo = id_infos, tcdLoc =src_loc} <- decls]
  where
    in_scope_vars = filter (nameIsLocalOrFrom mod . idName) (tcEnvIds unf_env)
		-- Oops: using isLocalId instead can give a black hole
		-- because it looks at the idinfo

	-- When we have hi-boot files, an unfolding might refer to
	-- something defined in this module, so we must build a
	-- suitable in-scope set.  This thunk will only be poked
	-- if -dcore-lint is on.

    do_one name ty id_infos src_loc
      = tcAddSrcLoc src_loc 		 		$	
	tcAddErrCtxt (ifaceSigCtxt name)		$
	tcIfaceType ty					`thenTc` \ sigma_ty ->
	tcIdInfo unf_env in_scope_vars name 
		 sigma_ty id_infos			`thenTc` \ id_info ->
	returnTc (mkVanillaGlobal name sigma_ty id_info)
\end{code}

\begin{code}
tcIdInfo unf_env in_scope_vars name ty info_ins
  = foldlTc tcPrag init_info info_ins 
  where
    -- set the CgInfo to something sensible but uninformative before
    -- we start, because the default CgInfo is a panic.
    init_info = vanillaIdInfo `setCgInfo` vanillaCgInfo

    tcPrag info (HsNoCafRefs)   = returnTc (info `setCafInfo`	 NoCafRefs)

    tcPrag info (HsArity arity) = 
	returnTc (info `setArityInfo` arity)

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

    tcPrag info (HsStrictness strict_info)
	= returnTc (info `setNewStrictnessInfo` Just strict_info)

    tcPrag info (HsWorker nm arity)
	= tcWorkerInfo unf_env ty info nm arity
\end{code}

\begin{code}
tcWorkerInfo unf_env ty info worker_name arity
  = uniqSMToTcM (mkWrapper ty strict_sig) `thenNF_Tc` \ wrap_fn ->
    let
	-- Watch out! We can't pull on unf_env too eagerly!
	info' = case tcLookupRecId_maybe unf_env worker_name of
		  Just worker_id -> 
		    info `setUnfoldingInfo`  mkTopUnfolding (wrap_fn worker_id)
		         `setWorkerInfo`     HasWorker worker_id arity

    		  Nothing -> pprTrace "tcWorkerInfo failed:" 
				(ppr worker_name) info
    in
    returnTc info'
  where
    	-- We are relying here on strictness info always appearing 
	-- before worker info,  fingers crossed ....
      strict_sig = case newStrictnessInfo info of
		 	Just sig -> sig
			Nothing  -> pprPanic "Worker info but no strictness for" (ppr worker_name)
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr unf_env name in_scope_vars expr
  = tcDelay unf_env doc Nothing $
	tcCoreExpr expr		`thenTc` \ core_expr' ->

		-- Check for type consistency in the unfolding
	tcGetSrcLoc		`thenNF_Tc` \ src_loc -> 
	getDOptsTc		`thenTc` \ dflags ->
	case lintUnfolding dflags src_loc in_scope_vars core_expr' of
	  (Nothing,_)       -> returnTc (Just core_expr')  -- ignore warnings
	  (Just fail_msg,_) -> failWithTc ((doc <+> text "failed Lint") $$ fail_msg)
  where
    doc = text "unfolding of" <+> ppr name

tcDelay :: RecTcEnv -> SDoc -> a -> TcM a -> NF_TcM a
tcDelay unf_env doc bad_ans thing_inside
  = forkNF_Tc (
	recoverNF_Tc bad_value (
		tcSetEnv unf_env thing_inside
    ))			
  where
	-- The trace tells what wasn't available, for the benefit of
	-- compiler hackers who want to improve it!
    bad_value = getErrsTc		`thenNF_Tc` \ (warns,errs) ->
		returnNF_Tc (pprTrace "Failed:" 
				   	 (hang doc 4 (pprBagOfErrors errs))
					 bad_ans)
\end{code}


Variables in unfoldings
~~~~~~~~~~~~~~~~~~~~~~~
****** Inside here we use only the Global environment, even for locally bound variables.
****** Why? Because we know all the types and want to bind them to real Ids.

\begin{code}
tcVar :: Name -> TcM Id
tcVar name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of {
	Just (AnId id)	-> returnTc id ;
	Nothing		-> failWithTc (noDecl name)
    }

noDecl name = hsep [ptext SLIT("Warning: no binding for"), ppr name]
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM CoreExpr

tcCoreExpr (UfType ty)
  = tcIfaceType ty		`thenTc` \ ty' ->
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
  = tcIfaceType ty		`thenTc` \ ty' ->
    returnTc (Lit (MachLitLit lit ty'))

tcCoreExpr (UfFCall cc ty)
  = tcIfaceType ty 	`thenTc` \ ty' ->
    tcGetUnique		`thenNF_Tc` \ u ->
    returnTc (Var (mkFCallId u cc ty'))

tcCoreExpr (UfTuple (HsTupCon _ boxity arity) args) 
  = mapTc tcCoreExpr args	`thenTc` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . exprType) args' ++ args'
    in
    returnTc (mkApps (Var con_id) con_args)
  where
    con_id = dataConId (tupleCon boxity arity)
    

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
	case_bndr' = mkLocalId case_bndr scrut_ty
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
	UfCoerce to_ty -> tcIfaceType to_ty	`thenTc` \ to_ty' ->
			  returnTc (Note (Coerce to_ty'
                                                 (exprType expr')) expr')
	UfInlineCall   -> returnTc (Note InlineCall expr')
	UfInlineMe     -> returnTc (Note InlineMe   expr')
	UfSCC cc       -> returnTc (Note (SCC cc)   expr')
\end{code}

\begin{code}
tcCoreLamBndr (UfValBinder name ty) thing_inside
  = tcIfaceType ty		`thenTc` \ ty' ->
    let
	id = mkLocalId name ty'
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
  = tcIfaceType ty			`thenTc` \ ty' ->
    let
	id = mkLocalId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapTc tcIfaceType tys		`thenTc` \ tys' ->
    let
	ids = zipWithEqual "tcCoreValBndr" mkLocalId names tys'
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
    tcIfaceType ty		`thenTc` \ ty' ->
    returnTc (LitAlt (MachLitLit str ty'), [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcCoreAlt scrut_ty alt@(con, names, rhs)
  = tcConAlt con	`thenTc` \ con ->
    let
	(main_tyvars, _, ex_tyvars, _, _, _) = dataConSig con

	(tycon, inst_tys)   = splitTyConApp scrut_ty	-- NB: not tcSplitTyConApp
							-- We are looking at Core here
	ex_tyvars'	    = [mkTyVar name (tyVarKind tv) | (name,tv) <- names `zip` ex_tyvars] 
	ex_tys'		    = mkTyVarTys ex_tyvars'
	arg_tys		    = dataConArgTys con (inst_tys ++ ex_tys')
	id_names	    = drop (length ex_tyvars) names
	arg_ids
#ifdef DEBUG
		| length id_names /= length arg_tys
		= pprPanic "tcCoreAlts" (ppr (con, names, rhs) $$
					 (ppr main_tyvars <+> ppr ex_tyvars) $$
					 ppr arg_tys)
		| otherwise
#endif
		= zipWithEqual "tcCoreAlts" mkLocalId id_names arg_tys
    in
    ASSERT( con `elem` tyConDataCons tycon && length inst_tys == length main_tyvars )
    tcExtendTyVarEnv ex_tyvars'			$
    tcExtendGlobalValEnv arg_ids		$
    tcCoreExpr rhs					`thenTc` \ rhs' ->
    returnTc (DataAlt con, ex_tyvars' ++ arg_ids, rhs')


tcConAlt :: UfConAlt Name -> TcM DataCon
tcConAlt (UfTupleAlt (HsTupCon _ boxity arity))
  = returnTc (tupleCon boxity arity)

tcConAlt (UfDataAlt con_name)
  = tcVar con_name	`thenTc` \ con_id ->
    returnTc (case isDataConWrapId_maybe con_id of
		    Just con -> con
		    Nothing  -> pprPanic "tcCoreAlt" (ppr con_id))
\end{code}

\begin{code}
ifaceSigCtxt sig_name
  = hsep [ptext SLIT("In an interface-file signature for"), ppr sig_name]
\end{code}

