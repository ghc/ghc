%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIfaceSig ( tcInterfaceSigs,
		    tcVar,
		    tcCoreExpr,
		    tcCoreLamBndrs,
		    tcCoreBinds ) where

#include "HsVersions.h"

import HsSyn		( CoreDecl(..), TyClDecl(..), HsTupCon(..) )
import TcHsSyn		( TypecheckedCoreBind )
import TcRnMonad
import TcMonoType	( tcIfaceType, kcHsSigType )
import TcEnv		( RecTcGblEnv, tcExtendTyVarEnv, 
			  tcExtendGlobalValEnv, 
			  tcLookupGlobal_maybe, tcLookupRecId_maybe
			)

import RnHsSyn		( RenamedCoreDecl, RenamedTyClDecl )
import HsCore
import Literal		( Literal(..) )
import CoreSyn
import CoreUtils	( exprType )
import CoreUnfold
import CoreLint		( lintUnfolding )
import WorkWrap		( mkWrapper )

import Id		( Id, mkVanillaGlobal, mkLocalId, isDataConWrapId_maybe )
import MkId		( mkFCallId )
import IdInfo
import TyCon		( tyConDataCons, tyConTyVars )
import DataCon		( DataCon, dataConWorkId, dataConExistentialTyVars, dataConArgTys )
import Type		( mkTyVarTys, splitTyConApp )
import TysWiredIn	( tupleCon )
import Var		( mkTyVar, tyVarKind )
import Name		( Name )
import UniqSupply	( initUs_ )
import Outputable	
import Util		( zipWithEqual, dropList, equalLength )
import HscTypes		( TyThing(..) )
import CmdLineOpts	( DynFlag(..) )
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: RecTcGblEnv		-- Envt to use when checking unfoldings
		-> [RenamedTyClDecl]	-- Ignore non-sig-decls in these decls
		-> TcM [Id]
		

tcInterfaceSigs unf_env decls
  = sequenceM [ do_one name ty id_infos src_loc
	      | IfaceSig {tcdName = name, tcdType = ty, 
			  tcdIdInfo = id_infos, tcdLoc =src_loc} <- decls]
  where
    in_scope_vars = []
--    in_scope_vars = filter (nameIsLocalOrFrom mod . idName) (tcEnvIds unf_env)
		-- Oops: using isLocalId instead can give a black hole
		-- because it looks at the idinfo

	-- When we have hi-boot files, an unfolding might refer to
	-- something defined in this module, so we must build a
	-- suitable in-scope set.  This thunk will only be poked
	-- if -dcore-lint is on.

    do_one name ty id_infos src_loc
      = addSrcLoc src_loc 		 		$	
	addErrCtxt (ifaceSigCtxt name)		$
	tcIfaceType ty					`thenM` \ sigma_ty ->
	tcIdInfo unf_env in_scope_vars name 
		 sigma_ty id_infos			`thenM` \ id_info ->
	returnM (mkVanillaGlobal name sigma_ty id_info)
\end{code}

\begin{code}
tcIdInfo unf_env in_scope_vars name ty info_ins
  = foldlM tcPrag init_info info_ins 
  where
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    init_info = hasCafIdInfo

    tcPrag info (HsNoCafRefs)   = returnM (info `setCafInfo`	 NoCafRefs)

    tcPrag info (HsArity arity) = 
	returnM (info `setArityInfo` arity)

    tcPrag info (HsUnfold inline_prag expr)
	= tcPragExpr unf_env name in_scope_vars expr 	`thenM` \ maybe_expr' ->
	  let
		-- maybe_expr doesn't get looked at if the unfolding
		-- is never inspected; so the typecheck doesn't even happen
		unfold_info = case maybe_expr' of
				Nothing    -> noUnfolding
				Just expr' -> mkTopUnfolding expr' 
		info1 = info `setUnfoldingInfo` unfold_info
		info2 = info1 `setInlinePragInfo` inline_prag
	  in
	  returnM info2

    tcPrag info (HsStrictness strict_info)
	= returnM (info `setAllStrictnessInfo` Just strict_info)

    tcPrag info (HsWorker nm arity)
	= tcWorkerInfo unf_env ty info nm arity
\end{code}

\begin{code}
tcWorkerInfo unf_env ty info worker_name arity
  = newUniqueSupply			`thenM` \ us ->
    let
	wrap_fn = initUs_ us (mkWrapper ty strict_sig)

	-- Watch out! We can't pull on unf_env too eagerly!
	info' = case tcLookupRecId_maybe unf_env worker_name of
		  Just worker_id -> 
		    info `setUnfoldingInfo`  mkTopUnfolding (wrap_fn worker_id)
		         `setWorkerInfo`     HasWorker worker_id arity

    		  Nothing -> pprTrace "tcWorkerInfo failed:" 
				      (ppr worker_name) info
    in
    returnM info'
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
  = forkM doc $
    setGblEnv unf_env $

    tcCoreExpr expr		`thenM` \ core_expr' ->

		-- Check for type consistency in the unfolding
    ifOptM Opt_DoCoreLinting (
	getSrcLocM		`thenM` \ src_loc -> 
	case lintUnfolding src_loc in_scope_vars core_expr' of
	  Nothing       -> returnM ()
	  Just fail_msg -> failWithTc ((doc <+> text "Failed Lint") $$ fail_msg)
    )				`thenM_`

   returnM core_expr'	
  where
    doc = text "unfolding of" <+> ppr name
\end{code}


Variables in unfoldings
~~~~~~~~~~~~~~~~~~~~~~~
****** Inside here we use only the Global environment, even for locally bound variables.
****** Why? Because we know all the types and want to bind them to real Ids.

\begin{code}
tcVar :: Name -> TcM Id
tcVar name
  = tcLookupGlobal_maybe name	`thenM` \ maybe_id ->
    case maybe_id of {
	Just (AnId id)	-> returnM id ;
	Nothing		-> failWithTc (noDecl name)
    }

noDecl name = hsep [ptext SLIT("Warning: no binding for"), ppr name]
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM CoreExpr

tcCoreExpr (UfType ty)
  = tcIfaceType ty		`thenM` \ ty' ->
	-- It might not be of kind type
    returnM (Type ty')

tcCoreExpr (UfVar name)
  = tcVar name 	`thenM` \ id ->
    returnM (Var id)

tcCoreExpr (UfLit lit)
  = returnM (Lit lit)

-- The dreaded lit-lits are also similar, except here the type
-- is read in explicitly rather than being implicit
tcCoreExpr (UfLitLit lit ty)
  = tcIfaceType ty		`thenM` \ ty' ->
    returnM (Lit (MachLitLit lit ty'))

tcCoreExpr (UfFCall cc ty)
  = tcIfaceType ty 	`thenM` \ ty' ->
    newUnique		`thenM` \ u ->
    returnM (Var (mkFCallId u cc ty'))

tcCoreExpr (UfTuple (HsTupCon boxity arity) args) 
  = mappM tcCoreExpr args	`thenM` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . exprType) args' ++ args'
    in
    returnM (mkApps (Var con_id) con_args)
  where
    con_id = dataConWorkId (tupleCon boxity arity)
    

tcCoreExpr (UfLam bndr body)
  = tcCoreLamBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Lam bndr' body')

tcCoreExpr (UfApp fun arg)
  = tcCoreExpr fun		`thenM` \ fun' ->
    tcCoreExpr arg		`thenM` \ arg' ->
    returnM (App fun' arg')

tcCoreExpr (UfCase scrut case_bndr alts) 
  = tcCoreExpr scrut					`thenM` \ scrut' ->
    let
	scrut_ty = exprType scrut'
	case_bndr' = mkLocalId case_bndr scrut_ty
    in
    tcExtendGlobalValEnv [case_bndr']	$
    mappM (tcCoreAlt scrut_ty) alts	`thenM` \ alts' ->
    returnM (Case scrut' case_bndr' alts')

tcCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = tcCoreExpr rhs		`thenM` \ rhs' ->
    tcCoreValBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Let (NonRec bndr' rhs') body')

tcCoreExpr (UfLet (UfRec pairs) body)
  = tcCoreValBndrs bndrs	$ \ bndrs' ->
    mappM tcCoreExpr rhss	`thenM` \ rhss' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Let (Rec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs

tcCoreExpr (UfNote note expr) 
  = tcCoreExpr expr		`thenM` \ expr' ->
    case note of
	UfCoerce to_ty -> tcIfaceType to_ty	`thenM` \ to_ty' ->
			  returnM (Note (Coerce to_ty'
                                                 (exprType expr')) expr')
	UfInlineCall   -> returnM (Note InlineCall expr')
	UfInlineMe     -> returnM (Note InlineMe   expr')
	UfSCC cc       -> returnM (Note (SCC cc)   expr')
\end{code}

\begin{code}
tcCoreLamBndr (UfValBinder name ty) thing_inside
  = tcIfaceType ty		`thenM` \ ty' ->
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
  = tcIfaceType ty			`thenM` \ ty' ->
    let
	id = mkLocalId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mappM tcIfaceType tys		`thenM` \ tys' ->
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
    tcCoreExpr rhs		`thenM` \ rhs' ->
    returnM (DEFAULT, [], rhs')
  
tcCoreAlt scrut_ty (UfLitAlt lit, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenM` \ rhs' ->
    returnM (LitAlt lit, [], rhs')

tcCoreAlt scrut_ty (UfLitLitAlt str ty, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenM` \ rhs' ->
    tcIfaceType ty		`thenM` \ ty' ->
    returnM (LitAlt (MachLitLit str ty'), [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcCoreAlt scrut_ty alt@(con, names, rhs)
  = tcConAlt con	`thenM` \ con ->
    let
	ex_tyvars  	  = dataConExistentialTyVars con
	(tycon, inst_tys) = splitTyConApp scrut_ty	-- NB: not tcSplitTyConApp
							-- We are looking at Core here
	main_tyvars	  = tyConTyVars tycon
	ex_tyvars'	  = [mkTyVar name (tyVarKind tv) | (name,tv) <- names `zip` ex_tyvars] 
	ex_tys'		  = mkTyVarTys ex_tyvars'
	arg_tys		  = dataConArgTys con (inst_tys ++ ex_tys')
	id_names	  = dropList ex_tyvars names
	arg_ids
#ifdef DEBUG
		| not (equalLength id_names arg_tys)
		= pprPanic "tcCoreAlts" (ppr (con, names, rhs) $$
					 (ppr main_tyvars <+> ppr ex_tyvars) $$
					 ppr arg_tys)
		| otherwise
#endif
		= zipWithEqual "tcCoreAlts" mkLocalId id_names arg_tys
    in
    ASSERT( con `elem` tyConDataCons tycon && equalLength inst_tys main_tyvars )
    tcExtendTyVarEnv ex_tyvars'			$
    tcExtendGlobalValEnv arg_ids		$
    tcCoreExpr rhs					`thenM` \ rhs' ->
    returnM (DataAlt con, ex_tyvars' ++ arg_ids, rhs')


tcConAlt :: UfConAlt Name -> TcM DataCon
tcConAlt (UfTupleAlt (HsTupCon boxity arity))
  = returnM (tupleCon boxity arity)

tcConAlt (UfDataAlt con_name)
  = tcVar con_name	`thenM` \ con_id ->
    returnM (case isDataConWrapId_maybe con_id of
		    Just con -> con
		    Nothing  -> pprPanic "tcCoreAlt" (ppr con_id))
\end{code}

%************************************************************************
%*									*
\subsection{Core decls}
%*									*
%************************************************************************


\begin{code}
tcCoreBinds :: [RenamedCoreDecl] -> TcM [TypecheckedCoreBind]
-- We don't assume the bindings are in dependency order
-- So first build the environment, then check the RHSs
tcCoreBinds ls = mappM tcCoreBinder ls		`thenM` \ bndrs ->
		 tcExtendGlobalValEnv bndrs	$
		 mappM (tcCoreBind bndrs) ls

tcCoreBinder (CoreDecl nm ty _ _)
 = kcHsSigType ty	`thenM_`
   tcIfaceType ty 	`thenM` \ ty' ->
   returnM (mkLocalId nm ty')

tcCoreBind bndrs (CoreDecl nm _ rhs loc)
 = tcVar nm		`thenM` \ id ->
   tcCoreExpr rhs	`thenM` \ rhs' ->
   let
	mb_err = lintUnfolding loc bndrs rhs'
   in
   (case mb_err of
	Just err -> addErr err
	Nothing  -> returnM ())	`thenM_`

   returnM (id, rhs')
\end{code}


\begin{code}
ifaceSigCtxt sig_name
  = hsep [ptext SLIT("In an interface-file signature for"), ppr sig_name]
\end{code}

