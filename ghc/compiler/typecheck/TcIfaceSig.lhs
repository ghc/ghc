%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIfaceSig ( tcInterfaceSigs ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), IfaceSig(..) )
import TcMonad
import TcMonoType	( tcHsType, tcHsTypeKind )
import TcEnv		( tcExtendTyVarEnv, tcExtendGlobalValEnv,
			  tcLookupTyConByKey, tcLookupGlobalValueMaybe,
			  tcExplicitLookupGlobal
			)
import TcKind		( TcKind, kindToTcKind )

import RnHsSyn		( RenamedHsDecl(..) )
import HsCore
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import Literal		( Literal(..) )
import CoreSyn
import CoreUtils	( coreExprType )
import CoreUnfold
import MagicUFs		( MagicUnfoldingFun )
import WwLib		( mkWrapper )
import PrimOp		( PrimOp(..) )

import Id		( GenId, mkImported, mkUserId, addInlinePragma,
			  isPrimitiveId_maybe, dataConArgTys, Id )
import Type		( mkSynTy, splitAlgTyConApp )
import TyVar		( mkSysTyVar )
import Name		( Name )
import Unique		( rationalTyConKey, uniqueOf )
import TysWiredIn	( integerTy )
import PragmaInfo	( PragmaInfo(..) )
import ErrUtils		( pprBagOfErrors )
import Maybes		( maybeToBool )
import Outputable	
import Util		( zipWithEqual )

import IdInfo
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: TcEnv s		-- Envt to use when checking unfoldings
		-> [RenamedHsDecl]	-- Ignore non-sig-decls in these decls
		-> TcM s [Id]
		

tcInterfaceSigs unf_env (SigD (IfaceSig name ty id_infos src_loc) : rest)
  = tcAddSrcLoc src_loc (
    tcAddErrCtxt (ifaceSigCtxt name) (
	tcHsType ty						`thenTc` \ sigma_ty ->
	tcIdInfo unf_env name sigma_ty noIdInfo id_infos	`thenTc` \ id_info' ->
	let
	    imp_id = mkImported name sigma_ty id_info'
	    sig_id | any inline_please id_infos = addInlinePragma imp_id
	           | otherwise	 	        = imp_id

	    inline_please (HsUnfold inline _) = inline
	    inline_please other		  = False
	in
	returnTc sig_id
    ))						`thenTc` \ sig_id ->
    tcInterfaceSigs unf_env rest		`thenTc` \ sig_ids ->
    returnTc (sig_id : sig_ids)

tcInterfaceSigs unf_env (other_decl : rest) = tcInterfaceSigs unf_env rest

tcInterfaceSigs unf_env [] = returnTc []
\end{code}

\begin{code}
tcIdInfo unf_env name ty info info_ins
  = go noIdInfo info_ins
  where
    go info_so_far []		   = returnTc info_so_far
    go info (HsArity arity : rest) = go (info `addArityInfo` arity) rest
    go info (HsUpdate upd : rest)  = go (info `addUpdateInfo` upd)  rest
    go info (HsFBType fb : rest)   = go (info `addFBTypeInfo` fb)   rest
    go info (HsArgUsage au : rest) = go (info `addArgUsageInfo` au) rest

    go info (HsUnfold inline expr : rest) = tcUnfolding unf_env name expr 	`thenNF_Tc` \ unfold_info ->
					    go (info `addUnfoldInfo` unfold_info) rest

    go info (HsStrictness strict : rest)  = tcStrictness unf_env ty info strict	`thenTc` \ info' ->
					    go info' rest
\end{code}

\begin{code}
tcStrictness unf_env ty info (HsStrictnessInfo demands maybe_worker)
  = tcWorker unf_env maybe_worker		`thenNF_Tc` \ maybe_worker_id ->
    uniqSMToTcM (mkWrapper ty demands)		`thenNF_Tc` \ wrap_fn ->
    let
	-- Watch out! We can't pull on maybe_worker_id too eagerly!
	info' = case maybe_worker_id of
			Just worker_id -> info `addUnfoldInfo` mkUnfolding NoPragmaInfo (wrap_fn worker_id)
			Nothing        -> info
	has_worker = maybeToBool maybe_worker_id
    in
    returnTc (info' `addStrictnessInfo` StrictnessInfo demands has_worker)

-- Boring to write these out, but the result type differs from the arg type...
tcStrictness unf_env ty info HsBottom
  = returnTc (info `addStrictnessInfo` BottomGuaranteed)
\end{code}

\begin{code}
tcWorker unf_env Nothing = returnNF_Tc Nothing

tcWorker unf_env (Just (worker_name,_))
  = returnNF_Tc (trace_maybe maybe_worker_id)
  where
    maybe_worker_id = tcExplicitLookupGlobal unf_env worker_name

	-- The trace is so we can see what's getting dropped
    trace_maybe Nothing  = pprTrace "tcWorker failed:" (ppr worker_name) Nothing
    trace_maybe (Just x) = Just x
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcUnfolding unf_env name core_expr
  = forkNF_Tc (
	recoverNF_Tc no_unfolding (
		tcSetEnv unf_env $
		tcCoreExpr core_expr	`thenTc` \ core_expr' ->
		returnTc (mkUnfolding NoPragmaInfo core_expr')
    ))			
  where
	-- The trace tells what wasn't available, for the benefit of
	-- compiler hackers who want to improve it!
    no_unfolding = getErrsTc		`thenNF_Tc` \ (warns,errs) ->
		   returnNF_Tc (pprTrace "tcUnfolding failed with:" 
				   	(hang (ppr name) 4 (pprBagOfErrors errs))
					NoUnfolding)
\end{code}


Variables in unfoldings
~~~~~~~~~~~~~~~~~~~~~~~
****** Inside here we use only the Global environment, even for locally bound variables.
****** Why? Because we know all the types and want to bind them to real Ids.

\begin{code}
tcVar :: Name -> TcM s Id
tcVar name
  = tcLookupGlobalValueMaybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of {
	Just id -> returnTc id;
	Nothing -> failWithTc (noDecl name)
    }

noDecl name = hsep [ptext SLIT("Warning: no binding for"), ppr name]
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM s CoreExpr

tcCoreExpr (UfVar name)
  = tcVar name 	`thenTc` \ id ->
    returnTc (Var id)

-- rationalTy isn't built in so we have to construct it
-- (the "ty" part of the incoming literal is simply bottom)
tcCoreExpr (UfLit (NoRepRational lit _)) 
  = tcLookupTyConByKey rationalTyConKey	`thenNF_Tc` \ rational_tycon ->
    let
	rational_ty  = mkSynTy rational_tycon []
    in
    returnTc (Lit (NoRepRational lit rational_ty)) 

-- Similarly for integers, except that it is wired in
tcCoreExpr (UfLit (NoRepInteger lit _)) 
  = returnTc (Lit (NoRepInteger lit integerTy))

tcCoreExpr (UfLit other_lit)
  = returnTc (Lit other_lit)

tcCoreExpr (UfCon con args) 
  = tcVar con			`thenTc` \ con_id ->
    mapTc tcCoreArg args	`thenTc` \ args' ->
    returnTc (Con con_id args')

tcCoreExpr (UfPrim prim args) 
  = tcCorePrim prim		`thenTc` \ primop ->
    mapTc tcCoreArg args	`thenTc` \ args' ->
    returnTc (Prim primop args')

tcCoreExpr (UfApp fun arg)
  = tcCoreExpr fun		`thenTc` \ fun' ->
    tcCoreArg arg		`thenTc` \ arg' ->
    returnTc (App fun' arg')

tcCoreExpr (UfCase scrut alts) 
  = tcCoreExpr scrut				`thenTc` \ scrut' ->
    tcCoreAlts (coreExprType scrut') alts	`thenTc` \ alts' ->
    returnTc (Case scrut' alts')

tcCoreExpr (UfSCC cc expr) 
  = tcCoreExpr expr		`thenTc` \ expr' ->
    returnTc  (SCC cc expr') 

tcCoreExpr(UfCoerce coercion ty body)
  = tcCoercion coercion		`thenTc` \ coercion' ->
    tcHsTypeKind ty		`thenTc` \ (_,ty') ->
    tcCoreExpr body		`thenTc` \ body' ->
    returnTc (Coerce coercion' ty' body')

tcCoreExpr (UfLam bndr body)
  = tcCoreLamBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenTc` \ body' ->
    returnTc (Lam bndr' body')

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
\end{code}

\begin{code}
tcCoreLamBndr (UfValBinder name ty) thing_inside
  = tcHsType ty			`thenTc` \ ty' ->
    let
	id = mkUserId name ty' NoPragmaInfo
    in
    tcExtendGlobalValEnv [id] $
    thing_inside (ValBinder id)
    
tcCoreLamBndr (UfTyBinder name kind) thing_inside
  = let
	tyvar = mkSysTyVar (uniqueOf name) kind
    in
    tcExtendTyVarEnv [name] [(kindToTcKind kind, tyvar)] $
    thing_inside (TyBinder tyvar)
    
tcCoreValBndr (UfValBinder name ty) thing_inside
  = tcHsType ty			`thenTc` \ ty' ->
    let
	id = mk_id name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapTc tcHsType tys			`thenTc` \ tys' ->
    let
	ids = zipWithEqual "tcCoreValBndr" mk_id names tys'
    in
    tcExtendGlobalValEnv ids $
    thing_inside ids
  where
    names = map (\ (UfValBinder name _) -> name) bndrs
    tys   = map (\ (UfValBinder _   ty) -> ty)   bndrs

mk_id name ty = mkUserId name ty NoPragmaInfo
\end{code}    

\begin{code}
tcCoreArg (UfVarArg v)	 = tcVar v 		`thenTc` \ v' -> returnTc (VarArg v')
tcCoreArg (UfTyArg ty)	 = tcHsTypeKind ty	`thenTc` \ (_,ty') -> returnTc (TyArg ty')
tcCoreArg (UfLitArg lit) = returnTc (LitArg lit)

tcCoreAlts scrut_ty (UfAlgAlts alts deflt)
  = mapTc tc_alt alts			`thenTc` \ alts' ->
    tcCoreDefault scrut_ty deflt	`thenTc` \ deflt' ->
    returnTc (AlgAlts alts' deflt')
  where
    tc_alt (con, names, rhs)
      =	tcVar con			`thenTc` \ con' ->
	let
	    arg_tys		    = dataConArgTys con' inst_tys
	    (tycon, inst_tys, cons) = splitAlgTyConApp scrut_ty
	    arg_ids		    = zipWithEqual "tcCoreAlts" mk_id names arg_tys
	in
	tcExtendGlobalValEnv arg_ids 	$
	tcCoreExpr rhs			`thenTc` \ rhs' ->
	returnTc (con', arg_ids, rhs')

tcCoreAlts scrut_ty (UfPrimAlts alts deflt)
  = mapTc tc_alt alts			`thenTc` \ alts' ->
    tcCoreDefault scrut_ty deflt	`thenTc` \ deflt' ->
    returnTc (PrimAlts alts' deflt')
  where
    tc_alt (lit, rhs) =	tcCoreExpr rhs		`thenTc` \ rhs' ->
			returnTc (lit, rhs')

tcCoreDefault scrut_ty UfNoDefault = returnTc NoDefault
tcCoreDefault scrut_ty (UfBindDefault name rhs)
  = let
	deflt_id           = mk_id name scrut_ty
    in
    tcExtendGlobalValEnv [deflt_id] 	$
    tcCoreExpr rhs			`thenTc` \ rhs' ->
    returnTc (BindDefault deflt_id rhs')
    

tcCoercion (UfIn  n) = tcVar n `thenTc` \ n' -> returnTc (CoerceIn  n')
tcCoercion (UfOut n) = tcVar n `thenTc` \ n' -> returnTc (CoerceOut n')

tcCorePrim (UfOtherOp op) 
  = tcVar op		`thenTc` \ op_id ->
    case isPrimitiveId_maybe op_id of
	Just prim_op -> returnTc prim_op
	Nothing	     -> pprPanic "tcCorePrim" (ppr op_id)

tcCorePrim (UfCCallOp str casm gc arg_tys res_ty)
  = mapTc tcHsType arg_tys	`thenTc` \ arg_tys' ->
    tcHsType res_ty		`thenTc` \ res_ty' ->
    returnTc (CCallOp str casm gc arg_tys' res_ty')
\end{code}

\begin{code}
ifaceSigCtxt sig_name
  = hsep [ptext SLIT("In an interface-file signature for"), ppr sig_name]
\end{code}

