%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
#include "HsVersions.h"

module TcIfaceSig ( tcInterfaceSigs ) where

IMP_Ubiq()

import TcMonad
import TcMonoType	( tcHsType )
import TcEnv		( tcLookupGlobalValue, tcExtendTyVarEnv, tcExtendGlobalValEnv )
import TcKind		( TcKind, kindToTcKind )

import HsSyn		( IfaceSig(..), HsDecl(..), TyDecl, ClassDecl, InstDecl, DefaultDecl, HsBinds,
			  Fake, InPat, HsType )
import RnHsSyn		( RenamedHsDecl(..) )
import HsCore
import HsDecls		( HsIdInfo(..) )
import CoreSyn
import CoreUnfold
import MagicUFs		( MagicUnfoldingFun )
import SpecEnv		( SpecEnv )
import PrimOp		( PrimOp(..) )

import Id		( GenId, mkImported, mkUserId, isPrimitiveId_maybe )
import TyVar		( mkTyVar )
import Name		( Name )
import PragmaInfo	( PragmaInfo(..) )
import Maybes		( maybeToBool )
import Pretty
import PprStyle		( PprStyle(..) )
import Util		( zipWithEqual, panic, pprTrace, pprPanic )

import IdInfo
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: [RenamedHsDecl] -> TcM s [Id]
		   -- Ignore non-sig-decls in these decls

tcInterfaceSigs (SigD (IfaceSig name ty id_infos src_loc) : rest)
  = tcAddSrcLoc src_loc $
    tcHsType ty				`thenTc` \ sigma_ty ->
    tcIdInfo name noIdInfo id_infos	`thenTc` \ id_info' ->
    let
	sig_id = mkImported name sigma_ty id_info'
    in
    tcInterfaceSigs rest		`thenTc` \ sig_ids ->
    returnTc (sig_id : sig_ids)

tcInterfaceSigs (other_decl : rest) = tcInterfaceSigs rest

tcInterfaceSigs [] = returnTc []
\end{code}

Inside here we use only the Global environment, even for locally bound variables.
Why? Because we know all the types and want to bind them to real Ids.

\begin{code}
tcIdInfo name info [] = returnTc info

tcIdInfo name info (HsArity arity : rest)
  = tcIdInfo name (info `addArityInfo` arity) rest

tcIdInfo name info (HsUpdate upd : rest)
  = tcIdInfo name (info `addUpdateInfo` upd) rest

tcIdInfo name info (HsFBType fb : rest)
  = tcIdInfo name (info `addFBTypeInfo` fb) rest

tcIdInfo name info (HsArgUsage au : rest)
  = tcIdInfo name (info `addArgUsageInfo` au) rest

tcIdInfo name info (HsDeforest df : rest)
  = tcIdInfo name (info `addDeforestInfo` df) rest

tcIdInfo name info (HsUnfold expr : rest)
  = tcUnfolding name expr 	`thenNF_Tc` \ unfold_info ->
    tcIdInfo name (info `addUnfoldInfo` unfold_info) rest

tcIdInfo name info (HsStrictness strict : rest)
  = tcStrictness strict 	`thenTc` \ strict_info ->
    tcIdInfo name (info `addStrictnessInfo` strict_info) rest
\end{code}

\begin{code}
tcStrictness (StrictnessInfo demands (Just worker))
  = tcLookupGlobalValue worker		`thenNF_Tc` \ worker_id ->
    returnTc (StrictnessInfo demands (Just worker_id))

-- Boring to write these out, but the result type differe from the arg type...
tcStrictness (StrictnessInfo demands Nothing) = returnTc (StrictnessInfo demands Nothing)
tcStrictness NoStrictnessInfo		      = returnTc NoStrictnessInfo
tcStrictness BottomGuaranteed		      = returnTc BottomGuaranteed
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcUnfolding name core_expr
  = forkNF_Tc (
	recoverNF_Tc (returnNF_Tc no_unfolding) (
		tcCoreExpr core_expr	`thenTc` \ core_expr' ->
		returnTc (mkUnfolding False core_expr')
    ))			
  where
    no_unfolding = pprTrace "tcUnfolding failed:" (ppr PprDebug name) NoUnfolding
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM s CoreExpr

tcCoreExpr (UfVar name)
  = tcLookupGlobalValue name 	`thenNF_Tc` \ id ->
    returnTc (Var id)

tcCoreExpr (UfLit lit) = returnTc (Lit lit)

tcCoreExpr (UfCon con args) 
  = tcLookupGlobalValue con	`thenNF_Tc` \ con_id ->
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
  = tcCoreExpr scrut		`thenTc` \ scrut' ->
    tcCoreAlts alts		`thenTc` \ alts' ->
    returnTc (Case scrut' alts')

tcCoreExpr (UfSCC cc expr) 
  = tcCoreExpr expr		`thenTc` \ expr' ->
    returnTc  (SCC cc expr') 

tcCoreExpr(UfCoerce coercion ty body)
  = tcCoercion coercion		`thenTc` \ coercion' ->
    tcHsType ty			`thenTc` \ ty' ->
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
	tyvar = mkTyVar name kind
    in
    tcExtendTyVarEnv [name] [(kindToTcKind kind, tyvar)] $
    thing_inside (TyBinder tyvar)
    
tcCoreLamBndr (UfUsageBinder name) thing_inside
  = error "tcCoreLamBndr: usage"

tcCoreValBndr (UfValBinder name ty) thing_inside
  = tcHsType ty			`thenTc` \ ty' ->
    let
	id = mkUserId name ty' NoPragmaInfo
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapTc tcHsType tys			`thenTc` \ tys' ->
    let
	ids = zipWithEqual "tcCoreValBndr" mk_id names tys'
	mk_id name ty' = mkUserId name ty' NoPragmaInfo
    in
    tcExtendGlobalValEnv ids $
    thing_inside ids
  where
    names = map (\ (UfValBinder name _) -> name) bndrs
    tys   = map (\ (UfValBinder _   ty) -> ty)   bndrs
\end{code}    

\begin{code}
tcCoreArg (UfVarArg v)	 = tcLookupGlobalValue v  `thenNF_Tc` \ v' -> returnTc (VarArg v')
tcCoreArg (UfTyArg ty)	 = tcHsType ty		  `thenTc` \ ty' -> returnTc (TyArg ty')
tcCoreArg (UfLitArg lit) = returnTc (LitArg lit)
tcCoreArg (UfUsageArg u) = error "tcCoreArg: usage"

tcCoreAlts (UfAlgAlts alts deflt)
  = mapTc tc_alt alts		`thenTc` \ alts' ->
    tcCoreDefault deflt		`thenTc` \ deflt' ->
    returnTc (AlgAlts alts' deflt')
  where
    tc_alt (con, bndrs, rhs) =	tcLookupGlobalValue con		`thenNF_Tc` \ con' ->
				tcCoreValBndrs bndrs		$ \ bndrs' ->
				tcCoreExpr rhs			`thenTc` \ rhs' ->
				returnTc (con', bndrs', rhs')

tcCoreAlts (UfPrimAlts alts deflt)
  = mapTc tc_alt alts		`thenTc` \ alts' ->
    tcCoreDefault deflt		`thenTc` \ deflt' ->
    returnTc (PrimAlts alts' deflt')
  where
    tc_alt (lit, rhs) =	tcCoreExpr rhs		`thenTc` \ rhs' ->
			returnTc (lit, rhs')

tcCoreDefault UfNoDefault = returnTc NoDefault
tcCoreDefault (UfBindDefault bndr rhs) = tcCoreValBndr bndr	$ \ bndr' ->
					 tcCoreExpr rhs		`thenTc` \ rhs' ->
				  	 returnTc (BindDefault bndr' rhs')

tcCoercion (UfIn  n) = tcLookupGlobalValue n `thenNF_Tc` \ n' -> returnTc (CoerceIn  n')
tcCoercion (UfOut n) = tcLookupGlobalValue n `thenNF_Tc` \ n' -> returnTc (CoerceOut n')

tcCorePrim (UfOtherOp op) 
  = tcLookupGlobalValue op	`thenNF_Tc` \ op_id ->
    case isPrimitiveId_maybe op_id of
	Just prim_op -> returnTc prim_op
	Nothing	     -> pprPanic "tcCorePrim" (ppr PprDebug op_id)

tcCorePrim (UfCCallOp str casm gc arg_tys res_ty)
  = mapTc tcHsType arg_tys	`thenTc` \ arg_tys' ->
    tcHsType res_ty		`thenTc` \ res_ty' ->
    returnTc (CCallOp str casm gc arg_tys' res_ty')
\end{code}

