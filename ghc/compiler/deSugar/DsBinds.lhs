%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
#include "HsVersions.h"

module DsBinds ( dsBinds ) where

IMP_Ubiq()
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(DsLoop)		-- break dsExpr-ish loop
#else
import {-# SOURCE #-} DsExpr
#endif

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils	( coreExprType )
import TcHsSyn		( SYN_IE(TypecheckedHsBinds), SYN_IE(TypecheckedHsExpr),
			  SYN_IE(TypecheckedMonoBinds),
			  SYN_IE(TypecheckedPat)
			)
import DsMonad
import DsGRHSs		( dsGuarded )
import DsUtils
import Match		( matchWrapper )

import BasicTypes       ( SYN_IE(Module) )
import CmdLineOpts	( opt_SccProfilingOn, opt_AutoSccsOnAllToplevs, 
			  opt_AutoSccsOnExportedToplevs, opt_CompilingGhcInternals )
import CostCentre	( mkAutoCC, IsCafCC(..), mkAllDictsCC, preludeDictsCostCentre )
import Id		( idType, SYN_IE(DictVar), GenId, SYN_IE(Id) )
import ListSetOps	( minusList, intersectLists )
import Name		( isExported )
import PprType		( GenType )
import Outputable	( PprStyle(..) )
import Type		( mkTyVarTy, isDictTy, instantiateTy
			)
import TyVar		( tyVarSetToList, GenTyVar{-instance Eq-} )
import TysPrim		( voidTy )
import Util		( isIn, panic, assertPanic  )
\end{code}

%************************************************************************
%*									*
\subsection[toplevel-and-regular-DsBinds]{Regular and top-level @dsBinds@}
%*									*
%************************************************************************

Like @dsBinds@, @dsBind@ returns a @[CoreBinding]@, but it may be
that some of the binders are of unboxed type.  This is sorted out when
the caller wraps the bindings round an expression.

\begin{code}
type Group = FAST_STRING

dsBinds :: Maybe (Module, Group) -> TypecheckedHsBinds -> DsM [CoreBinding]

dsBinds _ EmptyBinds	  	     = returnDs []
dsBinds mb_mod_grp (ThenBinds binds_1 binds_2) 
  = andDs (++) (dsBinds mb_mod_grp binds_1) (dsBinds mb_mod_grp binds_2)

dsBinds mb_mod_grp (MonoBind binds sigs is_rec)
  = dsMonoBinds mb_mod_grp is_rec binds  `thenDs` \ prs ->
    returnDs (if is_rec then
		[Rec prs]
	      else
		[NonRec binder rhs | (binder,rhs) <- prs]
    )
\end{code}


%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsMonoBinds :: Maybe (Module, Group)   -- Nothing => don't (auto-)annotate scc on toplevs.
	    -> RecFlag 
	    -> TypecheckedMonoBinds 
	    -> DsM [(Id,CoreExpr)]

dsMonoBinds _ is_rec EmptyMonoBinds = returnDs []

dsMonoBinds mb_mod_grp is_rec (AndMonoBinds  binds_1 binds_2)
  = andDs (++) (dsMonoBinds mb_mod_grp is_rec binds_1) (dsMonoBinds mb_mod_grp is_rec binds_2)

dsMonoBinds _ is_rec (CoreMonoBind var core_expr)
  = returnDs [(var, core_expr)]

dsMonoBinds _ is_rec (VarMonoBind var expr)
  = dsExpr expr			`thenDs` \ core_expr ->

	-- Dictionary bindings are always VarMonoBinds, so
	-- we only need do this here
    addDictScc var core_expr	`thenDs` \ core_expr' ->

    returnDs [(var, core_expr')]

dsMonoBinds mb_mod_grp is_rec (FunMonoBind fun _ matches locn)
  = putSrcLocDs locn	$
    matchWrapper (FunMatch fun) matches error_string	`thenDs` \ (args, body) ->
    returnDs [addAutoScc mb_mod_grp (fun, mkValLam args body)]
  where
    error_string = "function " ++ showForErr fun

dsMonoBinds mb_mod_grp is_rec (PatMonoBind pat grhss_and_binds locn)
  = putSrcLocDs locn $
    dsGuarded grhss_and_binds		`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr

	-- Common special case: no type or dictionary abstraction
dsMonoBinds mb_mod_grp is_rec (AbsBinds [] [] exports binds)
  = dsMonoBinds Nothing is_rec binds			`thenDs` \ prs ->
    returnDs (prs ++ [ addAutoScc mb_mod_grp (global, Var local) | (_, global, local) <- exports])

	-- Another common case: one exported variable
	-- All non-recursive bindings come through this way
dsMonoBinds mb_mod_grp is_rec (AbsBinds all_tyvars dicts [(tyvars, global, local)] binds)
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsMonoBinds Nothing is_rec binds			`thenDs` \ core_prs ->
    let 
	core_binds | is_rec    = [Rec core_prs]
		   | otherwise = [NonRec b e | (b,e) <- core_prs]
    in
    returnDs [addAutoScc mb_mod_grp (global, mkLam tyvars dicts $ 
					     mkCoLetsAny core_binds (Var local))]

dsMonoBinds mb_mod_grp is_rec (AbsBinds all_tyvars dicts exports binds)
  = dsMonoBinds Nothing is_rec binds			`thenDs` \ core_prs ->
    let 
	core_binds | is_rec    = [Rec core_prs]
		   | otherwise = [NonRec b e | (b,e) <- core_prs]

	tup_expr = mkLam all_tyvars dicts $
		   mkCoLetsAny core_binds $
		   mkTupleExpr locals
	locals    = [local | (_, _, local) <- exports]
	local_tys = map idType locals
    in
    newSysLocalDs (coreExprType tup_expr)		`thenDs` \ tup_id ->
    let
	dict_args    = map VarArg dicts

	mk_bind (tyvars, global, local) n	-- locals !! n == local
	  = 	-- Need to make fresh locals to bind in the selector, because
		-- some of the tyvars will be bound to voidTy
	    newSysLocalsDs (map (instantiateTy env) local_tys) 	`thenDs` \ locals' ->
	    returnDs (addAutoScc mb_mod_grp $
			(global, mkLam tyvars dicts $
		     	         mkTupleSelector locals' (locals' !! n) $
		     	         mkValApp (mkTyApp (Var tup_id) ty_args) dict_args))
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args = map mk_ty_arg all_tyvars
	    env     = all_tyvars `zip` ty_args
    in
    zipWithDs mk_bind exports [0..]		`thenDs` \ export_binds ->
     -- don't scc (auto-)annotate the tuple itself.
    returnDs ((tup_id, tup_expr) : export_binds)
\end{code}


%************************************************************************
%*									*
\subsection[addAutoScc]{Adding automatic sccs}
%*									*
%************************************************************************

\begin{code}
addAutoScc :: Maybe (Module, Group)	-- Module and group
	   -> (Id, CoreExpr)
	   -> (Id, CoreExpr)

addAutoScc mb_mod_grp pair@(bndr, core_expr) 
  = case mb_mod_grp of
      Just (mod,grp) 
       | worthSCC core_expr &&
         (opt_AutoSccsOnAllToplevs ||
          (isExported bndr && opt_AutoSccsOnExportedToplevs))
        -> (bndr, SCC (mkAutoCC bndr mod grp IsNotCafCC) core_expr)
      _ -> pair -- no auto-annotation.

worthSCC (SCC _ _) = False
worthSCC (Con _ _) = False
worthSCC core_expr = True
\end{code}

If profiling and dealing with a dict binding, wrap the dict in "_scc_ DICT <dict>":

\begin{code}
addDictScc var rhs
  | not ( opt_SccProfilingOn || opt_AutoSccsOnAllToplevs)
	    -- the latter is so that -unprof-auto-scc-all adds dict sccs
    || not (isDictTy (idType var))
  = returnDs rhs				-- That's easy: do nothing

  | opt_CompilingGhcInternals
  = returnDs (SCC prel_dicts_cc rhs)

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->

	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (SCC (mkAllDictsCC mod grp False) rhs)

prel_dicts_cc = preludeDictsCostCentre False{-not dupd-} -- ditto
\end{code}
