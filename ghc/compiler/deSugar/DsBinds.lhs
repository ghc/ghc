%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
#include "HsVersions.h"

module DsBinds ( dsBinds, dsMonoBinds ) where

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
			  opt_AutoSccsOnExportedToplevs
		        )
import CostCentre	( mkAutoCC, IsCafCC(..), mkAllDictsCC, preludeDictsCostCentre )
import Id		( idType, SYN_IE(DictVar), GenId, SYN_IE(Id) )
--ToDo: rm import ListSetOps	( minusList, intersectLists )
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

dsBinds :: Bool   -- if candidate, auto add scc's on toplevs ?
	-> TypecheckedHsBinds 
	-> DsM [CoreBinding]

dsBinds _ EmptyBinds	  	     = returnDs []
dsBinds auto_scc (ThenBinds binds_1 binds_2) 
  = andDs (++) (dsBinds auto_scc binds_1) (dsBinds auto_scc binds_2)

dsBinds auto_scc (MonoBind binds sigs is_rec)
  = dsMonoBinds auto_scc is_rec binds []  `thenDs` \ prs ->
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
dsMonoBinds :: Bool		-- False => don't (auto-)annotate scc on toplevs.
	    -> RecFlag 
	    -> TypecheckedMonoBinds
	    -> [(Id,CoreExpr)]		-- Put this on the end (avoid quadratic append)
	    -> DsM [(Id,CoreExpr)]	-- Result

dsMonoBinds _ is_rec EmptyMonoBinds rest = returnDs rest

dsMonoBinds auto_scc is_rec (AndMonoBinds  binds_1 binds_2) rest
  = dsMonoBinds auto_scc is_rec binds_2 rest	`thenDs` \ rest' ->
    dsMonoBinds auto_scc is_rec binds_1 rest'

dsMonoBinds _ is_rec (CoreMonoBind var core_expr) rest
  = returnDs ((var, core_expr) : rest)

dsMonoBinds _ is_rec (VarMonoBind var expr) rest
  = dsExpr expr			`thenDs` \ core_expr ->

	-- Dictionary bindings are always VarMonoBinds, so
	-- we only need do this here
    addDictScc var core_expr	`thenDs` \ core_expr' ->

    returnDs ((var, core_expr') : rest)

dsMonoBinds auto_scc is_rec (FunMonoBind fun _ matches locn) rest
  = putSrcLocDs locn	$
    matchWrapper (FunMatch fun) matches error_string	`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkValLam args body)       `thenDs` \ pair ->
    returnDs (pair : rest)
  where
    error_string = "function " ++ showForErr fun

dsMonoBinds _ is_rec (PatMonoBind pat grhss_and_binds locn) rest
  = putSrcLocDs locn $
    dsGuarded grhss_and_binds		`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common special case: no type or dictionary abstraction
dsMonoBinds auto_scc is_rec (AbsBinds [] [] exports binds) rest
  = mapDs (addAutoScc auto_scc) [(global, Var local) | (_, global, local) <- exports] `thenDs` \ exports' ->
    dsMonoBinds False is_rec binds (exports' ++ rest)

	-- Another common case: one exported variable
	-- All non-recursive bindings come through this way
dsMonoBinds auto_scc is_rec (AbsBinds all_tyvars dicts [(tyvars, global, local)] binds) rest
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsMonoBinds False is_rec binds []			`thenDs` \ core_prs ->
    let 
	core_binds | is_rec    = [Rec core_prs]
		   | otherwise = [NonRec b e | (b,e) <- core_prs]
    in
    addAutoScc auto_scc (global, mkLam tyvars dicts $ 
			         mkCoLetsAny core_binds (Var local)) `thenDs` \ global' ->
    returnDs (global' : rest)

dsMonoBinds auto_scc is_rec (AbsBinds all_tyvars dicts exports binds) rest
  = dsMonoBinds False is_rec binds []			`thenDs` \ core_prs ->
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
	    addAutoScc auto_scc
		       (global, mkLam tyvars dicts $
		     	        mkTupleSelector locals' (locals' !! n) $
		     	        mkValApp (mkTyApp (Var tup_id) ty_args) dict_args)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args = map mk_ty_arg all_tyvars
	    env     = all_tyvars `zip` ty_args
    in
    zipWithDs mk_bind exports [0..]		`thenDs` \ export_binds ->
     -- don't scc (auto-)annotate the tuple itself.
    returnDs ((tup_id, tup_expr) : (export_binds ++ rest))
\end{code}


%************************************************************************
%*									*
\subsection[addAutoScc]{Adding automatic sccs}
%*									*
%************************************************************************

\begin{code}
addAutoScc :: Bool		-- if needs be, decorate toplevs?
	   -> (Id, CoreExpr)
	   -> DsM (Id, CoreExpr)

addAutoScc auto_scc_candidate pair@(bndr, core_expr) 
 | auto_scc_candidate && worthSCC core_expr && 
   (opt_AutoSccsOnAllToplevs || (isExported bndr && opt_AutoSccsOnExportedToplevs))
     = getModuleAndGroupDs `thenDs` \ (mod,grp) ->
       returnDs (bndr, SCC (mkAutoCC bndr mod grp IsNotCafCC) core_expr)
 | otherwise 
     = returnDs pair

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

{-
  | opt_CompilingGhcInternals
  = returnDs (SCC prel_dicts_cc rhs)
-}

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->

	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (SCC (mkAllDictsCC mod grp False) rhs)

{- UNUSED:
prel_dicts_cc = preludeDictsCostCentre False{-not dupd-} -- ditto
-}
\end{code}
