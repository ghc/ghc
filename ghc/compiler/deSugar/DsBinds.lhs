%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsMonoBinds ) where

#include "HsVersions.h"


import {-# SOURCE #-}	DsExpr( dsExpr )

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils	( coreExprType )
import TcHsSyn		( TypecheckedMonoBinds )
import DsMonad
import DsGRHSs		( dsGuarded )
import DsUtils
import Match		( matchWrapper )

import BasicTypes       ( Module, RecFlag(..) )
import CmdLineOpts	( opt_SccProfilingOn, opt_AutoSccsOnAllToplevs, 
			  opt_AutoSccsOnExportedToplevs
		        )
import CostCentre	( mkAutoCC, IsCafCC(..), mkAllDictsCC )
import Id		( idType, Id )
import VarEnv
import Name		( isExported )
import Type		( mkTyVarTy, isDictTy, substTy
			)
import TysWiredIn	( voidTy )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsMonoBinds :: Bool		-- False => don't (auto-)annotate scc on toplevs.
	    -> TypecheckedMonoBinds
	    -> [(Id,CoreExpr)]		-- Put this on the end (avoid quadratic append)
	    -> DsM [(Id,CoreExpr)]	-- Result

dsMonoBinds _ EmptyMonoBinds rest = returnDs rest

dsMonoBinds auto_scc (AndMonoBinds  binds_1 binds_2) rest
  = dsMonoBinds auto_scc binds_2 rest	`thenDs` \ rest' ->
    dsMonoBinds auto_scc binds_1 rest'

dsMonoBinds _ (CoreMonoBind var core_expr) rest
  = returnDs ((var, core_expr) : rest)

dsMonoBinds _ (VarMonoBind var expr) rest
  = dsExpr expr			`thenDs` \ core_expr ->

	-- Dictionary bindings are always VarMonoBinds, so
	-- we only need do this here
    addDictScc var core_expr	`thenDs` \ core_expr' ->

    returnDs ((var, core_expr') : rest)

dsMonoBinds auto_scc (FunMonoBind fun _ matches locn) rest
  = putSrcLocDs locn	$
    matchWrapper (FunMatch fun) matches error_string	`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkLams args body)		`thenDs` \ pair ->
    returnDs (pair : rest)
  where
    error_string = "function " ++ showSDoc (ppr fun)

dsMonoBinds _ (PatMonoBind pat grhss_and_binds locn) rest
  = putSrcLocDs locn $
    dsGuarded grhss_and_binds		`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common special case: no type or dictionary abstraction
dsMonoBinds auto_scc (AbsBinds [] [] exports binds) rest
  = mapDs (addAutoScc auto_scc) [(global, Var local) | (_, global, local) <- exports] `thenDs` \ exports' ->
    dsMonoBinds False binds (exports' ++ rest)

	-- Another common case: one exported variable
	-- All non-recursive bindings come through this way
dsMonoBinds auto_scc (AbsBinds all_tyvars dicts [(tyvars, global, local)] binds) rest
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsMonoBinds False binds []			`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_binds = [Rec core_prs]
    in
    addAutoScc auto_scc (global, mkLams tyvars $ mkLams dicts $ 
			         mkLets core_binds (Var local)) `thenDs` \ global' ->
    returnDs (global' : rest)

dsMonoBinds auto_scc (AbsBinds all_tyvars dicts exports binds) rest
  = dsMonoBinds False binds []			`thenDs` \ core_prs ->
    let 
	core_binds = [Rec core_prs]

	tup_expr      = mkTupleExpr locals
	tup_ty	      = coreExprType tup_expr
	poly_tup_expr = mkLams all_tyvars $ mkLams dicts $
		        mkLets core_binds tup_expr
	locals        = [local | (_, _, local) <- exports]
	local_tys     = map idType locals
    in
    newSysLocalDs (coreExprType poly_tup_expr)		`thenDs` \ poly_tup_id ->
    let
	dict_args = map Var dicts

	mk_bind (tyvars, global, local) n	-- locals !! n == local
	  = 	-- Need to make fresh locals to bind in the selector, because
		-- some of the tyvars will be bound to voidTy
	    newSysLocalsDs (map (substTy env) local_tys) 	`thenDs` \ locals' ->
	    newSysLocalDs  (substTy env tup_ty)			`thenDs` \ tup_id ->
	    addAutoScc auto_scc
		       (global, mkLams tyvars $ mkLams dicts $
		     	        mkTupleSelector locals' (locals' !! n) tup_id $
		     	        mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args = map mk_ty_arg all_tyvars
	    env     = all_tyvars `zipVarEnv` ty_args
    in
    zipWithDs mk_bind exports [0..]		`thenDs` \ export_binds ->
     -- don't scc (auto-)annotate the tuple itself.
    returnDs ((poly_tup_id, poly_tup_expr) : (export_binds ++ rest))
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
       returnDs (bndr, Note (SCC (mkAutoCC bndr mod grp IsNotCafCC)) core_expr)
 | otherwise 
     = returnDs pair

worthSCC (Note (SCC _) _) = False
worthSCC (Con _ _)        = False
worthSCC core_expr        = True
\end{code}

If profiling and dealing with a dict binding, wrap the dict in "_scc_ DICT <dict>":

\begin{code}
addDictScc var rhs
  | not ( opt_SccProfilingOn || opt_AutoSccsOnAllToplevs)
	    -- the latter is so that -unprof-auto-scc-all adds dict sccs
    || not (isDictTy (idType var))
  = returnDs rhs				-- That's easy: do nothing

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->

	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (Note (SCC (mkAllDictsCC mod grp False)) rhs)
\end{code}
