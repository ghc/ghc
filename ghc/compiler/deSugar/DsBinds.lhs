%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsMonoBinds, AutoScc(..) ) where

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

import BasicTypes       ( RecFlag(..) )
import CmdLineOpts	( opt_SccProfilingOn, opt_AutoSccsOnAllToplevs, 
			  opt_AutoSccsOnExportedToplevs, opt_AutoSccsOnDicts
		        )
import CostCentre	( CostCentre, mkAutoCC, IsCafCC(..) )
import Id		( idType, idName, isUserExportedId, isSpecPragmaId, Id )
import NameSet
import VarEnv
import VarSet
import Type		( mkTyVarTy, isDictTy )
import Subst		( mkTyVarSubst, substTy )
import TysWiredIn	( voidTy )
import Outputable

import Maybe
import IOExts (trace)
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsMonoBinds :: AutoScc			-- scc annotation policy (see below)
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

    let
	-- Gross hack to prevent inlining into SpecPragmaId rhss
	-- Consider	fromIntegral = fromInteger . toInteger
	--		spec1 = fromIntegral Int Float
	-- Even though fromIntegral is small we don't want to inline
	-- it inside spec1, so that we collect the specialised call
	-- Solution: make spec1 an INLINE thing.  
   	core_expr'' = mkInline (isSpecPragmaId var) core_expr'
    in  

    returnDs ((var, core_expr'') : rest)

dsMonoBinds auto_scc (FunMonoBind fun _ matches locn) rest
  = putSrcLocDs locn	$
    matchWrapper (FunMatch fun) matches error_string	`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkLams args body)		`thenDs` \ pair ->
    returnDs (pair : rest)
  where
    error_string = "function " ++ showSDoc (ppr fun)

dsMonoBinds auto_scc (PatMonoBind pat grhss locn) rest
  = putSrcLocDs locn $
    dsGuarded grhss				`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr		`thenDs` \ sel_binds ->
    mapDs (addAutoScc auto_scc) sel_binds	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common case: one exported variable
	-- All non-recursive bindings come through this way
dsMonoBinds auto_scc
     (AbsBinds all_tyvars dicts exps@[(tyvars, global, local)] inlines binds) rest
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsMonoBinds (addSccs auto_scc exps) binds []	`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_binds = [Rec core_prs]
	global'    = (global, mkInline (idName global `elemNameSet` inlines) $
			      mkLams tyvars $ mkLams dicts $ 
	                      mkDsLets core_binds (Var local))
    in
    returnDs (global' : rest)

	-- Another common special case: no type or dictionary abstraction
dsMonoBinds auto_scc (AbsBinds [] [] exports inlines binds) rest
  = dsMonoBinds (addSccs auto_scc exports) binds []`thenDs` \ core_prs ->
    let 
	exports' = [(global, Var local) | (_, global, local) <- exports]
    in
    returnDs (addLocalInlines exports inlines core_prs ++ exports' ++ rest)

dsMonoBinds auto_scc (AbsBinds all_tyvars dicts exports inlines binds) rest
  = dsMonoBinds (addSccs auto_scc exports) binds []`thenDs` \ core_prs ->
    let 
	core_binds = [Rec (addLocalInlines exports inlines core_prs)]

	tup_expr      = mkTupleExpr locals
	tup_ty	      = coreExprType tup_expr
	poly_tup_expr = mkLams all_tyvars $ mkLams dicts $
		        mkDsLets core_binds tup_expr
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
	    returnDs (global, mkLams tyvars $ mkLams dicts $
		              mkTupleSelector locals' (locals' !! n) tup_id $
		              mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args = map mk_ty_arg all_tyvars
	    env     = mkTyVarSubst all_tyvars ty_args
    in
    zipWithDs mk_bind exports [0..]		`thenDs` \ export_binds ->
     -- don't scc (auto-)annotate the tuple itself.
    returnDs ((poly_tup_id, poly_tup_expr) : (export_binds ++ rest))
\end{code}


%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
mkInline :: Bool -> CoreExpr -> CoreExpr
mkInline True  body = Note InlineMe body
mkInline False body = body

addLocalInlines :: [(a, Id, Id)] -> NameSet -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
addLocalInlines exports inlines pairs
  = [(bndr, mkInline (bndr `elemVarSet` local_inlines) rhs) | (bndr,rhs) <- pairs]
  where
    local_inlines = mkVarSet [l | (_,g,l) <- exports, idName g `elemNameSet` inlines]
\end{code}


%************************************************************************
%*									*
\subsection[addAutoScc]{Adding automatic sccs}
%*									*
%************************************************************************

\begin{code}
data AutoScc
 	= TopLevel
	| TopLevelAddSccs (Id -> Maybe Id)
	| NoSccs

addSccs :: AutoScc -> [(a,Id,Id)] -> AutoScc
addSccs auto_scc@(TopLevelAddSccs _) exports = auto_scc
addSccs NoSccs   exports = NoSccs
addSccs TopLevel exports 
  = TopLevelAddSccs (\id -> case [ exp | (_,exp,loc) <- exports, loc == id ] of
				(exp:_)  | opt_AutoSccsOnAllToplevs || 
					    (isUserExportedId exp && 
					     opt_AutoSccsOnExportedToplevs)
					-> Just exp
				_ -> Nothing)

addAutoScc :: AutoScc		-- if needs be, decorate toplevs?
	   -> (Id, CoreExpr)
	   -> DsM (Id, CoreExpr)

addAutoScc (TopLevelAddSccs auto_scc_fn) pair@(bndr, core_expr) 
 | do_auto_scc && worthSCC core_expr
     = getModuleAndGroupDs `thenDs` \ (mod,grp) ->
       returnDs (bndr, Note (SCC (mkAutoCC top_bndr mod grp NotCafCC)) core_expr)
 where do_auto_scc = isJust maybe_auto_scc
       maybe_auto_scc = auto_scc_fn bndr
       (Just top_bndr) = maybe_auto_scc
addAutoScc _ pair
     = returnDs pair

worthSCC (Note (SCC _) _) = False
worthSCC (Con _ _)        = False
worthSCC core_expr        = True
\end{code}

If profiling and dealing with a dict binding,
wrap the dict in @_scc_ DICT <dict>@:

\begin{code}
addDictScc var rhs = returnDs rhs

{- DISABLED for now (need to somehow make up a name for the scc) -- SDM
  | not ( opt_SccProfilingOn && opt_AutoSccsOnDicts)
    || not (isDictTy (idType var))
  = returnDs rhs				-- That's easy: do nothing

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->
	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (Note (SCC (mkAllDictsCC mod grp False)) rhs)
-}
\end{code}
