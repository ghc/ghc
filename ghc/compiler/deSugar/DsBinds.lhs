%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsHsBinds, dsHsNestedBinds, AutoScc(..) ) where

#include "HsVersions.h"


import {-# SOURCE #-}	DsExpr( dsLExpr )
import {-# SOURCE #-}	Match( matchWrapper )

import DsMonad
import DsGRHSs		( dsGuarded )
import DsUtils

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils	( exprType, mkInlineMe, mkSCC )

import StaticFlags	( opt_AutoSccsOnAllToplevs,
			  opt_AutoSccsOnExportedToplevs )
import CostCentre	( mkAutoCC, IsCafCC(..) )
import Id		( idType, idName, isExportedId, isSpecPragmaId, Id )
import NameSet
import VarSet
import Type		( mkTyVarTy, substTyWith )
import TysWiredIn	( voidTy )
import Outputable
import SrcLoc		( Located(..) )
import Maybe		( isJust )
import Bag		( bagToList )
import Monad		( foldM )
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsHsNestedBinds :: LHsBinds Id -> DsM [(Id,CoreExpr)]
dsHsNestedBinds binds = dsHsBinds NoSccs binds []

dsHsBinds :: AutoScc		 -- scc annotation policy (see below)
	  -> LHsBinds Id
	  -> [(Id,CoreExpr)]	 -- Put this on the end (avoid quadratic append)
	  -> DsM [(Id,CoreExpr)] -- Result

dsHsBinds auto_scc binds rest
  =  foldM (dsLHsBind auto_scc) rest (bagToList binds)

dsLHsBind :: AutoScc
	 -> [(Id,CoreExpr)]	-- Put this on the end (avoid quadratic append)
	 -> LHsBind Id
	 -> DsM [(Id,CoreExpr)] -- Result
dsLHsBind auto_scc rest (L loc bind)
  = putSrcSpanDs loc $ dsHsBind auto_scc rest bind

dsHsBind :: AutoScc
	 -> [(Id,CoreExpr)]	-- Put this on the end (avoid quadratic append)
	 -> HsBind Id
	 -> DsM [(Id,CoreExpr)] -- Result

dsHsBind auto_scc rest (VarBind var expr)
  = dsLExpr expr		`thenDs` \ core_expr ->

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

dsHsBind auto_scc rest (FunBind (L _ fun) _ matches)
  = matchWrapper (FunRhs (idName fun)) matches		`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkLams args body)		`thenDs` \ pair ->
    returnDs (pair : rest)

dsHsBind auto_scc rest (PatBind pat grhss ty)
  = dsGuarded grhss ty				`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr		`thenDs` \ sel_binds ->
    mappM (addAutoScc auto_scc) sel_binds	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common special case: no type or dictionary abstraction
	-- For the (rare) case when there are some mixed-up
	-- dictionary bindings (for which a Rec is convenient)
	-- we reply on the enclosing dsBind to wrap a Rec around.
dsHsBind auto_scc rest (AbsBinds [] [] exports inlines binds)
  = dsHsBinds (addSccs auto_scc exports) binds []`thenDs` \ core_prs ->
    let
	core_prs' = addLocalInlines exports inlines core_prs
	exports'  = [(global, Var local) | (_, global, local) <- exports]
    in
    returnDs (core_prs' ++ exports' ++ rest)

	-- Another common case: one exported variable
	-- Non-recursive bindings come through this way
dsHsBind auto_scc rest
     (AbsBinds all_tyvars dicts exps@[(tyvars, global, local)] inlines binds)
  = ASSERT( all (`elem` tyvars) all_tyvars )
    dsHsBinds (addSccs auto_scc exps) binds []	`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_bind = Rec core_prs

	-- The mkInline does directly what the 
	-- addLocalInlines do in the other cases
	export'    = (global, mkInline (idName global `elemNameSet` inlines) $
			      mkLams tyvars $ mkLams dicts $ 
	                      Let core_bind (Var local))
    in
    returnDs (export' : rest)

dsHsBind auto_scc rest (AbsBinds all_tyvars dicts exports inlines binds)
  = dsHsBinds (addSccs auto_scc exports) binds []`thenDs` \ core_prs ->
    let 
	-- Rec because of mixed-up dictionary bindings
	core_bind = Rec (addLocalInlines exports inlines core_prs)

	tup_expr      = mkTupleExpr locals
	tup_ty	      = exprType tup_expr
	poly_tup_expr = mkLams all_tyvars $ mkLams dicts $
		        Let core_bind tup_expr
	locals        = [local | (_, _, local) <- exports]
	local_tys     = map idType locals
    in
    newSysLocalDs (exprType poly_tup_expr)		`thenDs` \ poly_tup_id ->
    let
	dict_args = map Var dicts

	mk_bind ((tyvars, global, local), n)	-- locals !! n == local
	  = 	-- Need to make fresh locals to bind in the selector, because
		-- some of the tyvars will be bound to voidTy
	    newSysLocalsDs (map substitute local_tys) 	`thenDs` \ locals' ->
	    newSysLocalDs  (substitute tup_ty)		`thenDs` \ tup_id ->
	    returnDs (global, mkLams tyvars $ mkLams dicts $
		              mkTupleSelector locals' (locals' !! n) tup_id $
		              mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args    = map mk_ty_arg all_tyvars
	    substitute = substTyWith all_tyvars ty_args
    in
    mappM mk_bind (exports `zip` [0..])		`thenDs` \ export_binds ->
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
mkInline True  body = mkInlineMe body
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
					    (isExportedId exp && 
					     opt_AutoSccsOnExportedToplevs)
					-> Just exp
				_ -> Nothing)

addAutoScc :: AutoScc		-- if needs be, decorate toplevs?
	   -> (Id, CoreExpr)
	   -> DsM (Id, CoreExpr)

addAutoScc (TopLevelAddSccs auto_scc_fn) pair@(bndr, core_expr) 
 | do_auto_scc
     = getModuleDs `thenDs` \ mod ->
       returnDs (bndr, mkSCC (mkAutoCC top_bndr mod NotCafCC) core_expr)
 where do_auto_scc = isJust maybe_auto_scc
       maybe_auto_scc = auto_scc_fn bndr
       (Just top_bndr) = maybe_auto_scc

addAutoScc _ pair
     = returnDs pair
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
