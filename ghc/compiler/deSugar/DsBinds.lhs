%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsBinds]{Pattern-matching bindings (HsBinds and MonoBinds)}

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsTopLHsBinds, dsLHsBinds, decomposeRuleLhs, AutoScc(..) ) where

#include "HsVersions.h"


import {-# SOURCE #-}	DsExpr( dsLExpr, dsExpr )
import {-# SOURCE #-}	Match( matchWrapper )

import DsMonad
import DsGRHSs		( dsGuarded )
import DsUtils

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils	( exprType, mkInlineMe, mkSCC )

import StaticFlags	( opt_AutoSccsOnAllToplevs,
			  opt_AutoSccsOnExportedToplevs )
import OccurAnal	( occurAnalyseExpr )
import CostCentre	( mkAutoCC, IsCafCC(..) )
import Id		( Id, idType, idName, isExportedId, mkLocalId, setInlinePragma )
import Rules		( addIdSpecialisations, mkLocalRule )
import Var		( Var, isGlobalId )
import VarEnv
import Type		( mkTyVarTy, substTyWith )
import TysWiredIn	( voidTy )
import Outputable
import SrcLoc		( Located(..) )
import Maybes		( isJust, catMaybes, orElse )
import Bag		( bagToList )
import BasicTypes	( Activation(..), isAlwaysActive )
import Monad		( foldM )
import FastString	( mkFastString )
import List		( (\\) )
import Util		( mapSnd )
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsTopLHsBinds :: AutoScc -> LHsBinds Id -> DsM [(Id,CoreExpr)]
dsTopLHsBinds auto_scc binds = ds_lhs_binds auto_scc binds

dsLHsBinds :: LHsBinds Id -> DsM [(Id,CoreExpr)]
dsLHsBinds binds = ds_lhs_binds NoSccs binds


------------------------
ds_lhs_binds :: AutoScc -> LHsBinds Id -> DsM [(Id,CoreExpr)]
	 -- scc annotation policy (see below)
ds_lhs_binds auto_scc binds =  foldM (dsLHsBind auto_scc) [] (bagToList binds)

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
    returnDs ((var, core_expr') : rest)

dsHsBind auto_scc rest (FunBind (L _ fun) _ matches _)
  = matchWrapper (FunRhs (idName fun)) matches		`thenDs` \ (args, body) ->
    addAutoScc auto_scc (fun, mkLams args body)		`thenDs` \ pair ->
    returnDs (pair : rest)

dsHsBind auto_scc rest (PatBind pat grhss ty _)
  = dsGuarded grhss ty				`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr		`thenDs` \ sel_binds ->
    mappM (addAutoScc auto_scc) sel_binds	`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

	-- Common special case: no type or dictionary abstraction
	-- For the (rare) case when there are some mixed-up
	-- dictionary bindings (for which a Rec is convenient)
	-- we reply on the enclosing dsBind to wrap a Rec around.
dsHsBind auto_scc rest (AbsBinds [] [] exports binds)
  = ds_lhs_binds (addSccs auto_scc exports) binds 	`thenDs` \ core_prs ->
    let
	core_prs' = addLocalInlines exports core_prs
	exports'  = [(global, Var local) | (_, global, local, _) <- exports]
    in
    returnDs (core_prs' ++ exports' ++ rest)

	-- Another common case: one exported variable
	-- Non-recursive bindings come through this way
dsHsBind auto_scc rest
     (AbsBinds all_tyvars dicts exports@[(tyvars, global, local, prags)] binds)
  = ASSERT( all (`elem` tyvars) all_tyvars )
    ds_lhs_binds (addSccs auto_scc exports) binds 	`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_bind = Rec core_prs
	inline_env = mkVarEnv [(global, prag) | prag <- prags, isInlinePrag prag]
    in
    mappM (dsSpec all_tyvars dicts tyvars global local core_bind) 
	  prags				`thenDs` \ mb_specs ->
    let
	(spec_binds, rules) = unzip (catMaybes mb_specs)
	global' = addIdSpecialisations global rules
	rhs'    = mkLams tyvars $ mkLams dicts $ Let core_bind (Var local)
    in
    returnDs (addInlineInfo inline_env (global', rhs') : spec_binds ++ rest)

dsHsBind auto_scc rest (AbsBinds all_tyvars dicts exports binds)
  = ds_lhs_binds (addSccs auto_scc exports) binds 	`thenDs` \ core_prs ->
     let 
	-- Rec because of mixed-up dictionary bindings
	core_bind = Rec (addLocalInlines exports core_prs)

	tup_expr      = mkTupleExpr locals
	tup_ty	      = exprType tup_expr
	poly_tup_expr = mkLams all_tyvars $ mkLams dicts $
		        Let core_bind tup_expr
	locals        = [local | (_, _, local, _) <- exports]
	local_tys     = map idType locals
    in
    newSysLocalDs (exprType poly_tup_expr)		`thenDs` \ poly_tup_id ->
    let
	dict_args = map Var dicts

	mk_bind ((tyvars, global, local, prags), n)	-- locals !! n == local
	  = 	-- Need to make fresh locals to bind in the selector, because
		-- some of the tyvars will be bound to voidTy
	    newSysLocalsDs (map substitute local_tys) 	`thenDs` \ locals' ->
	    newSysLocalDs  (substitute tup_ty)		`thenDs` \ tup_id ->
	    mapM (dsSpec all_tyvars dicts tyvars global local core_bind) 
		 prags				`thenDs` \ mb_specs ->
	    let
		(spec_binds, rules) = unzip (catMaybes mb_specs)
		global' = addIdSpecialisations global rules
	        rhs = mkLams tyvars $ mkLams dicts $
		      mkTupleSelector locals' (locals' !! n) tup_id $
		      mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args
	    in
	    returnDs ((global', rhs) : spec_binds)
	  where
	    mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
				| otherwise		  = voidTy
	    ty_args    = map mk_ty_arg all_tyvars
	    substitute = substTyWith all_tyvars ty_args
    in
    mappM mk_bind (exports `zip` [0..])		`thenDs` \ export_binds_s ->
     -- don't scc (auto-)annotate the tuple itself.

    returnDs ((poly_tup_id, poly_tup_expr) : (concat export_binds_s ++ rest))

-- Example:
--	f :: (Eq a, Ix b) => a -> b -> b
--
--	AbsBinds [ab] [d1,d2] [([ab], f, f_mono, prags)] binds
-- 
-- 	SpecPrag (/\b.\(d:Ix b). f Int b dInt d) 
--		 (forall b. Ix b => Int -> b -> b)
--
-- Rule: 	forall b,(d:Ix b). f Int b dInt d = f_spec b d
--
-- Spec bind:	f_spec = Let f = /\ab \(d1:Eq a)(d2:Ix b). let binds in f_mono 
--			 /\b.\(d:Ix b). in f Int b dInt d

dsSpec all_tvs dicts tvs poly_id mono_id mono_bind (InlinePrag {})
  = return Nothing

dsSpec all_tvs dicts tvs poly_id mono_id mono_bind
       (SpecPrag spec_expr spec_ty const_dicts)
  = do	{ let poly_name = idName poly_id
	; spec_name <- newLocalName (idName poly_id)
	; ds_spec_expr  <- dsExpr spec_expr
	; let (bndrs, body) = collectBinders ds_spec_expr
	      mb_lhs  	    = decomposeRuleLhs (bndrs ++ const_dicts) body

	; case mb_lhs of
	    Nothing -> do { dsWarn msg; return Nothing }

	    Just (bndrs', var, args) -> return (Just ((spec_id, spec_rhs), rule))
		where
		  spec_id     = mkLocalId spec_name spec_ty
		  spec_rhs    = Let (NonRec poly_id poly_f_body) ds_spec_expr
		  poly_f_body = mkLams (tvs ++ dicts) $
			   	fix_up (Let mono_bind (Var mono_id))

			-- Quantify over constant dicts on the LHS, since
			-- their value depends only on their type
			-- The ones we are interested in may even be imported
			-- e.g. GHC.Base.dEqInt

		  rule =  mkLocalRule (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
				AlwaysActive poly_name
			        bndrs'	-- Includes constant dicts
				args
				(mkVarApps (Var spec_id) bndrs)
	}
  where
	-- Bind to voidTy any of all_ptvs that aren't 
	-- relevant for this particular function 
    fix_up body | null void_tvs = body
		| otherwise	= mkTyApps (mkLams void_tvs body) 
					   (map (const voidTy) void_tvs)
    void_tvs = all_tvs \\ tvs

    msg = hang (ptext SLIT("Specialisation too complicated to desugar; ignored"))
	     2 (ppr spec_expr)
\end{code}


%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
decomposeRuleLhs :: [Var] -> CoreExpr -> Maybe ([Var], Id, [CoreExpr])
-- Returns Nothing if the LHS isn't of the expected shape
-- The argument 'all_bndrs' includes the "constant dicts" of the LHS,
-- and they may be GlobalIds, which we can't forall-ify. 
-- So we substitute them out instead
decomposeRuleLhs all_bndrs lhs 
  = go init_env (occurAnalyseExpr lhs)	-- Occurrence analysis sorts out the dict
					-- bindings so we know if they are recursive
  where

	-- all_bndrs may include top-level imported dicts, 
	-- imported things with a for-all.  
	-- So we localise them and subtitute them out
    bndr_prs =	[ (id, Var (localise id)) | id <- all_bndrs, isGlobalId id ]
    localise d = mkLocalId (idName d) (idType d)

    init_env = mkVarEnv bndr_prs
    all_bndrs' = map subst_bndr all_bndrs
    subst_bndr bndr = case lookupVarEnv init_env bndr of
			Just (Var bndr') -> bndr'
			Just other	 -> panic "decomposeRuleLhs"
			Nothing		 -> bndr

	-- Substitute dicts in the LHS args, so that there 
	-- aren't any lets getting in the way
    go env (Let (NonRec dict rhs) body) 
	= go (extendVarEnv env dict (simpleSubst env rhs)) body
    go env body 
	= case collectArgs body of
	    (Var fn, args) -> Just (all_bndrs', fn, map (simpleSubst env) args)
	    other 	   -> Nothing

simpleSubst :: IdEnv CoreExpr -> CoreExpr -> CoreExpr
-- Similar to CoreSubst.substExpr, except that 
-- (a) takes no account of capture; dictionary bindings use new names
-- (b) can have a GlobalId (imported) in its domain
-- (c) Ids only; no types are substituted

simpleSubst subst expr
  = go expr
  where
    go (Var v)	       = lookupVarEnv subst v `orElse` Var v
    go (Type ty)       = Type ty
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Note note e)   = Note note (go e)
    go (Lam bndr body) = Lam bndr (go body)
    go (Let (NonRec bndr rhs) body) = Let (NonRec bndr (go rhs)) (go body)
    go (Let (Rec pairs) body)       = Let (Rec (mapSnd go pairs)) (go body)
    go (Case scrut bndr ty alts)    = Case (go scrut) bndr ty 
					   [(c,bs,go r) | (c,bs,r) <- alts]

addLocalInlines exports core_prs
  = map (addInlineInfo inline_env) core_prs
  where
    inline_env = mkVarEnv [(mono_id, prag) 
			  | (_, _, mono_id, prags) <- exports,
			    prag <- prags, isInlinePrag prag]
					   
addInlineInfo :: IdEnv Prag -> (Id,CoreExpr) -> (Id,CoreExpr)
addInlineInfo inline_env (bndr,rhs)
  | Just (InlinePrag is_inline phase) <- lookupVarEnv inline_env bndr
  = (attach_phase bndr phase, wrap_inline is_inline rhs)
  | otherwise
  = (bndr, rhs)
  where
    attach_phase bndr phase 
	| isAlwaysActive phase = bndr	-- Default phase
	| otherwise  	       = bndr `setInlinePragma` phase

    wrap_inline True  body = mkInlineMe body
    wrap_inline False body = body
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

addSccs :: AutoScc -> [(a,Id,Id,[Prag])] -> AutoScc
addSccs auto_scc@(TopLevelAddSccs _) exports = auto_scc
addSccs NoSccs   exports = NoSccs
addSccs TopLevel exports 
  = TopLevelAddSccs (\id -> case [ exp | (_,exp,loc,_) <- exports, loc == id ] of
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
    || not (isDictId var)
  = returnDs rhs				-- That's easy: do nothing

  | otherwise
  = getModuleAndGroupDs 	`thenDs` \ (mod, grp) ->
	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
    returnDs (Note (SCC (mkAllDictsCC mod grp False)) rhs)
-}
\end{code}
