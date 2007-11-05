%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Pattern-matching bindings (HsBinds and MonoBinds)

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module DsBinds ( dsTopLHsBinds, dsLHsBinds, decomposeRuleLhs, 
		 dsCoercion,
		 AutoScc(..)
  ) where

#include "HsVersions.h"

import {-# SOURCE #-}	DsExpr( dsLExpr, dsExpr )
import {-# SOURCE #-}	Match( matchWrapper )

import DsMonad
import DsGRHSs
import DsUtils

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreUtils

import TcHsSyn		( mkArbitraryType )	-- Mis-placed?
import TcType
import OccurAnal
import CostCentre
import Module
import Id
import Var	( TyVar )
import Rules
import VarEnv
import Type
import Outputable
import SrcLoc
import Maybes
import Bag
import BasicTypes hiding ( TopLevel )
import FastString
import Util		( mapSnd )

import Control.Monad
import Data.List
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

dsHsBind auto_scc rest (FunBind { fun_id = L _ fun, fun_matches = matches, 
				  fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf })
  = matchWrapper (FunRhs (idName fun) inf) matches	`thenDs` \ (args, body) ->
    mkOptTickBox tick body 				`thenDs` \ body' ->
    dsCoercion co_fn (return (mkLams args body'))	`thenDs` \ rhs ->
    returnDs ((fun,rhs) : rest)

dsHsBind auto_scc rest (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty })
  = dsGuarded grhss ty				`thenDs` \ body_expr ->
    mkSelectorBinds pat body_expr		`thenDs` \ sel_binds ->
    returnDs (sel_binds ++ rest)

-- Note [Rules and inlining]
-- Common special case: no type or dictionary abstraction
-- This is a bit less trivial than you might suppose
-- The naive way woudl be to desguar to something like
--	f_lcl = ...f_lcl...	-- The "binds" from AbsBinds
--	M.f = f_lcl		-- Generated from "exports"
-- But we don't want that, because if M.f isn't exported,
-- it'll be inlined unconditionally at every call site (its rhs is 
-- trivial).  That would be ok unless it has RULES, which would 
-- thereby be completely lost.  Bad, bad, bad.
--
-- Instead we want to generate
--	M.f = ...f_lcl...
--	f_lcl = M.f
-- Now all is cool. The RULES are attached to M.f (by SimplCore), 
-- and f_lcl is rapidly inlined away.
--
-- This does not happen in the same way to polymorphic binds,
-- because they desugar to
--	M.f = /\a. let f_lcl = ...f_lcl... in f_lcl
-- Although I'm a bit worried about whether full laziness might
-- float the f_lcl binding out and then inline M.f at its call site

dsHsBind auto_scc rest (AbsBinds [] [] exports binds)
  = do	{ core_prs <- ds_lhs_binds NoSccs binds
	; let env = mkABEnv exports
	      do_one (lcl_id, rhs) | Just (gbl_id, prags) <- lookupVarEnv env lcl_id
				   = addInlinePrags prags gbl_id $
				     addAutoScc auto_scc gbl_id rhs
				   | otherwise = (lcl_id, rhs)
	      locals'  = [(lcl_id, Var gbl_id) | (_, gbl_id, lcl_id, _) <- exports]
	; return (map do_one core_prs ++ locals' ++ rest) }
		-- No Rec needed here (contrast the other AbsBinds cases)
		-- because we can rely on the enclosing dsBind to wrap in Rec

	-- Another common case: one exported variable
	-- Non-recursive bindings come through this way
dsHsBind auto_scc rest
     (AbsBinds all_tyvars dicts exports@[(tyvars, global, local, prags)] binds)
  = ASSERT( all (`elem` tyvars) all_tyvars )
    ds_lhs_binds NoSccs binds 	`thenDs` \ core_prs ->
    let 
	-- Always treat the binds as recursive, because the typechecker
	-- makes rather mixed-up dictionary bindings
	core_bind = Rec core_prs
    in
    mappM (dsSpec all_tyvars dicts tyvars global local core_bind) 
	  prags				`thenDs` \ mb_specs ->
    let
	(spec_binds, rules) = unzip (catMaybes mb_specs)
	global' = addIdSpecialisations global rules
	rhs'    = mkLams tyvars $ mkLams dicts $ Let core_bind (Var local)
	bind    = addInlinePrags prags global' $ addAutoScc auto_scc global' rhs'
    in
    returnDs (bind  : spec_binds ++ rest)

dsHsBind auto_scc rest (AbsBinds all_tyvars dicts exports binds)
  = do	{ core_prs <- ds_lhs_binds NoSccs binds
	; let env = mkABEnv exports
	      do_one (lcl_id,rhs) | Just (gbl_id, prags) <- lookupVarEnv env lcl_id
			          = addInlinePrags prags lcl_id $
				    addAutoScc auto_scc gbl_id rhs
				  | otherwise = (lcl_id,rhs)
	       
		-- Rec because of mixed-up dictionary bindings
	      core_bind = Rec (map do_one core_prs)

	      tup_expr      = mkTupleExpr locals
	      tup_ty	    = exprType tup_expr
	      poly_tup_expr = mkLams all_tyvars $ mkLams dicts $
	 	     	      Let core_bind tup_expr
	      locals        = [local | (_, _, local, _) <- exports]
	      local_tys     = map idType locals

	; poly_tup_id <- newSysLocalDs (exprType poly_tup_expr)

	; let dict_args = map Var dicts

	      mk_bind ((tyvars, global, local, prags), n)	-- locals !! n == local
	        = 	-- Need to make fresh locals to bind in the selector, because
		      	-- some of the tyvars will be bound to 'Any'
		  do { locals' <- newSysLocalsDs (map substitute local_tys)
		     ; tup_id  <- newSysLocalDs  (substitute tup_ty)
		     ; mb_specs <- mapM (dsSpec all_tyvars dicts tyvars global local core_bind) 
				      	 prags
		     ; let (spec_binds, rules) = unzip (catMaybes mb_specs)
			   global' = addIdSpecialisations global rules
	                   rhs = mkLams tyvars $ mkLams dicts $
	      	     		 mkTupleSelector locals' (locals' !! n) tup_id $
			         mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args
		     ; returnDs ((global', rhs) : spec_binds) }
	        where
	          mk_ty_arg all_tyvar | all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
	      			      | otherwise		= mkArbitraryType all_tyvar
	          ty_args    = map mk_ty_arg all_tyvars
	          substitute = substTyWith all_tyvars ty_args

	; export_binds_s <- mappM mk_bind (exports `zip` [0..])
	     -- don't scc (auto-)annotate the tuple itself.

	; returnDs ((poly_tup_id, poly_tup_expr) : 
		    (concat export_binds_s ++ rest)) }

mkABEnv :: [([TyVar], Id, Id, [LPrag])] -> VarEnv (Id, [LPrag])
-- Takes the exports of a AbsBinds, and returns a mapping
--	lcl_id -> (gbl_id, prags)
mkABEnv exports = mkVarEnv [ (lcl_id, (gbl_id, prags)) 
			   | (_, gbl_id, lcl_id, prags) <- exports]


dsSpec :: [TyVar] -> [DictId] -> [TyVar]
       -> Id -> Id		-- Global, local
       -> CoreBind -> LPrag
       -> DsM (Maybe ((Id,CoreExpr), 	-- Binding for specialised Id
		      CoreRule))	-- Rule for the Global Id

-- Example:
--	f :: (Eq a, Ix b) => a -> b -> b
--	{-# SPECIALISE f :: Ix b => Int -> b -> b #-}
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
--		The idea is that f occurs just once, so it'll be 
--		inlined and specialised
--
-- Given SpecPrag (/\as.\ds. f es) t, we have
-- the defn		f_spec as ds = f es 
-- and the RULE		f es = f_spec as ds
--
-- It is *possible* that 'es' does not mention all of the dictionaries 'ds'
-- (a bit silly, because then the 
dsSpec all_tvs dicts tvs poly_id mono_id mono_bind (L _ (InlinePrag {}))
  = return Nothing

dsSpec all_tvs dicts tvs poly_id mono_id mono_bind
       (L loc (SpecPrag spec_expr spec_ty _const_dicts inl))
	-- See Note [Const rule dicts]
  = putSrcSpanDs loc $ 
    do	{ let poly_name = idName poly_id
	; spec_name <- newLocalName poly_name
	; ds_spec_expr  <- dsExpr spec_expr
	; let (bndrs, body) = collectBinders (occurAnalyseExpr ds_spec_expr)
		-- The occurrence-analysis does two things
		-- (a) identifies unused binders: Note [Unused spec binders]
		-- (b) sorts dict bindings into NonRecs 
		--	so they can be inlined by decomposeRuleLhs
	      mb_lhs = decomposeRuleLhs body

	-- Check for dead binders: Note [Unused spec binders]
	; case filter isDeadBinder bndrs of {
		bs | not (null bs) -> do { warnDs (dead_msg bs); return Nothing }
		   | otherwise -> 

	  case mb_lhs of
	    Nothing -> do { warnDs decomp_msg; return Nothing }

	    Just (var, args) -> return (Just (addInlineInfo inl spec_id spec_rhs, rule))
		where
		  local_poly  = setIdNotExported poly_id
			-- Very important to make the 'f' non-exported,
			-- else it won't be inlined!
		  spec_id     = mkLocalId spec_name spec_ty
		  spec_rhs    = Let (NonRec local_poly poly_f_body) ds_spec_expr
		  poly_f_body = mkLams (tvs ++ dicts) $
			   	fix_up (Let mono_bind (Var mono_id))

		  rule =  mkLocalRule (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
				AlwaysActive poly_name
			        bndrs args
				(mkVarApps (Var spec_id) bndrs)
	} }
  where
	-- Bind to Any any of all_ptvs that aren't 
	-- relevant for this particular function 
    fix_up body | null void_tvs = body
		| otherwise	= mkTyApps (mkLams void_tvs body) 
					   (map mkArbitraryType void_tvs)
    void_tvs = all_tvs \\ tvs

    dead_msg bs = vcat [ sep [ptext SLIT("Useless constraint") <> plural bs
				 <+> ptext SLIT("in specialied type:"),
			     nest 2 (pprTheta (map get_pred bs))]
		       , ptext SLIT("SPECIALISE pragma ignored")]
    get_pred b = ASSERT( isId b ) expectJust "dsSpec" (tcSplitPredTy_maybe (idType b))

    decomp_msg = hang (ptext SLIT("Specialisation too complicated to desugar; ignored"))
		    2 (ppr spec_expr)
\end{code}

Note [Unused spec binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	f :: a -> a
	{-# SPECIALISE f :: Eq a => a -> a #-}
It's true that this *is* a more specialised type, but the rule
we get is something like this:
	f_spec d = f
	RULE: f = f_spec d
Note that the rule is bogus, becuase it mentions a 'd' that is
not bound on the LHS!  But it's a silly specialisation anyway, becuase
the constraint is unused.  We could bind 'd' to (error "unused")
but it seems better to reject the program because it's almost certainly
a mistake.  That's what the isDeadBinder call detects.

Note [Const rule dicts]
~~~~~~~~~~~~~~~~~~~~~~~
A SpecPrag has a field for "constant dicts" in the RULE, but I think
it's pretty useless.  See the place where it's generated in TcBinds.
TcSimplify will discharge a constraint by binding it to, say, 
GHC.Base.$f2 :: Eq Int, withour putting anything in the LIE, so this 
dict won't show up in the const-dicts field.  It probably doesn't matter,
because the rule will end up being something like
	f Int GHC.Base.$f2 = ...
rather than
	forall d. f Int d = ...
The latter is more general, but in practice I think it won't make any
difference.


%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
decomposeRuleLhs :: CoreExpr -> Maybe (Id, [CoreExpr])
-- Returns Nothing if the LHS isn't of the expected shape
decomposeRuleLhs lhs 
  = go emptyVarEnv (occurAnalyseExpr lhs)	-- Occurrence analysis sorts out the dict
						-- bindings so we know if they are recursive
  where
	-- Substitute dicts in the LHS args, so that there 
	-- aren't any lets getting in the way
	-- Note that we substitute the function too; we might have this as
	-- a LHS:	let f71 = M.f Int in f71
    go env (Let (NonRec dict rhs) body) 
	= go (extendVarEnv env dict (simpleSubst env rhs)) body
    go env body 
	= case collectArgs (simpleSubst env body) of
	    (Var fn, args) -> Just (fn, args)
	    other 	   -> Nothing

simpleSubst :: IdEnv CoreExpr -> CoreExpr -> CoreExpr
-- Similar to CoreSubst.substExpr, except that 
-- (a) takes no account of capture; dictionary bindings use new names
-- (b) can have a GlobalId (imported) in its domain
-- (c) Ids only; no types are substituted
--
-- (b) is the reason we can't use CoreSubst... and it's no longer relevant
-- 	so really we should replace simpleSubst 
simpleSubst subst expr
  = go expr
  where
    go (Var v)	       = lookupVarEnv subst v `orElse` Var v
    go (Cast e co)     = Cast (go e) co
    go (Type ty)       = Type ty
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Note note e)   = Note note (go e)
    go (Lam bndr body) = Lam bndr (go body)
    go (Let (NonRec bndr rhs) body) = Let (NonRec bndr (go rhs)) (go body)
    go (Let (Rec pairs) body)       = Let (Rec (mapSnd go pairs)) (go body)
    go (Case scrut bndr ty alts)    = Case (go scrut) bndr ty 
					   [(c,bs,go r) | (c,bs,r) <- alts]

addInlinePrags :: [LPrag] -> Id -> CoreExpr -> (Id,CoreExpr)
addInlinePrags prags bndr rhs
  = case [inl | L _ (InlinePrag inl) <- prags] of
	[]      -> (bndr, rhs)
	(inl:_) -> addInlineInfo inl bndr rhs

addInlineInfo :: InlineSpec -> Id -> CoreExpr -> (Id,CoreExpr)
addInlineInfo (Inline phase is_inline) bndr rhs
  = (attach_phase bndr phase, wrap_inline is_inline rhs)
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
data AutoScc = NoSccs 
	     | AddSccs Module (Id -> Bool)
-- The (Id->Bool) says which Ids to add SCCs to 

addAutoScc :: AutoScc	
	   -> Id	-- Binder
	   -> CoreExpr 	-- Rhs
	   -> CoreExpr	-- Scc'd Rhs

addAutoScc NoSccs _ rhs
  = rhs
addAutoScc (AddSccs mod add_scc) id rhs
  | add_scc id = mkSCC (mkAutoCC id mod NotCafCC) rhs
  | otherwise  = rhs
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


%************************************************************************
%*									*
		Desugaring coercions
%*									*
%************************************************************************


\begin{code}
dsCoercion :: HsWrapper -> DsM CoreExpr -> DsM CoreExpr
dsCoercion WpHole 	     thing_inside = thing_inside
dsCoercion (WpCompose c1 c2) thing_inside = dsCoercion c1 (dsCoercion c2 thing_inside)
dsCoercion (WpCo co)     thing_inside = do { expr <- thing_inside
					       ; return (Cast expr co) }
dsCoercion (WpLam id)        thing_inside = do { expr <- thing_inside
					       ; return (Lam id expr) }
dsCoercion (WpTyLam tv)      thing_inside = do { expr <- thing_inside
					       ; return (Lam tv expr) }
dsCoercion (WpApp id)        thing_inside = do { expr <- thing_inside
					       ; return (App expr (Var id)) }
dsCoercion (WpTyApp ty)      thing_inside = do { expr <- thing_inside
					       ; return (App expr (Type ty)) }
dsCoercion WpInline 	     thing_inside = do { expr <- thing_inside
					       ; return (mkInlineMe expr) }
dsCoercion (WpLet bs)        thing_inside = do { prs <- dsLHsBinds bs
					       ; expr <- thing_inside
					       ; return (Let (Rec prs) expr) }
\end{code}
