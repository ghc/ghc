%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Pattern-matching bindings (HsBinds and MonoBinds)

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
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
import OccurAnal

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import MkCore
import CoreUtils
import CoreFVs

import TcHsSyn	( mkArbitraryType )	-- Mis-placed?
import TcType
import CostCentre
import Module
import Id
import Name	( localiseName )
import Var	( Var, TyVar )
import VarSet
import Rules
import VarEnv
import Type
import Outputable
import SrcLoc
import Maybes
import Bag
import BasicTypes hiding ( TopLevel )
import FastString
import StaticFlags	( opt_DsMultiTyVar )
import Util		( mapSnd, mapAndUnzip, lengthExceeds )

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

dsHsBind _ rest (VarBind var expr) = do
    core_expr <- dsLExpr expr

        -- Dictionary bindings are always VarMonoBinds, so
        -- we only need do this here
    core_expr' <- addDictScc var core_expr
    return ((var, core_expr') : rest)

dsHsBind _ rest (FunBind { fun_id = L _ fun, fun_matches = matches, 
				  fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf }) = do
    (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
    body' <- mkOptTickBox tick body
    rhs <- dsCoercion co_fn (return (mkLams args body'))
    return ((fun,rhs) : rest)

dsHsBind _ rest (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }) = do
    body_expr <- dsGuarded grhss ty
    sel_binds <- mkSelectorBinds pat body_expr
    return (sel_binds ++ rest)

{-  Note [Rules and inlining]
    ~~~~~~~~~~~~~~~~~~~~~~~~~
    Common special case: no type or dictionary abstraction
    This is a bit less trivial than you might suppose
    The naive way woudl be to desguar to something like
    	f_lcl = ...f_lcl...	-- The "binds" from AbsBinds
    	M.f = f_lcl		-- Generated from "exports"
    But we don't want that, because if M.f isn't exported,
    it'll be inlined unconditionally at every call site (its rhs is 
    trivial).  That would be ok unless it has RULES, which would 
    thereby be completely lost.  Bad, bad, bad.

    Instead we want to generate
    	M.f = ...f_lcl...
    	f_lcl = M.f
    Now all is cool. The RULES are attached to M.f (by SimplCore), 
    and f_lcl is rapidly inlined away.

    This does not happen in the same way to polymorphic binds,
    because they desugar to
    	M.f = /\a. let f_lcl = ...f_lcl... in f_lcl
    Although I'm a bit worried about whether full laziness might
    float the f_lcl binding out and then inline M.f at its call site -}

dsHsBind auto_scc rest (AbsBinds [] [] exports binds)
  = do	{ core_prs <- ds_lhs_binds NoSccs binds
	; let env = mkABEnv exports
	      do_one (lcl_id, rhs) | Just (_, gbl_id, _, prags) <- lookupVarEnv env lcl_id
				   = addInlinePrags prags gbl_id $
				     addAutoScc auto_scc gbl_id rhs
				   | otherwise = (lcl_id, rhs)
	      locals'  = [(lcl_id, Var gbl_id) | (_, gbl_id, lcl_id, _) <- exports]
			-- Note [Rules and inlining]
	; return (map do_one core_prs ++ locals' ++ rest) }
		-- No Rec needed here (contrast the other AbsBinds cases)
		-- because we can rely on the enclosing dsBind to wrap in Rec


{- Note [Abstracting over tyvars only]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   When abstracting over type variable only (not dictionaries), we don't really need to
   built a tuple and select from it, as we do in the general case. Instead we can take

	AbsBinds [a,b] [ ([a,b], fg, fl, _),
 		         ([b],   gg, gl, _) ]
		{ fl = e1
		  gl = e2
		   h = e3 }

   and desugar it to

	fg = /\ab. let B in e1
	gg = /\b. let a = () in let B in S(e2)
	h  = /\ab. let B in e3

  where B is the *non-recursive* binding
	fl = fg a b
	gl = gg b
	h  = h a b 
  
  Notice (a) g has a different number of type variables to f, so we must
	     use the mkArbitraryType thing to fill in the gaps.  
	     We use a type-let to do that.

	 (b) The local variable h isn't in the exports, and rather than
	     clone a fresh copy we simply replace h by (h a b).  

	 (c) The result is *still* quadratic-sized if there are a lot of
	     small bindings.  So if there are more than some small
	     number (10), we filter the binding set B by the free
	     variables of the particular RHS.  Tiresome.

  Why got to this trouble?  It's a common case, and it removes the
  quadratic-sized tuple desugaring.  Less clutter, hopefullly faster
  compilation, especially in a case where there are a *lot* of
  bindings.
-}


dsHsBind auto_scc rest (AbsBinds tyvars [] exports binds)
  | opt_DsMultiTyVar	-- This (static) debug flag just lets us
			-- switch on and off this optimisation to
			-- see if it has any impact; it is on by default
  = 	-- Note [Abstracting over tyvars only]
    do	{ core_prs <- ds_lhs_binds NoSccs binds
	; arby_env <- mkArbitraryTypeEnv tyvars exports
	; let (lg_binds, core_prs') = mapAndUnzip do_one core_prs
	      bndrs = mkVarSet (map fst core_prs)

	      add_lets | core_prs `lengthExceeds` 10 = add_some
		       | otherwise	             = mkLets lg_binds
	      add_some rhs = mkLets [ NonRec b r | NonRec b r <- lg_binds
				    , b `elemVarSet` fvs] rhs
		where
		  fvs = exprSomeFreeVars (`elemVarSet` bndrs) rhs

	      env = mkABEnv exports

	      do_one (lcl_id, rhs) 
		| Just (id_tvs, gbl_id, _, prags) <- lookupVarEnv env lcl_id
		= (NonRec lcl_id (mkTyApps (Var gbl_id) (mkTyVarTys id_tvs)),
		   addInlinePrags prags gbl_id $
		   addAutoScc auto_scc gbl_id  $
		   mkLams id_tvs $
		   mkLets [ NonRec tv (Type (lookupVarEnv_NF arby_env tv))
		          | tv <- tyvars, not (tv `elem` id_tvs)] $
	           add_lets rhs)
		| otherwise
		= (NonRec lcl_id (mkTyApps (Var non_exp_gbl_id) (mkTyVarTys tyvars)),
		   (non_exp_gbl_id, mkLams tyvars (add_lets rhs)))
		where
		  non_exp_gbl_id = setIdType lcl_id (mkForAllTys tyvars (idType lcl_id))
						  
	; return (core_prs' ++ rest) }

	-- Another common case: one exported variable
	-- Non-recursive bindings come through this way
dsHsBind auto_scc rest
     (AbsBinds all_tyvars dicts [(tyvars, global, local, prags)] binds)
  = ASSERT( all (`elem` tyvars) all_tyvars ) do
    core_prs <- ds_lhs_binds NoSccs binds
    let
        -- Always treat the binds as recursive, because the typechecker
        -- makes rather mixed-up dictionary bindings
        core_bind = Rec core_prs
    
    mb_specs <- mapM (dsSpec all_tyvars dicts tyvars global local core_bind) prags
    let
        (spec_binds, rules) = unzip (catMaybes mb_specs)
        global' = addIdSpecialisations global rules
        rhs'    = mkLams tyvars $ mkLams dicts $ Let core_bind (Var local)
        bind    = addInlinePrags prags global' $ addAutoScc auto_scc global' rhs'
    
    return (bind  : spec_binds ++ rest)

dsHsBind auto_scc rest (AbsBinds all_tyvars dicts exports binds)
  = do	{ core_prs <- ds_lhs_binds NoSccs binds
	; let env = mkABEnv exports
	      do_one (lcl_id,rhs) | Just (_, gbl_id, _, prags) <- lookupVarEnv env lcl_id
			          = addInlinePrags prags lcl_id $
				    addAutoScc auto_scc gbl_id rhs
				  | otherwise = (lcl_id,rhs)
	       
		-- Rec because of mixed-up dictionary bindings
	      core_bind = Rec (map do_one core_prs)

	      tup_expr      = mkBigCoreVarTup locals
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
		  do { ty_args <- mapM mk_ty_arg all_tyvars
		     ; let substitute = substTyWith all_tyvars ty_args
		     ; locals' <- newSysLocalsDs (map substitute local_tys)
		     ; tup_id  <- newSysLocalDs  (substitute tup_ty)
		     ; mb_specs <- mapM (dsSpec all_tyvars dicts tyvars global local core_bind) 
				      	 prags
		     ; let (spec_binds, rules) = unzip (catMaybes mb_specs)
			   global' = addIdSpecialisations global rules
	                   rhs = mkLams tyvars $ mkLams dicts $
	      	     		 mkTupleSelector locals' (locals' !! n) tup_id $
			         mkApps (mkTyApps (Var poly_tup_id) ty_args) dict_args
		     ; return ((global', rhs) : spec_binds) }
	        where
	          mk_ty_arg all_tyvar
			| all_tyvar `elem` tyvars = return (mkTyVarTy all_tyvar)
	      		| otherwise		  = dsMkArbitraryType all_tyvar

	; export_binds_s <- mapM mk_bind (exports `zip` [0..])
	     -- don't scc (auto-)annotate the tuple itself.

	; return ((poly_tup_id, poly_tup_expr) : 
		    (concat export_binds_s ++ rest)) }

mkABEnv :: [([TyVar], Id, Id, [LPrag])] -> VarEnv ([TyVar], Id, Id, [LPrag])
-- Takes the exports of a AbsBinds, and returns a mapping
--	lcl_id -> (tyvars, gbl_id, lcl_id, prags)
mkABEnv exports = mkVarEnv [ (lcl_id, export) | export@(_, _, lcl_id, _) <- exports]


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
-- the defn		f_spec as ds = let-nonrec f = /\fas\fds. let f_mono = <f-rhs> in f_mono
--				       in f es 
-- and the RULE		forall as, ds. f es = f_spec as ds
--
-- It is *possible* that 'es' does not mention all of the dictionaries 'ds'
-- (a bit silly, because then the 
dsSpec _ _ _ _ _ _ (L _ (InlinePrag {}))
  = return Nothing

dsSpec all_tvs dicts tvs poly_id mono_id mono_bind
       (L loc (SpecPrag spec_expr spec_ty inl))
  = putSrcSpanDs loc $ 
    do	{ let poly_name = idName poly_id
	; spec_name <- newLocalName poly_name
	; ds_spec_expr  <- dsExpr spec_expr
	; case (decomposeRuleLhs ds_spec_expr) of {
	    Nothing -> do { warnDs decomp_msg; return Nothing } ;

	    Just (bndrs, _fn, args) ->

	-- Check for dead binders: Note [Unused spec binders]
	  case filter isDeadBinder bndrs of {
		bs | not (null bs) -> do { warnDs (dead_msg bs); return Nothing } 
		   | otherwise -> do

	{ f_body <- fix_up (Let mono_bind (Var mono_id))

	; let	  local_poly  = setIdNotExported poly_id
			-- Very important to make the 'f' non-exported,
			-- else it won't be inlined!
		  spec_id     = mkLocalId spec_name spec_ty
		  spec_rhs    = Let (NonRec local_poly poly_f_body) ds_spec_expr
		  poly_f_body = mkLams (tvs ++ dicts) f_body
			   	
		  extra_dict_bndrs = [localise d 
		  		     | d <- varSetElems (exprFreeVars ds_spec_expr)
		  		     , isDictId d]
			-- Note [Const rule dicts]

		  rule =  mkLocalRule (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
				AlwaysActive poly_name
			        (extra_dict_bndrs ++ bndrs) args
				(mkVarApps (Var spec_id) bndrs)
	; return (Just (addInlineInfo inl spec_id spec_rhs, rule))
	} } } }
  where
	-- Bind to Any any of all_ptvs that aren't 
	-- relevant for this particular function 
    fix_up body | null void_tvs = return body
		| otherwise	= do { void_tys <- mapM dsMkArbitraryType void_tvs
				     ; return (mkTyApps (mkLams void_tvs body) void_tys) }

    void_tvs = all_tvs \\ tvs

    dead_msg bs = vcat [ sep [ptext (sLit "Useless constraint") <> plural bs
				 <+> ptext (sLit "in specialied type:"),
			     nest 2 (pprTheta (map get_pred bs))]
		       , ptext (sLit "SPECIALISE pragma ignored")]
    get_pred b = ASSERT( isId b ) expectJust "dsSpec" (tcSplitPredTy_maybe (idType b))

    decomp_msg = hang (ptext (sLit "Specialisation too complicated to desugar; ignored"))
		    2 (ppr spec_expr)

    localise d = mkLocalId (localiseName (idName d)) (idType d)
    	     -- See Note [Constant rule dicts]

mkArbitraryTypeEnv :: [TyVar] -> [([TyVar], a, b, c)] -> DsM (TyVarEnv Type)
-- If any of the tyvars is missing from any of the lists in 
-- the second arg, return a binding in the result
mkArbitraryTypeEnv tyvars exports
  = go emptyVarEnv exports
  where
    go env [] = return env
    go env ((ltvs, _, _, _) : exports)
	= do { env' <- foldlM extend env [tv | tv <- tyvars
					, not (tv `elem` ltvs)
					, not (tv `elemVarEnv` env)]
	     ; go env' exports }

    extend env tv = do { ty <- dsMkArbitraryType tv
		       ; return (extendVarEnv env tv ty) }


dsMkArbitraryType :: TcTyVar -> DsM Type
dsMkArbitraryType tv = mkArbitraryType warn tv
  where
    warn span msg = putSrcSpanDs span (warnDs msg)
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
When the LHS of a specialisation rule, (/\as\ds. f es) has a free dict, 
which is presumably in scope at the function definition site, we can quantify 
over it too.  *Any* dict with that type will do.

So for example when you have
	f :: Eq a => a -> a
	f = <rhs>
	{-# SPECIALISE f :: Int -> Int #-}

Then we get the SpecPrag
	SpecPrag (f Int dInt) Int

And from that we want the rule
	
	RULE forall dInt. f Int dInt = f_spec
	f_spec = let f = <rhs> in f Int dInt

But be careful!  That dInt might be GHC.Base.$fOrdInt, which is an External
Name, and you can't bind them in a lambda or forall without getting things
confused. Hence the use of 'localise' to make it Internal.


%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
decomposeRuleLhs :: CoreExpr -> Maybe ([Var], Id, [CoreExpr])
-- Take apart the LHS of a RULE.  It's suuposed to look like
--     /\a. f a Int dOrdInt
-- or  /\a.\d:Ord a. let { dl::Ord [a] = dOrdList a d } in f [a] dl
-- That is, the RULE binders are lambda-bound
-- Returns Nothing if the LHS isn't of the expected shape
decomposeRuleLhs lhs 
  = case (decomp emptyVarEnv body) of
	Nothing 	-> Nothing
	Just (fn, args) -> Just (bndrs, fn, args)
  where
    occ_lhs = occurAnalyseExpr lhs
		-- The occurrence-analysis does two things
		-- (a) identifies unused binders: Note [Unused spec binders]
		-- (b) sorts dict bindings into NonRecs 
		--	so they can be inlined by 'decomp'
    (bndrs, body) = collectBinders occ_lhs

        -- Substitute dicts in the LHS args, so that there 
        -- aren't any lets getting in the way
        -- Note that we substitute the function too; we might have this as
        -- a LHS:       let f71 = M.f Int in f71
    decomp env (Let (NonRec dict rhs) body) 
        = decomp (extendVarEnv env dict (simpleSubst env rhs)) body
    decomp env body 
        = case collectArgs (simpleSubst env body) of
            (Var fn, args) -> Just (fn, args)
            _              -> Nothing

simpleSubst :: IdEnv CoreExpr -> CoreExpr -> CoreExpr
-- Similar to CoreSubst.substExpr, except that 
-- (a) Takes no account of capture; at this point there is no shadowing
-- (b) Can have a GlobalId (imported) in its domain
-- (c) Ids only; no types are substituted
-- (d) Does not insist (as does CoreSubst.lookupIdSubst) that the 
--     in-scope set mentions all LocalIds mentioned in the argument of the subst
--
-- (b) and (d) are the reasons we can't use CoreSubst
-- 
-- (I had a note that (b) is "no longer relevant", and indeed it doesn't
--  look relevant here. Perhaps there was another caller of simpleSubst.)

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
addDictScc :: Id -> CoreExpr -> DsM CoreExpr
addDictScc _ rhs = return rhs

{- DISABLED for now (need to somehow make up a name for the scc) -- SDM
  | not ( opt_SccProfilingOn && opt_AutoSccsOnDicts)
    || not (isDictId var)
  = return rhs				-- That's easy: do nothing

  | otherwise
  = do (mod, grp) <- getModuleAndGroupDs
	-- ToDo: do -dicts-all flag (mark dict things with individual CCs)
       return (Note (SCC (mkAllDictsCC mod grp False)) rhs)
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
dsCoercion (WpCast co)       thing_inside = do { expr <- thing_inside
					       ; return (Cast expr co) }
dsCoercion (WpLam id)        thing_inside = do { expr <- thing_inside
					       ; return (Lam id expr) }
dsCoercion (WpTyLam tv)      thing_inside = do { expr <- thing_inside
					       ; return (Lam tv expr) }
dsCoercion (WpApp v)         thing_inside   
	   | isTyVar v	    		  = do { expr <- thing_inside
		{- Probably a coercion var -}  ; return (App expr (Type (mkTyVarTy v))) }
	   | otherwise	    		  = do { expr <- thing_inside
		{- An Id -}		       ; return (App expr (Var v)) }
dsCoercion (WpTyApp ty)      thing_inside = do { expr <- thing_inside
					       ; return (App expr (Type ty)) }
dsCoercion WpInline 	     thing_inside = do { expr <- thing_inside
					       ; return (mkInlineMe expr) }
dsCoercion (WpLet bs)        thing_inside = do { prs <- dsLHsBinds bs
					       ; expr <- thing_inside
					       ; return (Let (Rec prs) expr) }
\end{code}
