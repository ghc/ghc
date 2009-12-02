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

import {-# SOURCE #-}	DsExpr( dsLExpr )
import {-# SOURCE #-}	Match( matchWrapper )

import DsMonad
import DsGRHSs
import DsUtils

import HsSyn		-- lots of things
import CoreSyn		-- lots of things
import CoreSubst
import MkCore
import CoreUtils
import CoreArity ( etaExpand )
import CoreUnfold
import CoreFVs

import TcType
import TysPrim  ( anyTypeOfKind )
import CostCentre
import Module
import Id
import MkId	( seqId )
import Var	( Var, TyVar, tyVarKind )
import IdInfo	( vanillaIdInfo )
import VarSet
import Rules
import VarEnv
import Outputable
import SrcLoc
import Maybes
import Bag
import BasicTypes hiding ( TopLevel )
import FastString
import StaticFlags	( opt_DsMultiTyVar )
import Util		( count, lengthExceeds )

import MonadUtils
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

dsHsBind _ rest (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless })
  = do	{ core_expr <- dsLExpr expr

	        -- Dictionary bindings are always VarBinds,
	        -- so we only need do this here
	; core_expr' <- addDictScc var core_expr
	; let var' | inline_regardless = var `setIdUnfolding` mkCompulsoryUnfolding core_expr'
	      	   | otherwise         = var

	; return ((var', core_expr') : rest) }

dsHsBind _ rest 
	 (FunBind { fun_id = L _ fun, fun_matches = matches, 
		    fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf }) 
 = do	{ (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
	; body'    <- mkOptTickBox tick body
	; wrap_fn' <- dsCoercion co_fn 
	; return ((fun, wrap_fn' (mkLams args body')) : rest) }

dsHsBind _ rest 
	 (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty })
  = do	{ body_expr <- dsGuarded grhss ty
	; sel_binds <- mkSelectorBinds pat body_expr
	; return (sel_binds ++ rest) }

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
	      ar_env = mkArityEnv binds
	      do_one (lcl_id, rhs) 
		| Just (_, gbl_id, _, spec_prags) <- lookupVarEnv env lcl_id
		= WARN( not (null spec_prags), ppr gbl_id $$ ppr spec_prags )	  -- Not overloaded
                  makeCorePair gbl_id (lookupArity ar_env lcl_id)
		               (addAutoScc auto_scc gbl_id rhs)

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
	h  = h a b    -- See (b); note shadowing!
  
  Notice (a) g has a different number of type variables to f, so we must
	     use the mkArbitraryType thing to fill in the gaps.  
	     We use a type-let to do that.

	 (b) The local variable h isn't in the exports, and rather than
	     clone a fresh copy we simply replace h by (h a b), where
	     the two h's have different types!  Shadowing happens here,
	     which looks confusing but works fine.

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
	; let arby_env = mkArbitraryTypeEnv tyvars exports
	      bndrs = mkVarSet (map fst core_prs)

	      add_lets | core_prs `lengthExceeds` 10 = add_some
		       | otherwise	             = mkLets
	      add_some lg_binds rhs = mkLets [ NonRec b r | NonRec b r <- lg_binds
				                          , b `elemVarSet` fvs] rhs
		where
		  fvs = exprSomeFreeVars (`elemVarSet` bndrs) rhs

	      ar_env = mkArityEnv binds
	      env = mkABEnv exports

	      mk_lg_bind lcl_id gbl_id tyvars
		 = NonRec (setIdInfo lcl_id vanillaIdInfo)
				-- Nuke the IdInfo so that no old unfoldings
				-- confuse use (it might mention something not
				-- even in scope at the new site
			  (mkTyApps (Var gbl_id) (mkTyVarTys tyvars))

	      do_one lg_binds (lcl_id, rhs) 
		| Just (id_tvs, gbl_id, _, spec_prags) <- lookupVarEnv env lcl_id
		= WARN( not (null spec_prags), ppr gbl_id $$ ppr spec_prags )	  -- Not overloaded
                  (let rhs' = addAutoScc auto_scc gbl_id  $
			      mkLams id_tvs $
			      mkLets [ NonRec tv (Type (lookupVarEnv_NF arby_env tv))
			             | tv <- tyvars, not (tv `elem` id_tvs)] $
		              add_lets lg_binds rhs
		  in return (mk_lg_bind lcl_id gbl_id id_tvs,
			     makeCorePair gbl_id (lookupArity ar_env lcl_id) rhs'))
		| otherwise
		= do { non_exp_gbl_id <- newUniqueId lcl_id (mkForAllTys tyvars (idType lcl_id))
		     ; return (mk_lg_bind lcl_id non_exp_gbl_id tyvars,
			      (non_exp_gbl_id, mkLams tyvars (add_lets lg_binds rhs))) }
						  
	; (_, core_prs') <- fixDs (\ ~(lg_binds, _) -> mapAndUnzipM (do_one lg_binds) core_prs)
	; return (core_prs' ++ rest) }

	-- Another common case: one exported variable
	-- Non-recursive bindings come through this way
	-- So do self-recursive bindings, and recursive bindings
	-- that have been chopped up with type signatures
dsHsBind auto_scc rest
     (AbsBinds all_tyvars dicts [(tyvars, global, local, prags)] binds)
  = ASSERT( all (`elem` tyvars) all_tyvars )
    do	{ core_prs <- ds_lhs_binds NoSccs binds

	; let	-- Always treat the binds as recursive, because the typechecker
	        -- makes rather mixed-up dictionary bindings
	        core_bind = Rec core_prs
		inl_arity = lookupArity (mkArityEnv binds) local
    
	; (spec_binds, rules) <- dsSpecs all_tyvars dicts tyvars global 
				         local inl_arity core_bind prags

	; let   global'   = addIdSpecialisations global rules
	        rhs       = addAutoScc auto_scc global $
			    mkLams tyvars $ mkLams dicts $ Let core_bind (Var local)
		main_bind = makeCorePair global' (inl_arity + dictArity dicts) rhs
    
	; return (main_bind : spec_binds ++ rest) }

dsHsBind auto_scc rest (AbsBinds all_tyvars dicts exports binds)
  = do	{ core_prs <- ds_lhs_binds NoSccs binds
	; let env = mkABEnv exports
	      ar_env = mkArityEnv binds
	      do_one (lcl_id,rhs) | Just (_, gbl_id, _, _prags) <- lookupVarEnv env lcl_id
			          = (lcl_id, addAutoScc auto_scc gbl_id rhs)
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

	; let mk_bind ((tyvars, global, local, spec_prags), n)  -- locals!!n == local
	        = 	-- Need to make fresh locals to bind in the selector,
		      	-- because some of the tyvars will be bound to 'Any'
		  do { let ty_args = map mk_ty_arg all_tyvars
		           substitute = substTyWith all_tyvars ty_args
		     ; locals' <- newSysLocalsDs (map substitute local_tys)
		     ; tup_id  <- newSysLocalDs  (substitute tup_ty)
		     ; (spec_binds, rules) <- dsSpecs all_tyvars dicts tyvars global local 
					              (lookupArity ar_env local) core_bind 
				                      spec_prags
		     ; let global' = addIdSpecialisations global rules
	                   rhs = mkLams tyvars $ mkLams dicts $
	      	     		 mkTupleSelector locals' (locals' !! n) tup_id $
			         mkVarApps (mkTyApps (Var poly_tup_id) ty_args)
			 		   dicts
		     ; return ((global', rhs) : spec_binds) }
	        where
	          mk_ty_arg all_tyvar
			| all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
	      		| otherwise		  = dsMkArbitraryType all_tyvar

	; export_binds_s <- mapM mk_bind (exports `zip` [0..])
	     -- Don't scc (auto-)annotate the tuple itself.

	; return ((poly_tup_id, poly_tup_expr) : 
		    (concat export_binds_s ++ rest)) }

------------------------
makeCorePair :: Id-> Arity -> CoreExpr -> (Id, CoreExpr)
makeCorePair gbl_id arity rhs
  | isInlinePragma (idInlinePragma gbl_id)
      	-- Add an Unfolding for an INLINE (but not for NOINLINE)
	-- And eta-expand the RHS; see Note [Eta-expanding INLINE things]
  = (gbl_id `setIdUnfolding` mkInlineRule needSaturated rhs arity,
     etaExpand arity rhs)
  | otherwise
  = (gbl_id, rhs)

------------------------
type AbsBindEnv = VarEnv ([TyVar], Id, Id, [LSpecPrag])
	-- Maps the "lcl_id" for an AbsBind to
	-- its "gbl_id" and associated pragmas, if any

mkABEnv :: [([TyVar], Id, Id, [LSpecPrag])] -> AbsBindEnv
-- Takes the exports of a AbsBinds, and returns a mapping
--	lcl_id -> (tyvars, gbl_id, lcl_id, prags)
mkABEnv exports = mkVarEnv [ (lcl_id, export) | export@(_, _, lcl_id, _) <- exports]

mkArityEnv :: LHsBinds Id -> IdEnv Arity
	-- Maps a local to the arity of its definition
mkArityEnv binds = foldrBag (plusVarEnv . lhsBindArity) emptyVarEnv binds

lhsBindArity :: LHsBind Id -> IdEnv Arity
lhsBindArity (L _ (FunBind { fun_id = id, fun_matches = ms })) 
  = unitVarEnv (unLoc id) (matchGroupArity ms)
lhsBindArity (L _ (AbsBinds { abs_exports = exports
                            , abs_dicts = dicts
                            , abs_binds = binds })) 
  = mkVarEnv [ (gbl, lookupArity ar_env lcl + n_val_dicts) 
             | (_, gbl, lcl, _) <- exports]
  where	     -- See Note [Nested arities] 
    ar_env = mkArityEnv binds
    n_val_dicts = dictArity dicts	

lhsBindArity _ = emptyVarEnv	-- PatBind/VarBind

dictArity :: [Var] -> Arity
-- Don't count coercion variables in arity
dictArity dicts = count isId dicts

lookupArity :: IdEnv Arity -> Id -> Arity
lookupArity ar_env id = lookupVarEnv ar_env id `orElse` 0
\end{code}

Note [Eta-expanding INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   foo :: Eq a => a -> a
   {-# INLINE foo #-}
   foo x = ...

If (foo d) ever gets floated out as a common sub-expression (which can
happen as a result of method sharing), there's a danger that we never 
get to do the inlining, which is a Terribly Bad thing given that the
user said "inline"!

To avoid this we pre-emptively eta-expand the definition, so that foo
has the arity with which it is declared in the source code.  In this
example it has arity 2 (one for the Eq and one for x). Doing this 
should mean that (foo d) is a PAP and we don't share it.

Note [Nested arities]
~~~~~~~~~~~~~~~~~~~~~
For reasons that are not entirely clear, method bindings come out looking like
this:

  AbsBinds [] [] [$cfromT <= [] fromT]
    $cfromT [InlPrag=INLINE] :: T Bool -> Bool
    { AbsBinds [] [] [fromT <= [] fromT_1]
        fromT :: T Bool -> Bool
        { fromT_1 ((TBool b)) = not b } } }

Note the nested AbsBind.  The arity for the InlineRule on $cfromT should be
gotten from the binding for fromT_1.

It might be better to have just one level of AbsBinds, but that requires more
thought!


\begin{code}
------------------------
dsSpecs :: [TyVar] -> [DictId] -> [TyVar]
        -> Id -> Id -> Arity		-- Global, local, arity of local
        -> CoreBind -> [LSpecPrag]
        -> DsM ( [(Id,CoreExpr)] 	-- Binding for specialised Ids
	       , [CoreRule] )		-- Rules for the Global Ids
-- Example:
--	f :: (Eq a, Ix b) => a -> b -> Bool
--	{-# SPECIALISE f :: (Ix p, Ix q) => Int -> (p,q) -> Bool #-}
--
--	AbsBinds [ab] [d1,d2] [([ab], f, f_mono, prags)] binds
-- 
-- 	SpecPrag /\pq.\(dp:Ix p, dq:Ix q). f Int (p,q) dInt ($dfIxPair dp dq)
--	     :: forall p q. (Ix p, Ix q) => Int -> (p,q) -> Bool 
--
--
-- Rule: 	forall p,q,(dp:Ix p),(dq:Ix q). 
--                 f Int (p,q) dInt ($dfInPair dp dq) = f_spec p q dp dq
--
-- Spec bind:	f_spec = Let f = /\ab \(d1:Eq a)(d2:Ix b). let binds in f_mono 
--			 /\pq.\(dp:Ix p, dq:Ix q). f Int (p,q) dInt ($dfIxPair dp dq)
--		The idea is that f occurs just once, so it'll be 
--		inlined and specialised
--
-- Note that the LHS of the rule may mention dictionary *expressions* 
--   (eg $dfIxPair dp dq), and that is essential because 
--   the dp, dq are needed on the RHS.
--
-- In general, given SpecPrag (/\as.\ds. f es) t, we have
-- the defn		f_spec as ds = let-nonrec f = /\fas\fds. let f_mono = <f-rhs> in f_mono
--				       in f es 
-- and the RULE		forall as, ds. f es = f_spec as ds
--
-- It is *possible* that 'es' does not mention all of the dictionaries 'ds'
-- (a bit silly, because then the 

dsSpecs all_tvs dicts tvs poly_id mono_id inl_arity mono_bind prags
  = do { pairs <- mapMaybeM spec_one prags
       ; let (spec_binds_s, rules) = unzip pairs
       ; return (concat spec_binds_s, rules) }
 where 
    spec_one :: LSpecPrag -> DsM (Maybe ([(Id,CoreExpr)], CoreRule))
    spec_one (L loc (SpecPrag spec_co spec_inl))
      = putSrcSpanDs loc $ 
        do { let poly_name = idName poly_id
	   ; spec_name <- newLocalName poly_name
	   ; wrap_fn   <- dsCoercion spec_co
           ; let ds_spec_expr = wrap_fn (Var poly_id)
	   ; case decomposeRuleLhs ds_spec_expr of {
	       Nothing -> do { warnDs (decomp_msg spec_co)
                             ; return Nothing } ;

	       Just (bndrs, _fn, args) ->

	   -- Check for dead binders: Note [Unused spec binders]
	     case filter isDeadBinder bndrs of {
	   	bs | not (null bs) -> do { warnDs (dead_msg bs); return Nothing } 
	   	   | otherwise -> do

	   { (spec_unf, unf_pairs) <- specUnfolding wrap_fn (realIdUnfolding poly_id)

	   ; let f_body = fix_up (Let mono_bind (Var mono_id))
                 spec_ty = exprType ds_spec_expr
              	 spec_id  = mkLocalId spec_name spec_ty 
              	            `setInlinePragma` inl_prag
	      	 	    `setIdUnfolding`  spec_unf
              	 inl_prag | isDefaultInlinePragma spec_inl = idInlinePragma poly_id
	      	 	  | otherwise                      = spec_inl
	   	      -- Get the INLINE pragma from SPECIALISE declaration, or,
                      -- failing that, from the original Id

              	 spec_id_arity = inl_arity + count isDictId bndrs

              	 extra_dict_bndrs = [ localiseId d  -- See Note [Constant rule dicts]
	      	 	  	    | d <- varSetElems (exprFreeVars ds_spec_expr)
	      	 	  	    , isDictId d]
	      	 		-- Note [Const rule dicts]

              	 rule =  mkLocalRule (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
	    			AlwaysActive poly_name
	    		        (extra_dict_bndrs ++ bndrs) args
	    			(mkVarApps (Var spec_id) bndrs)

                 spec_rhs = wrap_fn (mkLams (tvs ++ dicts) f_body)
                 spec_pair = makeCorePair spec_id spec_id_arity spec_rhs

	    ; return (Just (spec_pair : unf_pairs, rule))
	    } } } }

	-- Bind to Any any of all_ptvs that aren't 
	-- relevant for this particular function 
    fix_up body | null void_tvs = body
		| otherwise	= mkTyApps (mkLams void_tvs body) $
                                  map dsMkArbitraryType void_tvs

    void_tvs = all_tvs \\ tvs

    dead_msg bs = vcat [ sep [ptext (sLit "Useless constraint") <> plural bs
				 <+> ptext (sLit "in specialied type:"),
			     nest 2 (pprTheta (map get_pred bs))]
		       , ptext (sLit "SPECIALISE pragma ignored")]
    get_pred b = ASSERT( isId b ) expectJust "dsSpec" (tcSplitPredTy_maybe (idType b))

    decomp_msg spec_co 
        = hang (ptext (sLit "Specialisation too complicated to desugar; ignored"))
	     2 (pprHsWrapper (ppr poly_id) spec_co)
    	     

specUnfolding :: (CoreExpr -> CoreExpr) -> Unfolding -> DsM (Unfolding, [(Id,CoreExpr)])
specUnfolding wrap_fn (DFunUnfolding con ops)
  = do { let spec_rhss = map wrap_fn ops
       ; spec_ids <- mapM (mkSysLocalM (fsLit "spec") . exprType) spec_rhss
       ; return (DFunUnfolding con (map Var spec_ids), spec_ids `zip` spec_rhss) }
specUnfolding _ _
  = return (noUnfolding, [])

mkArbitraryTypeEnv :: [TyVar] -> [([TyVar], a, b, c)] -> TyVarEnv Type
-- If any of the tyvars is missing from any of the lists in 
-- the second arg, return a binding in the result
mkArbitraryTypeEnv tyvars exports
  = go emptyVarEnv exports
  where
    go env [] = env
    go env ((ltvs, _, _, _) : exports)
	= go env' exports
        where
          env' = foldl extend env [tv | tv <- tyvars
			              , not (tv `elem` ltvs)
				      , not (tv `elemVarEnv` env)]

    extend env tv = extendVarEnv env tv (dsMkArbitraryType tv)

dsMkArbitraryType :: TcTyVar -> Type
dsMkArbitraryType tv = anyTypeOfKind (tyVarKind tv)
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
	SpecPrag (f Int dInt) 

And from that we want the rule
	
	RULE forall dInt. f Int dInt = f_spec
	f_spec = let f = <rhs> in f Int dInt

But be careful!  That dInt might be GHC.Base.$fOrdInt, which is an External
Name, and you can't bind them in a lambda or forall without getting things
confused. Hence the use of 'localiseId' to make it Internal.


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
  = case collectArgs body of
        (Var fn, args) -> Just (bndrs, fn, args)

        (Case scrut bndr ty [(DEFAULT, _, body)], args)
	        | isDeadBinder bndr	-- Note [Matching seqId]
		-> Just (bndrs, seqId, args' ++ args)
		where
		   args' = [Type (idType bndr), Type ty, scrut, body]
	   
	_other -> Nothing	-- Unexpected shape
  where
    (bndrs, body) = collectBinders (simpleOptExpr lhs)
	-- simpleOptExpr occurrence-analyses and simplifies the lhs
	-- and thereby
	-- (a) identifies unused binders: Note [Unused spec binders]
	-- (b) sorts dict bindings into NonRecs 
	--	so they can be inlined by 'decomp'
	-- (c) substitute trivial lets so that they don't get in the way
	--     Note that we substitute the function too; we might 
	--     have this as a LHS:  let f71 = M.f Int in f71
        -- NB: tcSimplifyRuleLhs is very careful not to generate complicated
	--     dictionary expressions that we might have to match
\end{code}

Note [Matching seqId]
~~~~~~~~~~~~~~~~~~~
The desugarer turns (seq e r) into (case e of _ -> r), via a special-case hack
and this code turns it back into an application of seq!  
See Note [Rules for seq] in MkId for the details.


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
dsCoercion :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
dsCoercion WpHole 	     = return (\e -> e)
dsCoercion (WpCompose c1 c2) = do { k1 <- dsCoercion c1 
                                  ; k2 <- dsCoercion c2
                                  ; return (k1 . k2) }
dsCoercion (WpCast co)       = return (\e -> Cast e co) 
dsCoercion (WpLam id)        = return (\e -> Lam id e) 
dsCoercion (WpTyLam tv)      = return (\e -> Lam tv e) 
dsCoercion (WpApp v)         | isTyVar v   -- Probably a coercion var
                             = return (\e -> App e (Type (mkTyVarTy v)))
	                     | otherwise
                             = return (\e -> App e (Var v))
dsCoercion (WpTyApp ty)      = return (\e -> App e (Type ty))
dsCoercion (WpLet bs)        = do { prs <- dsLHsBinds bs
			          ; return (\e -> Let (Rec prs) e) }
\end{code}
