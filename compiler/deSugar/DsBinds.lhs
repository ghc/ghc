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
		 dsHsWrapper, dsTcEvBinds, dsEvBinds, wrapDsEvBinds, 
		 DsEvBind(..), AutoScc(..)
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
import Digraph

import TcType
import Type
import TysPrim  ( anyTypeOfKind )
import CostCentre
import Module
import Id
import TyCon	( tyConDataCons )
import Class
import DataCon	( dataConRepType )
import Name	( localiseName )
import MkId	( seqId )
import Var
import VarSet
import Rules
import VarEnv
import Outputable
import SrcLoc
import Maybes
import OrdList
import Bag
import BasicTypes hiding ( TopLevel )
import FastString
-- import StaticFlags	( opt_DsMultiTyVar )
import Util

import MonadUtils
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsTopLHsBinds :: AutoScc -> LHsBinds Id -> DsM [(Id,CoreExpr)]
dsTopLHsBinds auto_scc binds = do { binds' <- ds_lhs_binds auto_scc binds
                                  ; return (fromOL binds') }

dsLHsBinds :: LHsBinds Id -> DsM [(Id,CoreExpr)]
dsLHsBinds binds = do { binds' <- ds_lhs_binds NoSccs binds
                      ; return (fromOL binds') }

------------------------
ds_lhs_binds :: AutoScc -> LHsBinds Id -> DsM (OrdList (Id,CoreExpr))

	 -- scc annotation policy (see below)
ds_lhs_binds auto_scc binds = do { ds_bs <- mapBagM (dsLHsBind auto_scc) binds
                                 ; return (foldBag appOL id nilOL ds_bs) }

dsLHsBind :: AutoScc -> LHsBind Id -> DsM (OrdList (Id,CoreExpr))
dsLHsBind auto_scc (L loc bind)
  = putSrcSpanDs loc $ dsHsBind auto_scc bind

dsHsBind :: AutoScc -> HsBind Id -> DsM (OrdList (Id,CoreExpr))

dsHsBind _ (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless })
  = do	{ core_expr <- dsLExpr expr

	        -- Dictionary bindings are always VarBinds,
	        -- so we only need do this here
	; core_expr' <- addDictScc var core_expr
	; let var' | inline_regardless = var `setIdUnfolding` mkCompulsoryUnfolding core_expr'
	      	   | otherwise         = var

	; return (unitOL (var', core_expr')) }

dsHsBind _ (FunBind { fun_id = L _ fun, fun_matches = matches 
		    , fun_co_fn = co_fn, fun_tick = tick 
                    , fun_infix = inf }) 
 = do	{ (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
	; body'    <- mkOptTickBox tick body
	; wrap_fn' <- dsHsWrapper co_fn 
	; let rhs = wrap_fn' (mkLams args body')
	; return (unitOL (makeCorePair fun False 0 rhs)) }

dsHsBind _ (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty })
  = do	{ body_expr <- dsGuarded grhss ty
	; sel_binds <- mkSelectorBinds pat body_expr
	  -- We silently ignore inline pragmas; no makeCorePair
	  -- Not so cool, but really doesn't matter
	; return (toOL sel_binds) }

	-- A common case: one exported variable
	-- Non-recursive bindings come through this way
	-- So do self-recursive bindings, and recursive bindings
	-- that have been chopped up with type signatures
dsHsBind auto_scc (AbsBinds { abs_tvs = all_tyvars, abs_ev_vars = dicts
               		    , abs_exports = [(tyvars, global, local, prags)]
               		    , abs_ev_binds = ev_binds, abs_binds = binds })
  = ASSERT( all (`elem` tyvars) all_tyvars )
    do	{ bind_prs    <- ds_lhs_binds NoSccs binds
        ; ds_ev_binds <- dsTcEvBinds ev_binds

	; let	core_bind = Rec (fromOL bind_prs)
	        rhs       = addAutoScc auto_scc global $
			    mkLams tyvars $ mkLams dicts $ 
	                    wrapDsEvBinds ds_ev_binds $
                            Let core_bind $
                            Var local
    
	; (spec_binds, rules) <- dsSpecs global rhs prags

	; let   global'   = addIdSpecialisations global rules
		main_bind = makeCorePair global' (isDefaultMethod prags)
                                         (dictArity dicts) rhs 
    
	; return (main_bind `consOL` spec_binds) }

dsHsBind auto_scc (AbsBinds { abs_tvs = all_tyvars, abs_ev_vars = dicts
                            , abs_exports = exports, abs_ev_binds = ev_binds
                       	    , abs_binds = binds })
  = do	{ bind_prs    <- ds_lhs_binds NoSccs binds
        ; ds_ev_binds <- dsTcEvBinds ev_binds
	; let env = mkABEnv exports
	      do_one (lcl_id,rhs) | Just (_, gbl_id, _, _prags) <- lookupVarEnv env lcl_id
			          = (lcl_id, addAutoScc auto_scc gbl_id rhs)
				  | otherwise = (lcl_id,rhs)
	       
	      core_bind = Rec (map do_one (fromOL bind_prs))
	      	-- Monomorphic recursion possible, hence Rec

	      tup_expr     = mkBigCoreVarTup locals
	      tup_ty	   = exprType tup_expr
	      poly_tup_rhs = mkLams all_tyvars $ mkLams dicts $
	      		     wrapDsEvBinds ds_ev_binds $
			     Let core_bind $
	 	     	     tup_expr
	      locals       = [local | (_, _, local, _) <- exports]
	      local_tys    = map idType locals

	; poly_tup_id <- newSysLocalDs (exprType poly_tup_rhs)

	; let mk_bind ((tyvars, global, _, spec_prags), n)  -- locals!!n == local
	        = 	-- Need to make fresh locals to bind in the selector,
		      	-- because some of the tyvars will be bound to 'Any'
		  do { let ty_args = map mk_ty_arg all_tyvars
		           substitute = substTyWith all_tyvars ty_args
		     ; locals' <- newSysLocalsDs (map substitute local_tys)
		     ; tup_id  <- newSysLocalDs  (substitute tup_ty)
	             ; let rhs = mkLams tyvars $ mkLams dicts $
	      	     		 mkTupleSelector locals' (locals' !! n) tup_id $
			         mkVarApps (mkTyApps (Var poly_tup_id) ty_args)
			 		   dicts
		     ; (spec_binds, rules) <- dsSpecs global
					              (Let (NonRec poly_tup_id poly_tup_rhs) rhs)
				                      spec_prags
		     ; let global' = addIdSpecialisations global rules
		     ; return ((global', rhs) `consOL` spec_binds) }
	        where
	          mk_ty_arg all_tyvar
			| all_tyvar `elem` tyvars = mkTyVarTy all_tyvar
	      		| otherwise		  = dsMkArbitraryType all_tyvar

	; export_binds_s <- mapM mk_bind (exports `zip` [0..])
	     -- Don't scc (auto-)annotate the tuple itself.

	; return ((poly_tup_id, poly_tup_rhs) `consOL` 
		    concatOL export_binds_s) }

--------------------------------------
data DsEvBind 
  = LetEvBind		-- Dictionary or coercion
      CoreBind		-- recursive or non-recursive

  | CaseEvBind		-- Coercion binding by superclass selection
    			-- Desugars to case d of d { K _ g _ _ _ -> ... } 			
      DictId 		   -- b   The dictionary
      AltCon 		   -- K   Its constructor
      [CoreBndr] 	   -- _ g _ _ _   The binders in the alternative

wrapDsEvBinds :: [DsEvBind] -> CoreExpr -> CoreExpr
wrapDsEvBinds ds_ev_binds body = foldr wrap_one body ds_ev_binds
  where
    body_ty = exprType body
    wrap_one (LetEvBind b)       body = Let b body
    wrap_one (CaseEvBind x k xs) body = Case (Var x) x body_ty [(k,xs,body)]

dsTcEvBinds :: TcEvBinds -> DsM [DsEvBind]
dsTcEvBinds (TcEvBinds {}) = panic "dsEvBinds"	-- Zonker has got rid of this
dsTcEvBinds (EvBinds bs)   = dsEvBinds bs

dsEvBinds :: Bag EvBind -> DsM [DsEvBind]
dsEvBinds bs = return (map dsEvGroup sccs)
  where
    sccs :: [SCC EvBind]
    sccs = stronglyConnCompFromEdgedVertices edges

    edges :: [(EvBind, EvVar, [EvVar])]
    edges = foldrBag ((:) . mk_node) [] bs 

    mk_node :: EvBind -> (EvBind, EvVar, [EvVar])
    mk_node b@(EvBind var term) = (b, var, free_vars_of term)

    free_vars_of :: EvTerm -> [EvVar]
    free_vars_of (EvId v)           = [v]
    free_vars_of (EvCast v co)      = v : varSetElems (tyVarsOfType co)
    free_vars_of (EvCoercion co)    = varSetElems (tyVarsOfType co)
    free_vars_of (EvDFunApp _ _ vs) = vs
    free_vars_of (EvSuperClass d _) = [d]

dsEvGroup :: SCC EvBind -> DsEvBind
dsEvGroup (AcyclicSCC (EvBind co_var (EvSuperClass dict n)))
  | isCoVar co_var	 -- An equality superclass
  = ASSERT( null other_data_cons )
    CaseEvBind dict (DataAlt data_con) bndrs
  where
    (cls, tys) = getClassPredTys (evVarPred dict)
    (data_con:other_data_cons) = tyConDataCons (classTyCon cls)
    (ex_tvs, theta, rho) = tcSplitSigmaTy (applyTys (dataConRepType data_con) tys)
    (arg_tys, _) = splitFunTys rho
    bndrs = ex_tvs ++ map mk_wild_pred (theta `zip` [0..])
                   ++ map mkWildValBinder arg_tys
    mk_wild_pred (p, i) | i==n      = ASSERT( p `tcEqPred` (coVarPred co_var)) 
                                      co_var
                        | otherwise = mkWildEvBinder p
    
dsEvGroup (AcyclicSCC (EvBind v r))
  = LetEvBind (NonRec v (dsEvTerm r))

dsEvGroup (CyclicSCC bs)
  = LetEvBind (Rec (map ds_pair bs))
  where
    ds_pair (EvBind v r) = (v, dsEvTerm r)

dsEvTerm :: EvTerm -> CoreExpr
dsEvTerm (EvId v)      		 = Var v
dsEvTerm (EvCast v co) 		 = Cast (Var v) co 
dsEvTerm (EvDFunApp df tys vars) = Var df `mkTyApps` tys `mkVarApps` vars
dsEvTerm (EvCoercion co)         = Type co
dsEvTerm (EvSuperClass d n)
  = ASSERT( isClassPred (classSCTheta cls !! n) )
    	    -- We can only select *dictionary* superclasses
	    -- in terms.  Equality superclasses are dealt with
	    -- in dsEvGroup, where they can generate a case expression
    Var sc_sel_id `mkTyApps` tys `App` Var d
  where
    sc_sel_id  = classSCSelId cls n	-- Zero-indexed
    (cls, tys) = getClassPredTys (evVarPred d)    
    
------------------------
makeCorePair :: Id -> Bool -> Arity -> CoreExpr -> (Id, CoreExpr)
makeCorePair gbl_id is_default_method dict_arity rhs
  | is_default_method		      -- Default methods are *always* inlined
  = (gbl_id `setIdUnfolding` mkCompulsoryUnfolding rhs, rhs)

  | otherwise
  = case inlinePragmaSpec inline_prag of
      	  EmptyInlineSpec -> (gbl_id, rhs)
      	  NoInline        -> (gbl_id, rhs)
      	  Inlinable       -> (gbl_id `setIdUnfolding` inlinable_unf, rhs)
          Inline          -> inline_pair

  where
    inline_prag   = idInlinePragma gbl_id
    inlinable_unf = mkInlinableUnfolding rhs
    inline_pair
       | Just arity <- inlinePragmaSat inline_prag
      	-- Add an Unfolding for an INLINE (but not for NOINLINE)
	-- And eta-expand the RHS; see Note [Eta-expanding INLINE things]
       , let real_arity = dict_arity + arity
        -- NB: The arity in the InlineRule takes account of the dictionaries
       = ( gbl_id `setIdUnfolding` mkInlineUnfolding (Just real_arity) rhs
         , etaExpand real_arity rhs)

       | otherwise
       = pprTrace "makeCorePair: arity missing" (ppr gbl_id) $
         (gbl_id `setIdUnfolding` mkInlineUnfolding Nothing rhs, rhs)


dictArity :: [Var] -> Arity
-- Don't count coercion variables in arity
dictArity dicts = count isId dicts


------------------------
type AbsBindEnv = VarEnv ([TyVar], Id, Id, TcSpecPrags)
	-- Maps the "lcl_id" for an AbsBind to
	-- its "gbl_id" and associated pragmas, if any

mkABEnv :: [([TyVar], Id, Id, TcSpecPrags)] -> AbsBindEnv
-- Takes the exports of a AbsBinds, and returns a mapping
--	lcl_id -> (tyvars, gbl_id, lcl_id, prags)
mkABEnv exports = mkVarEnv [ (lcl_id, export) | export@(_, _, lcl_id, _) <- exports]
\end{code}

Note [Rules and inlining]
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
float the f_lcl binding out and then inline M.f at its call site

Note [Specialising in no-dict case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if there are no tyvars or dicts, we may have specialisation pragmas.
Class methods can generate
      AbsBinds [] [] [( ... spec-prag]
         { AbsBinds [tvs] [dicts] ...blah }
So the overloading is in the nested AbsBinds. A good example is in GHC.Float:

  class  (Real a, Fractional a) => RealFrac a  where
    round :: (Integral b) => a -> b

  instance  RealFrac Float  where
    {-# SPECIALIZE round :: Float -> Int #-}

The top-level AbsBinds for $cround has no tyvars or dicts (because the 
instance does not).  But the method is locally overloaded!

Note [Abstracting over tyvars only]
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

Note [Implementing SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example:
	f :: (Eq a, Ix b) => a -> b -> Bool
	{-# SPECIALISE f :: (Ix p, Ix q) => Int -> (p,q) -> Bool #-}
        f = <poly_rhs>

From this the typechecker generates

    AbsBinds [ab] [d1,d2] [([ab], f, f_mono, prags)] binds

    SpecPrag (wrap_fn :: forall a b. (Eq a, Ix b) => XXX
                      -> forall p q. (Ix p, Ix q) => XXX[ Int/a, (p,q)/b ])

Note that wrap_fn can transform *any* function with the right type prefix 
    forall ab. (Eq a, Ix b) => XXX
regardless of XXX.  It's sort of polymorphic in XXX.  This is
useful: we use the same wrapper to transform each of the class ops, as
well as the dict.

From these we generate:

    Rule: 	forall p, q, (dp:Ix p), (dq:Ix q). 
                    f Int (p,q) dInt ($dfInPair dp dq) = f_spec p q dp dq

    Spec bind:	f_spec = wrap_fn <poly_rhs>

Note that 

  * The LHS of the rule may mention dictionary *expressions* (eg
    $dfIxPair dp dq), and that is essential because the dp, dq are
    needed on the RHS.

  * The RHS of f_spec, <poly_rhs> has a *copy* of 'binds', so that it 
    can fully specialise it.

\begin{code}
------------------------
dsSpecs :: Id		-- The polymorphic Id
        -> CoreExpr     -- Its rhs
        -> TcSpecPrags
        -> DsM ( OrdList (Id,CoreExpr) 	-- Binding for specialised Ids
	       , [CoreRule] )		-- Rules for the Global Ids
-- See Note [Implementing SPECIALISE pragmas]
dsSpecs poly_id poly_rhs prags
  = case prags of
      IsDefaultMethod      -> return (nilOL, [])
      SpecPrags sps -> do { pairs <- mapMaybeM spec_one sps
                          ; let (spec_binds_s, rules) = unzip pairs
                          ; return (concatOL spec_binds_s, rules) }
 where 
    spec_one :: Located TcSpecPrag -> DsM (Maybe (OrdList (Id,CoreExpr), CoreRule))
    spec_one (L loc (SpecPrag spec_co spec_inl))
      = putSrcSpanDs loc $ 
        do { let poly_name = idName poly_id
	   ; spec_name <- newLocalName poly_name
	   ; wrap_fn   <- dsHsWrapper spec_co
           ; let (bndrs, ds_lhs) = collectBinders (wrap_fn (Var poly_id))
                 spec_ty = mkPiTypes bndrs (exprType ds_lhs)
	   ; case decomposeRuleLhs ds_lhs of {
	       Nothing -> do { warnDs (decomp_msg spec_co)
                             ; return Nothing } ;

	       Just (_fn, args) ->

	   -- Check for dead binders: Note [Unused spec binders]
             let arg_fvs = exprsFreeVars args
                 bad_bndrs = filterOut (`elemVarSet` arg_fvs) bndrs
	     in if not (null bad_bndrs)
                then do { warnDs (dead_msg bad_bndrs); return Nothing } 
	   	else do

	   { (spec_unf, unf_pairs) <- specUnfolding wrap_fn spec_ty (realIdUnfolding poly_id)

	   ; let spec_id  = mkLocalId spec_name spec_ty 
              	            `setInlinePragma` inl_prag
	      	 	    `setIdUnfolding`  spec_unf
              	 inl_prag | isDefaultInlinePragma spec_inl = idInlinePragma poly_id
	      	 	  | otherwise                      = spec_inl
	   	      -- Get the INLINE pragma from SPECIALISE declaration, or,
                      -- failing that, from the original Id

              	 extra_dict_bndrs = [ mkLocalId (localiseName (idName d)) (idType d)
                                            -- See Note [Constant rule dicts]
	      	 	  	    | d <- varSetElems (arg_fvs `delVarSetList` bndrs)
	      	 	  	    , isDictId d]

              	 rule =  mkLocalRule (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
	    			AlwaysActive poly_name
	    		        (extra_dict_bndrs ++ bndrs) args
	    			(mkVarApps (Var spec_id) bndrs)

                 spec_rhs  = wrap_fn poly_rhs
                 spec_pair = makeCorePair spec_id False (dictArity bndrs) spec_rhs

	    ; return (Just (spec_pair `consOL` unf_pairs, rule))
	    } } }

    dead_msg bs = vcat [ sep [ptext (sLit "Useless constraint") <> plural bs
				 <+> ptext (sLit "in specialied type:"),
			     nest 2 (pprTheta (map get_pred bs))]
		       , ptext (sLit "SPECIALISE pragma ignored")]
    get_pred b = ASSERT( isId b ) expectJust "dsSpec" (tcSplitPredTy_maybe (idType b))

    decomp_msg spec_co 
        = hang (ptext (sLit "Specialisation too complicated to desugar; ignored"))
	     2 (pprHsWrapper (ppr poly_id) spec_co)
    	     

specUnfolding :: (CoreExpr -> CoreExpr) -> Type 
              -> Unfolding -> DsM (Unfolding, OrdList (Id,CoreExpr))
specUnfolding wrap_fn spec_ty (DFunUnfolding _ _ ops)
  = do { let spec_rhss = map wrap_fn ops
       ; spec_ids <- mapM (mkSysLocalM (fsLit "spec") . exprType) spec_rhss
       ; return (mkDFunUnfolding spec_ty (map Var spec_ids), toOL (spec_ids `zip` spec_rhss)) }
specUnfolding _ _ _
  = return (noUnfolding, nilOL)

{-
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
-}

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

Note [Constant rule dicts]
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
confused.   Likewise it might have an InlineRule or something, which would be
utterly bogus. So we really make a fresh Id, with the same unique and type
as the old one, but with an Internal name and no IdInfo.

%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
decomposeRuleLhs :: CoreExpr -> Maybe (Id, [CoreExpr])
-- Take apart the LHS of a RULE.  It's suuposed to look like
--     /\a. f a Int dOrdInt
-- or  /\a.\d:Ord a. let { dl::Ord [a] = dOrdList a d } in f [a] dl
-- That is, the RULE binders are lambda-bound
-- Returns Nothing if the LHS isn't of the expected shape
decomposeRuleLhs lhs 
  =  -- Note [Simplifying the left-hand side of a RULE]
    case collectArgs (simpleOptExpr lhs) of
        (Var fn, args) -> Just (fn, args)

        (Case scrut bndr ty [(DEFAULT, _, body)], args)
	        | isDeadBinder bndr	-- Note [Matching seqId]
		-> Just (seqId, args' ++ args)
		where
		   args' = [Type (idType bndr), Type ty, scrut, body]
	   
	_other -> Nothing	-- Unexpected shape
\end{code}

Note [Simplifying the left-hand side of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simpleOptExpr occurrence-analyses and simplifies the lhs
and thereby
(a) sorts dict bindings into NonRecs and inlines them
(b) substitute trivial lets so that they don't get in the way
    Note that we substitute the function too; we might 
    have this as a LHS:  let f71 = M.f Int in f71
(c) does eta reduction

For (c) consider the fold/build rule, which without simplification
looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.
Similarly for a LHS like
	augment g (build h) 
we do not want to get
	augment (\a. g a) (build h)
otherwise we don't match when given an argument like
	augment (\a. h a a) (build h)

NB: tcSimplifyRuleLhs is very careful not to generate complicated
    dictionary expressions that we might have to match


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
-- But we never add a SCC to function marked INLINE

addAutoScc :: AutoScc	
	   -> Id	-- Binder
	   -> CoreExpr 	-- Rhs
	   -> CoreExpr	-- Scc'd Rhs

addAutoScc NoSccs _ rhs
  = rhs
addAutoScc _ id rhs | isInlinePragma (idInlinePragma id)
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
dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
dsHsWrapper WpHole 	      = return (\e -> e)
dsHsWrapper (WpTyApp ty)      = return (\e -> App e (Type ty))
dsHsWrapper (WpLet ev_binds)  = do { ds_ev_binds <- dsTcEvBinds ev_binds
                                   ; return (wrapDsEvBinds ds_ev_binds) }
dsHsWrapper (WpCompose c1 c2) = do { k1 <- dsHsWrapper c1 
                                   ; k2 <- dsHsWrapper c2
                                   ; return (k1 . k2) }
dsHsWrapper (WpCast co)       = return (\e -> Cast e co) 
dsHsWrapper (WpEvLam ev)      = return (\e -> Lam ev e) 
dsHsWrapper (WpTyLam tv)      = return (\e -> Lam tv e) 
dsHsWrapper (WpEvApp evtrm)   = return (\e -> App e (dsEvTerm evtrm))
\end{code}
