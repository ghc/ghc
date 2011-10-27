%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Pattern-matching bindings (HsBinds and MonoBinds)

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).

\begin{code}
module DsBinds ( dsTopLHsBinds, dsLHsBinds, decomposeRuleLhs, dsSpec,
                 dsHsWrapper, dsTcEvBinds, dsEvBinds,
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

import TyCon      ( isTupleTyCon, tyConDataCons_maybe )
import TcType
import Type
import Coercion hiding (substCo)
import TysWiredIn ( eqBoxDataCon, tupleCon )
import Id
import Class
import DataCon	( dataConWorkId )
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
import Util

import MonadUtils
\end{code}

%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

\begin{code}
dsTopLHsBinds :: LHsBinds Id -> DsM (OrdList (Id,CoreExpr))
dsTopLHsBinds binds = ds_lhs_binds binds

dsLHsBinds :: LHsBinds Id -> DsM [(Id,CoreExpr)]
dsLHsBinds binds = do { binds' <- ds_lhs_binds binds
                      ; return (fromOL binds') }

------------------------
ds_lhs_binds :: LHsBinds Id -> DsM (OrdList (Id,CoreExpr))

ds_lhs_binds binds = do { ds_bs <- mapBagM dsLHsBind binds
                        ; return (foldBag appOL id nilOL ds_bs) }

dsLHsBind :: LHsBind Id -> DsM (OrdList (Id,CoreExpr))
dsLHsBind (L loc bind)
  = putSrcSpanDs loc $ dsHsBind bind

dsHsBind :: HsBind Id -> DsM (OrdList (Id,CoreExpr))

dsHsBind (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless })
  = do  { core_expr <- dsLExpr expr

	        -- Dictionary bindings are always VarBinds,
	        -- so we only need do this here
        ; let var' | inline_regardless = var `setIdUnfolding` mkCompulsoryUnfolding core_expr
	      	   | otherwise         = var

        ; return (unitOL (makeCorePair var' False 0 core_expr)) }

dsHsBind (FunBind { fun_id = L _ fun, fun_matches = matches
                  , fun_co_fn = co_fn, fun_tick = tick
                  , fun_infix = inf })
 = do	{ (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
        ; let body' = mkOptTickBox tick body
        ; wrap_fn' <- dsHsWrapper co_fn
        ; let rhs = wrap_fn' (mkLams args body')
        ; {- pprTrace "dsHsBind" (ppr fun <+> ppr (idInlinePragma fun)) $ -}
           return (unitOL (makeCorePair fun False 0 rhs)) }

dsHsBind (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) })
  = do	{ body_expr <- dsGuarded grhss ty
        ; let body' = mkOptTickBox rhs_tick body_expr
        ; sel_binds <- mkSelectorBinds var_ticks pat body'
	  -- We silently ignore inline pragmas; no makeCorePair
	  -- Not so cool, but really doesn't matter
    ; return (toOL sel_binds) }

	-- A common case: one exported variable
	-- Non-recursive bindings come through this way
	-- So do self-recursive bindings, and recursive bindings
	-- that have been chopped up with type signatures
dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = [export]
                   , abs_ev_binds = ev_binds, abs_binds = binds })
  | ABE { abe_wrap = wrap, abe_poly = global
        , abe_mono = local, abe_prags = prags } <- export
  = do  { bind_prs    <- ds_lhs_binds binds
        ; ds_ev_binds <- dsTcEvBinds ev_binds
        ; wrap_fn <- dsHsWrapper wrap
	; let	core_bind = Rec (fromOL bind_prs)
                rhs       = wrap_fn $  -- Usually the identity
			    mkLams tyvars $ mkLams dicts $ 
	                    mkCoreLets ds_ev_binds $
                            Let core_bind $
                            Var local
    
	; (spec_binds, rules) <- dsSpecs rhs prags

	; let   global'   = addIdSpecialisations global rules
		main_bind = makeCorePair global' (isDefaultMethod prags)
                                         (dictArity dicts) rhs 
    
	; return (main_bind `consOL` spec_binds) }

dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = exports, abs_ev_binds = ev_binds
                   , abs_binds = binds })
  = do  { bind_prs    <- ds_lhs_binds binds
        ; ds_ev_binds <- dsTcEvBinds ev_binds
        ; let core_bind = Rec (fromOL bind_prs)
	      	-- Monomorphic recursion possible, hence Rec

	      tup_expr     = mkBigCoreVarTup locals
	      tup_ty	   = exprType tup_expr
	      poly_tup_rhs = mkLams tyvars $ mkLams dicts $
	      		     mkCoreLets ds_ev_binds $
			     Let core_bind $
	 	     	     tup_expr
	      locals       = map abe_mono exports

	; poly_tup_id <- newSysLocalDs (exprType poly_tup_rhs)

	; let mk_bind (ABE { abe_wrap = wrap, abe_poly = global
                           , abe_mono = local, abe_prags = spec_prags })
	        = do { wrap_fn <- dsHsWrapper wrap
		     ; tup_id  <- newSysLocalDs tup_ty
	             ; let rhs = wrap_fn $ mkLams tyvars $ mkLams dicts $
	      	     		 mkTupleSelector locals local tup_id $
			         mkVarApps (Var poly_tup_id) (tyvars ++ dicts)
                           rhs_for_spec = Let (NonRec poly_tup_id poly_tup_rhs) rhs
		     ; (spec_binds, rules) <- dsSpecs rhs_for_spec spec_prags
		     ; let global' = addIdSpecialisations global rules
		     ; return ((global', rhs) `consOL` spec_binds) }

        ; export_binds_s <- mapM mk_bind exports

	; return ((poly_tup_id, poly_tup_rhs) `consOL` 
		    concatOL export_binds_s) }

--------------------------------------
dsTcEvBinds :: TcEvBinds -> DsM [CoreBind]
dsTcEvBinds (TcEvBinds {}) = panic "dsEvBinds"	-- Zonker has got rid of this
dsTcEvBinds (EvBinds bs)   = dsEvBinds bs

dsEvBinds :: Bag EvBind -> DsM [CoreBind]
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
    free_vars_of (EvCast v co)      = v : varSetElems (tyCoVarsOfCo co)
    free_vars_of (EvCoercionBox co) = varSetElems (tyCoVarsOfCo co)
    free_vars_of (EvDFunApp _ _ vs) = vs
    free_vars_of (EvTupleSel v _)   = [v]
    free_vars_of (EvTupleMk vs)     = vs
    free_vars_of (EvSuperClass d _) = [d]

dsEvGroup :: SCC EvBind -> CoreBind

dsEvGroup (AcyclicSCC (EvBind v r))
  = NonRec v (dsEvTerm r)

dsEvGroup (CyclicSCC bs)
  = Rec (map ds_pair bs)
  where
    ds_pair (EvBind v r) = (v, dsEvTerm r)

---------------------------------------
dsLCoercion :: LCoercion -> (Coercion -> CoreExpr) -> CoreExpr
-- This is the crucial function that moves 
-- from LCoercions to Coercions; see Note [LCoercions] in Coercion
-- e.g.  dsLCoercion (trans g1 g2) k
--       = case g1 of EqBox g1# ->
--         case g2 of EqBox g2# ->
--         k (trans g1# g2#)
dsLCoercion co k 
  = foldr wrap_in_case result_expr eqvs_covs
  where
    result_expr = k (substCo subst co)
    result_ty   = exprType result_expr

    -- We use the same uniques for the EqVars and the CoVars, and just change
    -- the type. So the CoVars shadow the EqVars
    --
    -- NB: DON'T try to cheat and not substitute into the LCoercion to change the
    -- types of the free variables: -ddump-ds will panic if you do this since it
    -- runs Lint before we substitute CoVar occurrences out for their binding sites.
    eqvs_covs = [(eqv, eqv `setIdType` mkCoercionType ty1 ty2)
                | eqv <- varSetElems (coVarsOfCo co)
                , let (ty1, ty2) = getEqPredTys (evVarPred eqv)]

    subst = extendCvSubstList (mkEmptySubst (mkInScopeSet (tyCoVarsOfCo co)))
                              [(eqv, mkCoVarCo cov) | (eqv, cov) <- eqvs_covs]

    wrap_in_case (eqv, cov) body 
      = Case (Var eqv) eqv result_ty [(DataAlt eqBoxDataCon, [cov], body)]

---------------------------------------
dsEvTerm :: EvTerm -> CoreExpr
dsEvTerm (EvId v)                = Var v
dsEvTerm (EvCast v co)           = dsLCoercion co $ Cast (Var v)
dsEvTerm (EvDFunApp df tys vars) = Var df `mkTyApps` tys `mkVarApps` vars
dsEvTerm (EvCoercionBox co)      = dsLCoercion co mkEqBox
dsEvTerm (EvTupleSel v n)
   = ASSERT( isTupleTyCon tc )
     Case (Var v) (mkWildValBinder (varType v)) (tys !! n) [(DataAlt dc, xs, Var v')]
  where
    (tc, tys) = splitTyConApp (evVarPred v)
    Just [dc] = tyConDataCons_maybe tc
    v' = v `setVarType` ty_want
    xs = map mkWildValBinder tys_before ++ v' : map mkWildValBinder tys_after
    (tys_before, ty_want:tys_after) = splitAt n tys
dsEvTerm (EvTupleMk vs) = Var (dataConWorkId dc) `mkTyApps` tys `mkVarApps` vs
  where dc = tupleCon ConstraintTuple (length vs)
        tys = map varType vs
dsEvTerm (EvSuperClass d n)
  = Var sc_sel_id `mkTyApps` tys `App` Var d
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
dsSpecs :: CoreExpr     -- Its rhs
        -> TcSpecPrags
        -> DsM ( OrdList (Id,CoreExpr) 	-- Binding for specialised Ids
	       , [CoreRule] )		-- Rules for the Global Ids
-- See Note [Implementing SPECIALISE pragmas]
dsSpecs _ IsDefaultMethod = return (nilOL, [])
dsSpecs poly_rhs (SpecPrags sps)
  = do { pairs <- mapMaybeM (dsSpec (Just poly_rhs)) sps
       ; let (spec_binds_s, rules) = unzip pairs
       ; return (concatOL spec_binds_s, rules) }

dsSpec :: Maybe CoreExpr  	-- Just rhs => RULE is for a local binding
       	  			-- Nothing => RULE is for an imported Id
				-- 	      rhs is in the Id's unfolding
       -> Located TcSpecPrag
       -> DsM (Maybe (OrdList (Id,CoreExpr), CoreRule))
dsSpec mb_poly_rhs (L loc (SpecPrag poly_id spec_co spec_inl))
  | isJust (isClassOpId_maybe poly_id)
  = putSrcSpanDs loc $ 
    do { warnDs (ptext (sLit "Ignoring useless SPECIALISE pragma for class method selector") 
                 <+> quotes (ppr poly_id))
       ; return Nothing  }  -- There is no point in trying to specialise a class op
       	 		    -- Moreover, classops don't (currently) have an inl_sat arity set
			    -- (it would be Just 0) and that in turn makes makeCorePair bleat

  | otherwise
  = putSrcSpanDs loc $ 
    do { let poly_name = idName poly_id
       ; spec_name <- newLocalName poly_name
       ; wrap_fn   <- dsHsWrapper spec_co
       ; let (bndrs, ds_lhs) = collectBinders (wrap_fn (Var poly_id))
             spec_ty = mkPiTypes bndrs (exprType ds_lhs)
       ; case decomposeRuleLhs bndrs ds_lhs of {
           Left msg -> do { warnDs msg; return Nothing } ;
           Right (final_bndrs, _fn, args) -> do

       { (spec_unf, unf_pairs) <- specUnfolding wrap_fn spec_ty (realIdUnfolding poly_id)

       ; let spec_id  = mkLocalId spec_name spec_ty 
         	            `setInlinePragma` inl_prag
         	 	    `setIdUnfolding`  spec_unf
             inl_prag | not (isDefaultInlinePragma spec_inl)    = spec_inl
         	      | not is_local_id  -- See Note [Specialising imported functions]
		      	    		 -- in OccurAnal
                      , isStrongLoopBreaker (idOccInfo poly_id) = neverInlinePragma
		      | otherwise                               = idInlinePragma poly_id
       	      -- Get the INLINE pragma from SPECIALISE declaration, or,
              -- failing that, from the original Id

             rule =  mkRule False {- Not auto -} is_local_id
                        (mkFastString ("SPEC " ++ showSDoc (ppr poly_name)))
       			AlwaysActive poly_name
       		        final_bndrs args
       			(mkVarApps (Var spec_id) bndrs)

             spec_rhs  = wrap_fn poly_rhs
             spec_pair = makeCorePair spec_id False (dictArity bndrs) spec_rhs

       ; return (Just (spec_pair `consOL` unf_pairs, rule))
       } } }
  where
    is_local_id = isJust mb_poly_rhs
    poly_rhs | Just rhs <-  mb_poly_rhs
             = rhs  	    -- Local Id; this is its rhs
             | Just unfolding <- maybeUnfoldingTemplate (realIdUnfolding poly_id)
             = unfolding    -- Imported Id; this is its unfolding
	       		    -- Use realIdUnfolding so we get the unfolding 
			    -- even when it is a loop breaker. 
			    -- We want to specialise recursive functions!
             | otherwise = pprPanic "dsImpSpecs" (ppr poly_id)
	                    -- The type checker has checked that it *has* an unfolding

specUnfolding :: (CoreExpr -> CoreExpr) -> Type 
              -> Unfolding -> DsM (Unfolding, OrdList (Id,CoreExpr))
{-   [Dec 10: TEMPORARILY commented out, until we can straighten out how to
              generate unfoldings for specialised DFuns

specUnfolding wrap_fn spec_ty (DFunUnfolding _ _ ops)
  = do { let spec_rhss = map wrap_fn ops
       ; spec_ids <- mapM (mkSysLocalM (fsLit "spec") . exprType) spec_rhss
       ; return (mkDFunUnfolding spec_ty (map Var spec_ids), toOL (spec_ids `zip` spec_rhss)) }
-}
specUnfolding _ _ _
  = return (noUnfolding, nilOL)
\end{code}

%************************************************************************
%*									*
\subsection{Adding inline pragmas}
%*									*
%************************************************************************

\begin{code}
decomposeRuleLhs :: [Var] -> CoreExpr -> Either SDoc ([Var], Id, [CoreExpr])
-- Take apart the LHS of a RULE.  It's supposed to look like
--     /\a. f a Int dOrdInt
-- or  /\a.\d:Ord a. let { dl::Ord [a] = dOrdList a d } in f [a] dl
-- That is, the RULE binders are lambda-bound
-- Returns Nothing if the LHS isn't of the expected shape
decomposeRuleLhs bndrs lhs 
  =  -- Note [Simplifying the left-hand side of a RULE]
    case collectArgs opt_lhs of
        (Var fn, args) -> check_bndrs fn args

        (Case scrut bndr ty [(DEFAULT, _, body)], args)
	        | isDeadBinder bndr	-- Note [Matching seqId]
		-> check_bndrs seqId (args' ++ args)
		where
		   args' = [Type (idType bndr), Type ty, scrut, body]
	   
	_other -> Left bad_shape_msg
 where
   opt_lhs = simpleOptExpr lhs

   check_bndrs fn args
     | null (dead_bndrs) = Right (extra_dict_bndrs ++ bndrs, fn, args)
     | otherwise         = Left (vcat (map dead_msg dead_bndrs))
     where
       arg_fvs = exprsFreeVars args

            -- Check for dead binders: Note [Unused spec binders]
       dead_bndrs = filterOut (`elemVarSet` arg_fvs) bndrs

            -- Add extra dict binders: Note [Constant rule dicts]
       extra_dict_bndrs = [ mkLocalId (localiseName (idName d)) (idType d)
                          | d <- varSetElems (arg_fvs `delVarSetList` bndrs)
         	          , isDictId d]


   bad_shape_msg = hang (ptext (sLit "RULE left-hand side too complicated to desugar"))
                      2 (ppr opt_lhs)
   dead_msg bndr = hang (sep [ ptext (sLit "Forall'd") <+> pp_bndr bndr
			     , ptext (sLit "is not bound in RULE lhs")])
                      2 (ppr opt_lhs)
   pp_bndr bndr
    | isTyVar bndr                      = ptext (sLit "type variable") <+> quotes (ppr bndr)
    | Just pred <- evVarPred_maybe bndr = ptext (sLit "constraint") <+> quotes (ppr pred)
    | otherwise                         = ptext (sLit "variable") <+> quotes (ppr bndr)
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
		Desugaring coercions
%*									*
%************************************************************************


\begin{code}
dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
dsHsWrapper WpHole 	      = return (\e -> e)
dsHsWrapper (WpTyApp ty)      = return (\e -> App e (Type ty))
dsHsWrapper (WpLet ev_binds)  = do { ds_ev_binds <- dsTcEvBinds ev_binds
                                   ; return (mkCoreLets ds_ev_binds) }
dsHsWrapper (WpCompose c1 c2) = do { k1 <- dsHsWrapper c1 
                                   ; k2 <- dsHsWrapper c2
                                   ; return (k1 . k2) }
dsHsWrapper (WpCast co)
  = return (\e -> dsLCoercion co (Cast e)) 
dsHsWrapper (WpEvLam ev)      = return (\e -> Lam ev e) 
dsHsWrapper (WpTyLam tv)      = return (\e -> Lam tv e) 
dsHsWrapper (WpEvApp evtrm)
  = return (\e -> App e (dsEvTerm evtrm))
\end{code}
