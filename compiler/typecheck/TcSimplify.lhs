\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcSimplify( 
       simplifyInfer, simplifyAmbiguityCheck,
       simplifyDefault, simplifyDeriv, 
       simplifyRule, simplifyTop, simplifyInteractive
  ) where

#include "HsVersions.h"

import TcRnMonad
import TcErrors
import TcMType
import TcType 
import TcSMonad 
import TcInteract 
import Inst
import Unify	( niFixTvSubst, niSubstTvSet )
import Var
import VarSet
import VarEnv 
import TcEvidence
import TypeRep
import Name
import NameEnv	( emptyNameEnv )
import Bag
import ListSetOps
import Util
import PrelInfo
import PrelNames
import Class		( classKey )
import BasicTypes       ( RuleName )
import Control.Monad    ( when )
import Outputable
import FastString
import TrieMap

\end{code}


*********************************************************************************
*                                                                               * 
*                           External interface                                  *
*                                                                               *
*********************************************************************************

\begin{code}
simplifyTop :: WantedConstraints -> TcM (Bag EvBind)
-- Simplify top-level constraints
-- Usually these will be implications,
-- but when there is nothing to quantify we don't wrap
-- in a degenerate implication, so we do that here instead
simplifyTop wanteds 
  = simplifyCheck (SimplCheck (ptext (sLit "top level"))) wanteds

------------------
simplifyAmbiguityCheck :: Name -> WantedConstraints -> TcM (Bag EvBind)
simplifyAmbiguityCheck name wanteds
  = simplifyCheck (SimplCheck (ptext (sLit "ambiguity check for") <+> ppr name)) wanteds
 
------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds 
  = simplifyCheck SimplInteractive wanteds

------------------
simplifyDefault :: ThetaType	-- Wanted; has no type variables in it
                -> TcM ()	-- Succeeds iff the constraint is soluble
simplifyDefault theta
  = do { wanted <- newFlatWanteds DefaultOrigin theta
       ; _ignored_ev_binds <- simplifyCheck (SimplCheck (ptext (sLit "defaults"))) 
                                            (mkFlatWC wanted)
       ; return () }
\end{code}


***********************************************************************************
*                                                                                 * 
*                            Deriving                                             *
*                                                                                 *
***********************************************************************************

\begin{code}
simplifyDeriv :: CtOrigin
              -> PredType
	      -> [TyVar]	
	      -> ThetaType		-- Wanted
	      -> TcM ThetaType	-- Needed
-- Given  instance (wanted) => C inst_ty 
-- Simplify 'wanted' as much as possibles
-- Fail if not possible
simplifyDeriv orig pred tvs theta 
  = do { tvs_skols <- tcInstSkolTyVars tvs -- Skolemize
      	 	-- The constraint solving machinery 
		-- expects *TcTyVars* not TyVars.  
		-- We use *non-overlappable* (vanilla) skolems
		-- See Note [Overlap and deriving]

       ; let skol_subst = zipTopTvSubst tvs $ map mkTyVarTy tvs_skols
             subst_skol = zipTopTvSubst tvs_skols $ map mkTyVarTy tvs
             skol_set   = mkVarSet tvs_skols
	     doc = parens $ ptext (sLit "deriving") <+> parens (ppr pred)

       ; wanted <- newFlatWanteds orig (substTheta skol_subst theta)

       ; traceTc "simplifyDeriv" (ppr tvs $$ ppr theta $$ ppr wanted)
       ; (residual_wanted, _binds)
             <- solveWanteds (SimplInfer doc) NoUntouchables $
                mkFlatWC wanted

       ; let (good, bad) = partitionBagWith get_good (wc_flat residual_wanted)
                         -- See Note [Exotic derived instance contexts]
             get_good :: Ct -> Either PredType Ct
             get_good ct | validDerivPred skol_set p = Left p
                         | otherwise                 = Right ct
                         where p = ctPred ct

       ; reportUnsolved (residual_wanted { wc_flat = bad })

       ; let min_theta = mkMinimalBySCs (bagToList good)
       ; return (substTheta subst_skol min_theta) }
\end{code}

Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider some overlapping instances:
  data Show a => Show [a] where ..
  data Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance. 
	     Hence tcInstSkolTyVars not tcInstSuperSkolTyVars

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
	data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
	data T a b c = MkT (Foo a b c) deriving( Eq )
	instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination 
conditions.  Then we *could* derive an instance decl like this:

	instance (C Int a, Eq b, Eq c) => Eq (T a b c) 
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.  

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
	data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
	instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.

*********************************************************************************
*                                                                                 * 
*                            Inference
*                                                                                 *
***********************************************************************************

Note [Which variables to quantify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the inferred type of a function is
   T kappa (alpha:kappa) -> Int
where alpha is a type unification variable and 
      kappa is a kind unification variable
Then we want to quantify over *both* alpha and kappa.  But notice that
kappa appears "at top level" of the type, as well as inside the kind
of alpha.  So it should be fine to just look for the "top level"
kind/type variables of the type, without looking transitively into the
kinds of those type variables.

\begin{code}
simplifyInfer :: Bool
              -> Bool                  -- Apply monomorphism restriction
              -> [(Name, TcTauType)]   -- Variables to be generalised,
                                       -- and their tau-types
              -> WantedConstraints
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints
		      Bool,	    -- The monomorphism restriction did something
		      		    --   so the results type is not as general as
				    --   it could be
                      TcEvBinds)    -- ... binding these evidence variables
simplifyInfer _top_lvl apply_mr name_taus wanteds
  | isEmptyWC wanteds
  = do { gbl_tvs     <- tcGetGlobalTyVars            -- Already zonked
       ; zonked_taus <- zonkTcTypes (map snd name_taus)
       ; let tvs_to_quantify = tyVarsOfTypes zonked_taus `minusVarSet` gbl_tvs
       	     		       -- tvs_to_quantify can contain both kind and type vars
       	                       -- See Note [Which variables to quantify]
       ; qtvs <- zonkQuantifiedTyVars tvs_to_quantify
       ; return (qtvs, [], False, emptyTcEvBinds) }

  | otherwise
  = do { zonked_wanteds <- zonkWC wanteds
       ; zonked_taus    <- zonkTcTypes (map snd name_taus)
       ; gbl_tvs        <- tcGetGlobalTyVars

       ; traceTc "simplifyInfer {"  $ vcat
             [ ptext (sLit "names =") <+> ppr (map fst name_taus)
             , ptext (sLit "taus (zonked) =") <+> ppr zonked_taus
             , ptext (sLit "gbl_tvs =") <+> ppr gbl_tvs
             , ptext (sLit "closed =") <+> ppr _top_lvl
             , ptext (sLit "apply_mr =") <+> ppr apply_mr
             , ptext (sLit "wanted =") <+> ppr zonked_wanteds
             ]

             -- Step 1
             -- Make a guess at the quantified type variables
	     -- Then split the constraints on the baisis of those tyvars
	     -- to avoid unnecessarily simplifying a class constraint
	     -- See Note [Avoid unecessary constraint simplification]
       ; let zonked_tau_tvs = tyVarsOfTypes zonked_taus
             proto_qtvs = growWanteds gbl_tvs zonked_wanteds $
                          zonked_tau_tvs `minusVarSet` gbl_tvs
             (perhaps_bound, surely_free)
                        = partitionBag (quantifyMe proto_qtvs) (wc_flat zonked_wanteds)

       ; traceTc "simplifyInfer proto"  $ vcat
             [ ptext (sLit "zonked_tau_tvs =") <+> ppr zonked_tau_tvs
             , ptext (sLit "proto_qtvs =") <+> ppr proto_qtvs
             , ptext (sLit "surely_fref =") <+> ppr surely_free
             ]

       ; emitWantedCts surely_free
       ; traceTc "sinf"  $ vcat
             [ ptext (sLit "perhaps_bound =") <+> ppr perhaps_bound
             , ptext (sLit "surely_free   =") <+> ppr surely_free
             ]

            -- Step 2 
       	    -- Now simplify the possibly-bound constraints
       ; (simpl_results, tc_binds0)
           <- runTcS (SimplInfer (ppr (map fst name_taus))) NoUntouchables emptyInert emptyWorkList $
              simplifyWithApprox (zonked_wanteds { wc_flat = perhaps_bound })

       ; when (insolubleWC simpl_results)  -- Fail fast if there is an insoluble constraint
              (do { reportUnsolved simpl_results; failM })

            -- Step 3 
            -- Split again simplified_perhaps_bound, because some unifications 
            -- may have happened, and emit the free constraints. 
       ; gbl_tvs        <- tcGetGlobalTyVars
       ; zonked_tau_tvs <- zonkTcTyVarsAndFV zonked_tau_tvs
       ; zonked_simples <- zonkCts (wc_flat simpl_results)
       ; let init_tvs 	     = zonked_tau_tvs `minusVarSet` gbl_tvs
             poly_qtvs       = growWantedEVs gbl_tvs zonked_simples init_tvs
	     (pbound, pfree) = partitionBag (quantifyMe poly_qtvs) zonked_simples

	     -- Monomorphism restriction
             mr_qtvs  	     = init_tvs `minusVarSet` constrained_tvs
             constrained_tvs = tyVarsOfCts zonked_simples
	     mr_bites        = apply_mr && not (isEmptyBag pbound)

             (qtvs, (bound, free))
                | mr_bites  = (mr_qtvs,   (emptyBag, zonked_simples))
                | otherwise = (poly_qtvs, (pbound,   pfree))
       ; emitWantedCts free

       ; if isEmptyVarSet qtvs && isEmptyBag bound
         then ASSERT( isEmptyBag (wc_insol simpl_results) )
              do { traceTc "} simplifyInfer/no quantification" empty
                 ; emitImplications (wc_impl simpl_results)
                 ; return ([], [], mr_bites, EvBinds tc_binds0) }
         else do

            -- Step 4, zonk quantified variables 
       { let minimal_flat_preds = mkMinimalBySCs $ 
                                  map ctPred $ bagToList bound
             skol_info = InferSkol [ (name, mkSigmaTy [] minimal_flat_preds ty)
                                   | (name, ty) <- name_taus ]
                        -- Don't add the quantified variables here, because
                        -- they are also bound in ic_skols and we want them to be
                        -- tidied uniformly

       ; qtvs_to_return <- zonkQuantifiedTyVars qtvs

            -- Step 5
            -- Minimize `bound' and emit an implication
       ; minimal_bound_ev_vars <- mapM TcMType.newEvVar minimal_flat_preds
       ; ev_binds_var <- newTcEvBinds
       ; mapBagM_ (\(EvBind evar etrm) -> addTcEvBind ev_binds_var evar etrm) tc_binds0
       ; lcl_env <- getLclTypeEnv
       ; gloc <- getCtLoc skol_info
       ; let implic = Implic { ic_untch    = NoUntouchables
                             , ic_env      = lcl_env
                             , ic_skols    = mkVarSet qtvs_to_return
                             , ic_given    = minimal_bound_ev_vars
                             , ic_wanted   = simpl_results { wc_flat = bound }
                             , ic_insol    = False
                             , ic_binds    = ev_binds_var
                             , ic_loc      = gloc }
       ; emitImplication implic
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
             vcat [ ptext (sLit "implic =") <+> ppr implic
                       -- ic_skols, ic_given give rest of result
                  , ptext (sLit "qtvs =") <+> ppr qtvs_to_return
                  , ptext (sLit "spb =") <+> ppr zonked_simples
                  , ptext (sLit "bound =") <+> ppr bound ]



       ; return ( qtvs_to_return, minimal_bound_ev_vars
                , mr_bites,  TcEvBinds ev_binds_var) } }
\end{code}


Note [Minimize by Superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

When we quantify over a constraint, in simplifyInfer we need to
quantify over a constraint that is minimal in some sense: For
instance, if the final wanted constraint is (Eq alpha, Ord alpha),
we'd like to quantify over Ord alpha, because we can just get Eq alpha
from superclass selection from Ord alpha. This minimization is what
mkMinimalBySCs does. Then, simplifyInfer uses the minimal constraint
to check the original wanted.

\begin{code}

simplifyWithApprox :: WantedConstraints -> TcS WantedConstraints
-- Post: returns only wanteds (no deriveds)
simplifyWithApprox wanted
 = do { traceTcS "simplifyApproxLoop" (ppr wanted)

      ; let all_flats = wc_flat wanted `unionBags` keepWanted (wc_insol wanted) 
      ; solveInteractCts $ bagToList all_flats
      ; unsolved_implics <- simpl_loop 1 (wc_impl wanted)

      ; let (residual_implics,floats) = approximateImplications unsolved_implics

      -- Solve extra stuff for real: notice that all the extra unsolved constraints will 
      -- be in the inerts of the monad, so we are OK
      ; traceTcS "simplifyApproxLoop" $ text "Calling solve_wanteds!"
      ; wants_or_ders <- solve_wanteds (WC { wc_flat  = floats -- They are floated so they are not in the evvar cache
                                           , wc_impl  = residual_implics
                                           , wc_insol = emptyBag })
      ; return $ 
        wants_or_ders { wc_flat = keepWanted (wc_flat wants_or_ders) } }


approximateImplications :: Bag Implication -> (Bag Implication, Cts)
-- Extracts any nested constraints that don't mention the skolems
approximateImplications impls
  = do_bag (float_implic emptyVarSet) impls
  where 
    do_bag :: forall a b c. (a -> (Bag b, Bag c)) -> Bag a -> (Bag b, Bag c)
    do_bag f = foldrBag (plus . f) (emptyBag, emptyBag)
    plus :: forall b c. (Bag b, Bag c) -> (Bag b, Bag c) -> (Bag b, Bag c)
    plus (a1,b1) (a2,b2) = (a1 `unionBags` a2, b1 `unionBags` b2)

    float_implic :: TyVarSet -> Implication -> (Bag Implication, Cts)
    float_implic skols imp
      = (unitBag (imp { ic_wanted = wanted' }), floats)
      where
        (wanted', floats) = float_wc (skols `unionVarSet` ic_skols imp) (ic_wanted imp)

    float_wc skols wc@(WC { wc_flat = flat, wc_impl = implic })
      = (wc { wc_flat = flat', wc_impl = implic' }, floats1 `unionBags` floats2)
      where
        (flat',   floats1) = do_bag (float_flat   skols) flat
        (implic', floats2) = do_bag (float_implic skols) implic

    float_flat :: TcTyVarSet -> Ct -> (Cts, Cts)
    float_flat skols ct
      | tyVarsOfCt ct `disjointVarSet` skols = (emptyBag, unitBag ct)
      | otherwise                            = (unitBag ct, emptyBag)
\end{code}

\begin{code}
-- (growX gbls wanted tvs) grows a seed 'tvs' against the 
-- X-constraint 'wanted', nuking the 'gbls' at each stage
-- It's conservative in that if the seed could *possibly*
-- grow to include a type variable, then it does

growWanteds :: TyVarSet -> WantedConstraints -> TyVarSet -> TyVarSet
growWanteds gbl_tvs wc = fixVarSet (growWC gbl_tvs wc)

growWantedEVs :: TyVarSet -> Cts -> TyVarSet -> TyVarSet
growWantedEVs gbl_tvs ws tvs
  | isEmptyBag ws = tvs
  | otherwise     = fixVarSet (growPreds gbl_tvs ctPred ws) tvs

--------  Helper functions, do not do fixpoint ------------------------
growWC :: TyVarSet -> WantedConstraints -> TyVarSet -> TyVarSet
growWC gbl_tvs wc = growImplics gbl_tvs             (wc_impl wc) .
                    growPreds   gbl_tvs ctPred (wc_flat wc) .
                    growPreds   gbl_tvs ctPred (wc_insol wc)

growImplics :: TyVarSet -> Bag Implication -> TyVarSet -> TyVarSet
growImplics gbl_tvs implics tvs
  = foldrBag grow_implic tvs implics
  where
    grow_implic implic tvs
      = grow tvs `minusVarSet` ic_skols implic
      where
        grow = growWC gbl_tvs (ic_wanted implic) .
               growPreds gbl_tvs evVarPred (listToBag (ic_given implic))
               -- We must grow from givens too; see test IPRun

growPreds :: TyVarSet -> (a -> PredType) -> Bag a -> TyVarSet -> TyVarSet
growPreds gbl_tvs get_pred items tvs
  = foldrBag extend tvs items
  where
    extend item tvs = tvs `unionVarSet`
                      (growPredTyVars (get_pred item) tvs `minusVarSet` gbl_tvs)

--------------------
quantifyMe :: TyVarSet      -- Quantifying over these
	   -> Ct
	   -> Bool	    -- True <=> quantify over this wanted
quantifyMe qtvs ct
  | isIPPred pred = True  -- Note [Inheriting implicit parameters]
  | otherwise	  = tyVarsOfType pred `intersectsVarSet` qtvs
  where
    pred = ctPred ct
\end{code}

Note [Avoid unecessary constraint simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When inferring the type of a let-binding, with simplifyInfer,
try to avoid unnecessariliy simplifying class constraints.
Doing so aids sharing, but it also helps with delicate 
situations like
   instance C t => C [t] where ..
   f :: C [t] => ....
   f x = let g y = ...(constraint C [t])... 
         in ...
When inferring a type for 'g', we don't want to apply the
instance decl, because then we can't satisfy (C t).  So we
just notice that g isn't quantified over 't' and partition
the contraints before simplifying.

This only half-works, but then let-generalisation only half-works.


Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

	f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

	f :: Int -> Int

(so we get ?y from the context of f's definition), or

	f :: (?y::Int) => Int -> Int

At first you might think the first was better, becuase then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you *must* quantify 
over implicit parameters. See the predicate isFreeWhenInferring.


*********************************************************************************
*                                                                                 * 
*                             RULES                                               *
*                                                                                 *
***********************************************************************************

Note [Simplifying RULE lhs constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the LHS of transformation rules we only simplify only equalities,
but not dictionaries.  We want to keep dictionaries unsimplified, to
serve as the available stuff for the RHS of the rule.  We *do* want to
simplify equalities, however, to detect ill-typed rules that cannot be
applied.

Implementation: the TcSFlags carried by the TcSMonad controls the
amount of simplification, so simplifyRuleLhs just sets the flag
appropriately.

Example.  Consider the following left-hand side of a rule
	f (x == y) (y > z) = ...
If we typecheck this expression we get constraints
	d1 :: Ord a, d2 :: Eq a
We do NOT want to "simplify" to the LHS
	forall x::a, y::a, z::a, d1::Ord a.
	  f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...
Instead we want	
	forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
	  f ((==) d2 x y) ((>) d1 y z) = ...

Here is another example:
	fromIntegral :: (Integral a, Num b) => a -> b
	{-# RULES "foo"  fromIntegral = id :: Int -> Int #-}
In the rule, a=b=Int, and Num Int is a superclass of Integral Int. But
we *dont* want to get
	forall dIntegralInt.
	   fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int
because the scsel will mess up RULE matching.  Instead we want
	forall dIntegralInt, dNumInt.
	  fromIntegral Int Int dIntegralInt dNumInt = id Int

Even if we have 
	g (x == y) (y == z) = ..
where the two dictionaries are *identical*, we do NOT WANT
	forall x::a, y::a, z::a, d1::Eq a
	  f ((==) d1 x y) ((>) d1 y z) = ...
because that will only match if the dict args are (visibly) equal.
Instead we want to quantify over the dictionaries separately.

In short, simplifyRuleLhs must *only* squash equalities, leaving
all dicts unchanged, with absolutely no sharing.  

HOWEVER, under a nested implication things are different
Consider
  f :: (forall a. Eq a => a->a) -> Bool -> ...
  {-# RULES "foo" forall (v::forall b. Eq b => b->b).
       f b True = ...
    #=}
Here we *must* solve the wanted (Eq a) from the given (Eq a)
resulting from skolemising the agument type of g.  So we 
revert to SimplCheck when going under an implication.  

\begin{code}
simplifyRule :: RuleName 
             -> [TcTyVar]		-- Explicit skolems
             -> WantedConstraints	-- Constraints from LHS
             -> WantedConstraints	-- Constraints from RHS
             -> TcM ([EvVar], 		-- LHS dicts
                     TcEvBinds,		-- Evidence for LHS
                     TcEvBinds)		-- Evidence for RHS
-- See Note [Simplifying RULE lhs constraints]
simplifyRule name tv_bndrs lhs_wanted rhs_wanted
  = do { loc        <- getCtLoc (RuleSkol name)
       ; zonked_lhs <- zonkWC lhs_wanted
       ; let untch = NoUntouchables
	     	 -- We allow ourselves to unify environment 
		 -- variables; hence *no untouchables*

       ; (lhs_results, lhs_binds)
              <- solveWanteds (SimplRuleLhs name) untch zonked_lhs

       ; traceTc "simplifyRule" $
         vcat [ text "zonked_lhs"   <+> ppr zonked_lhs 
              , text "lhs_results" <+> ppr lhs_results
              , text "lhs_binds"    <+> ppr lhs_binds 
              , text "rhs_wanted"   <+> ppr rhs_wanted ]


       -- Don't quantify over equalities (judgement call here)
       ; let (eqs, dicts) = partitionBag (isEqPred . ctPred)
                                         (wc_flat lhs_results)
             lhs_dicts    = map cc_id (bagToList dicts)
                                 -- Dicts and implicit parameters

           -- Fail if we have not got down to unsolved flats
       ; ev_binds_var <- newTcEvBinds
       ; emitImplication $ Implic { ic_untch  = untch
                                  , ic_env    = emptyNameEnv
                                  , ic_skols  = mkVarSet tv_bndrs
                                  , ic_given  = lhs_dicts
                                  , ic_wanted = lhs_results { wc_flat = eqs }
                                  , ic_insol  = insolubleWC lhs_results
                                  , ic_binds  = ev_binds_var
                                  , ic_loc    = loc }

	     -- Notice that we simplify the RHS with only the explicitly
	     -- introduced skolems, allowing the RHS to constrain any 
	     -- unification variables.
	     -- Then, and only then, we call zonkQuantifiedTypeVariables
	     -- Example   foo :: Ord a => a -> a
	     --		  foo_spec :: Int -> Int
	     --		  {-# RULE "foo"  foo = foo_spec #-}
	     --	    Here, it's the RHS that fixes the type variable

	     -- So we don't want to make untouchable the type
	     -- variables in the envt of the RHS, because they include
	     -- the template variables of the RULE

	     -- Hence the rather painful ad-hoc treatement here
       ; rhs_binds_var@(EvBindsVar evb_ref _)  <- newTcEvBinds
       ; let doc = ptext (sLit "rhs of rule") <+> doubleQuotes (ftext name)
       ; rhs_binds1 <- simplifyCheck (SimplCheck doc) $
            WC { wc_flat = emptyBag
               , wc_insol = emptyBag
               , wc_impl = unitBag $
                    Implic { ic_untch   = NoUntouchables
                            , ic_env    = emptyNameEnv
                            , ic_skols  = mkVarSet tv_bndrs
                            , ic_given  = lhs_dicts
                            , ic_wanted = rhs_wanted
                            , ic_insol  = insolubleWC rhs_wanted
                            , ic_binds  = rhs_binds_var
                            , ic_loc    = loc } }
       ; rhs_binds2 <- readTcRef evb_ref

       ; return ( lhs_dicts
                , EvBinds lhs_binds 
                , EvBinds (rhs_binds1 `unionBags` evBindMapBinds rhs_binds2)) }
\end{code}


*********************************************************************************
*                                                                                 * 
*                                 Main Simplifier                                 *
*                                                                                 *
***********************************************************************************

\begin{code}
simplifyCheck :: SimplContext
	      -> WantedConstraints	-- Wanted
              -> TcM (Bag EvBind)
-- Solve a single, top-level implication constraint
-- e.g. typically one created from a top-level type signature
-- 	    f :: forall a. [a] -> [a]
--          f x = rhs
-- We do this even if the function has no polymorphism:
--    	    g :: Int -> Int

--          g y = rhs
-- (whereas for *nested* bindings we would not create
--  an implication constraint for g at all.)
--
-- Fails if can't solve something in the input wanteds
simplifyCheck ctxt wanteds
  = do { wanteds <- zonkWC wanteds

       ; traceTc "simplifyCheck {" (vcat
             [ ptext (sLit "wanted =") <+> ppr wanteds ])

       ; (unsolved, ev_binds) <- 
           solveWanteds ctxt NoUntouchables wanteds

       ; traceTc "simplifyCheck }" $
         ptext (sLit "unsolved =") <+> ppr unsolved

       ; reportUnsolved unsolved

       ; return ev_binds }

----------------
solveWanteds :: SimplContext 
             -> Untouchables
             -> WantedConstraints
             -> TcM (WantedConstraints, Bag EvBind)
-- Returns: residual constraints, plus evidence bindings 
-- NB: When we are called from TcM there are no inerts to pass down to TcS
solveWanteds ctxt untch wanted
  = do { (wc_out, ev_binds) <- runTcS ctxt untch emptyInert emptyWorkList $
                               solve_wanteds wanted
       ; let wc_ret = wc_out { wc_flat = keepWanted (wc_flat wc_out) } 
                      -- Discard Derived
       ; return (wc_ret, ev_binds) }

solve_wanteds :: WantedConstraints
              -> TcS WantedConstraints  -- NB: wc_flats may be wanted *or* derived now
solve_wanteds wanted@(WC { wc_flat = flats, wc_impl = implics, wc_insol = insols }) 
  = do { traceTcS "solveWanteds {" (ppr wanted)

                 -- Try the flat bit
                 -- Discard from insols all the derived/given constraints
                 -- because they will show up again when we try to solve
                 -- everything else.  Solving them a second time is a bit
                 -- of a waste, but the code is simple, and the program is
                 -- wrong anyway!

       ; let all_flats = flats `unionBags` keepWanted insols
       ; solveInteractCts $ bagToList all_flats

       -- solve_wanteds iterates when it is able to float equalities 
       -- out of one or more of the implications. 
       ; unsolved_implics <- simpl_loop 1 implics

       ; (insoluble_flats,unsolved_flats) <- extractUnsolvedTcS 

       ; bb <- getTcEvBindsMap
       ; tb <- getTcSTyBindsMap

       ; traceTcS "solveWanteds }" $
                 vcat [ text "unsolved_flats   =" <+> ppr unsolved_flats
                      , text "unsolved_implics =" <+> ppr unsolved_implics
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb)
                      , text "current tybinds  =" <+> vcat (map ppr (varEnvElts tb))
                      ]

       ; (subst, remaining_unsolved_flats) <- solveCTyFunEqs unsolved_flats
                -- See Note [Solving Family Equations]
                -- NB: remaining_flats has already had subst applied

       ; return $ 
         WC { wc_flat  = mapBag (substCt subst) remaining_unsolved_flats
            , wc_impl  = mapBag (substImplication subst) unsolved_implics
            , wc_insol = mapBag (substCt subst) insoluble_flats }
       }

simpl_loop :: Int
           -> Bag Implication
           -> TcS (Bag Implication)
simpl_loop n implics
  | n > 10 
  = traceTcS "solveWanteds: loop!" empty >> return implics
  | otherwise 
  = do { (implic_eqs, unsolved_implics) <- solveNestedImplications implics

       ; inerts <- getTcSInerts
       ; let ((_,unsolved_flats),_) = extractUnsolved inerts

       ; ecache_pre <- getTcSEvVarCacheMap
       ; let pr = ppr ((\k z m -> foldTM k m z) (:) [] ecache_pre)
       ; traceTcS "ecache_pre"  $ pr

       ; improve_eqs <- if not (isEmptyBag implic_eqs)
                        then return implic_eqs
                        else applyDefaultingRules unsolved_flats

       ; ecache_post <- getTcSEvVarCacheMap
       ; let po = ppr ((\k z m -> foldTM k m z) (:) [] ecache_post)
       ; traceTcS "ecache_po"  $ po

       ; traceTcS "solveWanteds: simpl_loop end" $
             vcat [ text "improve_eqs      =" <+> ppr improve_eqs
                  , text "unsolved_flats   =" <+> ppr unsolved_flats
                  , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; if isEmptyBag improve_eqs then return unsolved_implics 
         else do { solveInteractCts $ bagToList improve_eqs
                 ; simpl_loop (n+1) unsolved_implics } }

solveNestedImplications :: Bag Implication
                        -> TcS (Cts, Bag Implication)
-- Precondition: the TcS inerts may contain unsolved flats which have 
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return (emptyBag, emptyBag)
  | otherwise 
  = do { inerts <- getTcSInerts
       ; let ((_insoluble_flats, unsolved_flats),thinner_inerts) = extractUnsolved inerts 

       ; (implic_eqs, unsolved_implics)
           <- doWithInert thinner_inerts $ 
              do { let pushed_givens = givens_from_wanteds unsolved_flats
                       tcs_untouchables = filterVarSet isFlexiTcsTv $ 
                                          tyVarsOfCts unsolved_flats
                 -- See Note [Preparing inert set for implications]
	         -- Push the unsolved wanteds inwards, but as givens
                 ; traceTcS "solveWanteds: preparing inerts for implications {" $ 
                   vcat [ppr tcs_untouchables, ppr pushed_givens]
                 ; solveInteractCts pushed_givens 
                 ; traceTcS "solveWanteds: } now doing nested implications {" empty
                 ; flatMapBagPairM (solveImplication tcs_untouchables) implics }

       -- ... and we are back in the original TcS inerts 
       -- Notice that the original includes the _insoluble_flats so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveWanteds: done nested implications }" $
                  vcat [ text "implic_eqs ="       <+> ppr implic_eqs
                       , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return (implic_eqs, unsolved_implics) }

  where givens_from_wanteds = foldrBag get_wanted []
        get_wanted cc rest_givens
            | pushable_wanted cc
            = let this_given = cc { cc_flavor = mkGivenFlavor (cc_flavor cc) UnkSkol }
              in this_given : rest_givens
            | otherwise = rest_givens 

        pushable_wanted :: Ct -> Bool 
        pushable_wanted cc 
         | isWantedCt cc 
         = isEqPred (ctPred cc) -- see Note [Preparing inert set for implications]
         | otherwise = False 

solveImplication :: TcTyVarSet     -- Untouchable TcS unification variables
                 -> Implication    -- Wanted
                 -> TcS (Cts,      -- All wanted or derived floated equalities: var = type
                         Bag Implication) -- Unsolved rest (always empty or singleton)
-- Precondition: The TcS monad contains an empty worklist and given-only inerts 
-- which after trying to solve this implication we must restore to their original value
solveImplication tcs_untouchables
     imp@(Implic { ic_untch  = untch
                 , ic_binds  = ev_binds
                 , ic_skols  = skols 
                 , ic_given  = givens
                 , ic_wanted = wanteds
                 , ic_loc    = loc })
  = nestImplicTcS ev_binds (untch, tcs_untouchables) $
    recoverTcS (return (emptyBag, emptyBag)) $
       -- Recover from nested failures.  Even the top level is
       -- just a bunch of implications, so failing at the first one is bad
    do { traceTcS "solveImplication {" (ppr imp) 

         -- Solve flat givens
       ; solveInteractGiven loc givens 

         -- Simplify the wanteds
       ; WC { wc_flat = unsolved_flats
            , wc_impl = unsolved_implics
            , wc_insol = insols } <- solve_wanteds wanteds

       ; let (res_flat_free, res_flat_bound)
                 = floatEqualities skols givens unsolved_flats
             final_flat = keepWanted res_flat_bound

       ; let res_wanted = WC { wc_flat  = final_flat
                             , wc_impl  = unsolved_implics
                             , wc_insol = insols }

             res_implic = unitImplication $
                          imp { ic_wanted = res_wanted
                              , ic_insol  = insolubleWC res_wanted }

       ; evbinds <- getTcEvBindsMap

       ; traceTcS "solveImplication end }" $ vcat
             [ text "res_flat_free =" <+> ppr res_flat_free
             , text "implication evbinds = " <+> ppr (evBindMapBinds evbinds)
             , text "res_implic =" <+> ppr res_implic ]

       ; return (res_flat_free, res_implic) }
    -- and we are back to the original inerts


floatEqualities :: TcTyVarSet -> [EvVar] -> Cts -> (Cts, Cts)
-- Post: The returned FlavoredEvVar's are only Wanted or Derived
-- and come from the input wanted ev vars or deriveds 
floatEqualities skols can_given wantders
  | hasEqualities can_given = (emptyBag, wantders)
          -- Note [Float Equalities out of Implications]
  | otherwise = partitionBag is_floatable wantders
  
  where is_floatable :: Ct -> Bool
        is_floatable ct
          | ct_predty <- ctPred ct
          , isEqPred ct_predty
          = skols `disjointVarSet` tvs_under_fsks ct_predty
        is_floatable _ct = False

        tvs_under_fsks :: Type -> TyVarSet
        -- ^ NB: for type synonyms tvs_under_fsks does /not/ expand the synonym
        tvs_under_fsks (TyVarTy tv)     
          | not (isTcTyVar tv)               = unitVarSet tv
          | FlatSkol ty <- tcTyVarDetails tv = tvs_under_fsks ty
          | otherwise                        = unitVarSet tv
        tvs_under_fsks (TyConApp _ tys) = unionVarSets (map tvs_under_fsks tys)
        tvs_under_fsks (FunTy arg res)  = tvs_under_fsks arg `unionVarSet` tvs_under_fsks res
        tvs_under_fsks (AppTy fun arg)  = tvs_under_fsks fun `unionVarSet` tvs_under_fsks arg
        tvs_under_fsks (ForAllTy tv ty) -- The kind of a coercion binder 
        	     	       	        -- can mention type variables!
          | isTyVar tv		      = inner_tvs `delVarSet` tv
          | otherwise  {- Coercion -} = -- ASSERT( not (tv `elemVarSet` inner_tvs) )
                                        inner_tvs `unionVarSet` tvs_under_fsks (tyVarKind tv)
          where
            inner_tvs = tvs_under_fsks ty
\end{code}

Note [Preparing inert set for implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before solving the nested implications, we convert any unsolved flat wanteds
to givens, and add them to the inert set.  Reasons:

  a) In checking mode, suppresses unnecessary errors.  We already have
     on unsolved-wanted error; adding it to the givens prevents any 
     consequential errors from showing up

  b) More importantly, in inference mode, we are going to quantify over this
     constraint, and we *don't* want to quantify over any constraints that
     are deducible from it.

  c) Flattened type-family equalities must be exposed to the nested
     constraints.  Consider
	F b ~ alpha, (forall c.  F b ~ alpha)
     Obviously this is soluble with [alpha := F b].  But the
     unification is only done by solveCTyFunEqs, right at the end of
     solveWanteds, and if we aren't careful we'll end up with an
     unsolved goal inside the implication.  We need to "push" the
     as-yes-unsolved (F b ~ alpha) inwards, as a *given*, so that it
     can be used to solve the inner (F b
     ~ alpha).  See Trac #4935.

  d) There are other cases where interactions between wanteds that can help
     to solve a constraint. For example

  	class C a b | a -> b

  	(C Int alpha), (forall d. C d blah => C Int a)

     If we push the (C Int alpha) inwards, as a given, it can produce
     a fundep (alpha~a) and this can float out again and be used to
     fix alpha.  (In general we can't float class constraints out just
     in case (C d blah) might help to solve (C Int a).)

The unsolved wanteds are *canonical* but they may not be *inert*,
because when made into a given they might interact with other givens.
Hence the call to solveInteract.  Example:

 Original inert set = (d :_g D a) /\ (co :_w  a ~ [beta]) 

We were not able to solve (a ~w [beta]) but we can't just assume it as
given because the resulting set is not inert. Hence we have to do a
'solveInteract' step first. 

Finally, note that we convert them to [Given] and NOT [Given/Solved].
The reason is that Given/Solved are weaker than Givens and may be discarded.
As an example consider the inference case, where we may have, the following 
original constraints: 
     [Wanted] F Int ~ Int
             (F Int ~ a => F Int ~ a)
If we convert F Int ~ Int to [Given/Solved] instead of Given, then the next 
given (F Int ~ a) is going to cause the Given/Solved to be ignored, casting 
the (F Int ~ a) insoluble. Hence we should really convert the residual 
wanteds to plain old Given. 

We need only push in unsolved equalities both in checking mode and inference mode: 

  (1) In checking mode we should not push given dictionaries in because of
example LongWayOverlapping.hs, where we might get strange overlap
errors between far-away constraints in the program.  But even in
checking mode, we must still push type family equations. Consider:

   type instance F True a b = a 
   type instance F False a b = b

   [w] F c a b ~ gamma 
   (c ~ True) => a ~ gamma 
   (c ~ False) => b ~ gamma

Since solveCTyFunEqs happens at the very end of solving, the only way to solve
the two implications is temporarily consider (F c a b ~ gamma) as Given (NB: not 
merely Given/Solved because it has to interact with the top-level instance 
environment) and push it inside the implications. Now, when we come out again at
the end, having solved the implications solveCTyFunEqs will solve this equality.

  (2) In inference mode, we recheck the final constraint in checking mode and
hence we will be able to solve inner implications from top-level quantified
constraints nonetheless.


Note [Extra TcsTv untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Furthemore, we record the inert set simplifier-generated unification
variables of the TcsTv kind (such as variables from instance that have
been applied, or unification flattens). These variables must be passed
to the implications as extra untouchable variables. Otherwise we have
the danger of double unifications. Example (from trac ticket #4494):

   (F Int ~ uf)  /\  (forall a. C a => F Int ~ beta) 

In this example, beta is touchable inside the implication. The first
solveInteract step leaves 'uf' ununified. Then we move inside the
implication where a new constraint
       uf  ~  beta  
emerges. We may spontaneously solve it to get uf := beta, so the whole
implication disappears but when we pop out again we are left with (F
Int ~ uf) which will be unified by our final solveCTyFunEqs stage and
uf will get unified *once more* to (F Int).

The solution is to record the TcsTvs (i.e. the simplifier-generated
unification variables) that are generated when solving the flats, and
make them untouchables for the nested implication. In the example
above uf would become untouchable, so beta would be forced to be
unified as beta := uf.

NB: A consequence is that every simplifier-generated TcsTv variable
    that gets floated out of an implication becomes now untouchable
    next time we go inside that implication to solve any residual
    constraints. In effect, by floating an equality out of the
    implication we are committing to have it solved in the outside.

Note [Float Equalities out of Implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
We want to float equalities out of vanilla existentials, but *not* out 
of GADT pattern matches. 


\begin{code}

solveCTyFunEqs :: Cts -> TcS (TvSubst, Cts)
-- Default equalities (F xi ~ alpha) by setting (alpha := F xi), whenever possible
-- See Note [Solving Family Equations]
-- Returns: a bunch of unsolved constraints from the original Cts and implications
--          where the newly generated equalities (alpha := F xi) have been substituted through.
solveCTyFunEqs cts
 = do { untch   <- getUntouchables 
      ; let (unsolved_can_cts, (ni_subst, cv_binds))
                = getSolvableCTyFunEqs untch cts
      ; traceTcS "defaultCTyFunEqs" (vcat [text "Trying to default family equations:"
                                          , ppr ni_subst, ppr cv_binds
                                          ])
      ; mapM_ solve_one cv_binds

      ; return (niFixTvSubst ni_subst, unsolved_can_cts) }
  where
    solve_one (cv,tv,ty) = do { setWantedTyBind tv ty
                              ; _ <- setEqBind cv (mkTcReflCo ty) $
                                       (Wanted $ panic "Met an already solved function equality!")
                              ; return () -- Don't care about flavors etc this is
                                          -- the last thing happening
                              }

------------
type FunEqBinds = (TvSubstEnv, [(CoVar, TcTyVar, TcType)])
  -- The TvSubstEnv is not idempotent, but is loop-free
  -- See Note [Non-idempotent substitution] in Unify
emptyFunEqBinds :: FunEqBinds
emptyFunEqBinds = (emptyVarEnv, [])

extendFunEqBinds :: FunEqBinds -> CoVar -> TcTyVar -> TcType -> FunEqBinds
extendFunEqBinds (tv_subst, cv_binds) cv tv ty
  = (extendVarEnv tv_subst tv ty, (cv, tv, ty):cv_binds)

------------
getSolvableCTyFunEqs :: TcsUntouchables
                     -> Cts                -- Precondition: all Wanteds or Derived!
                     -> (Cts, FunEqBinds)  -- Postcondition: returns the unsolvables
getSolvableCTyFunEqs untch cts
  = Bag.foldlBag dflt_funeq (emptyCts, emptyFunEqBinds) cts
  where
    dflt_funeq :: (Cts, FunEqBinds) -> Ct
               -> (Cts, FunEqBinds)
    dflt_funeq (cts_in, feb@(tv_subst, _))
               (CFunEqCan { cc_id = cv
                          , cc_flavor = fl
                          , cc_fun = tc
                          , cc_tyargs = xis
                          , cc_rhs = xi })
      | Just tv <- tcGetTyVar_maybe xi      -- RHS is a type variable

      , isTouchableMetaTyVar_InRange untch tv
           -- And it's a *touchable* unification variable

      , typeKind xi `isSubKind` tyVarKind tv
         -- Must do a small kind check since TcCanonical invariants 
         -- on family equations only impose compatibility, not subkinding

      , not (tv `elemVarEnv` tv_subst)
           -- Check not in extra_binds
           -- See Note [Solving Family Equations], Point 1

      , not (tv `elemVarSet` niSubstTvSet tv_subst (tyVarsOfTypes xis))
           -- Occurs check: see Note [Solving Family Equations], Point 2
      = ASSERT ( not (isGivenOrSolved fl) )
        (cts_in, extendFunEqBinds feb cv tv (mkTyConApp tc xis))

    dflt_funeq (cts_in, fun_eq_binds) ct
      = (cts_in `extendCts` ct, fun_eq_binds)
\end{code}

Note [Solving Family Equations] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
After we are done with simplification we may be left with constraints of the form:
     [Wanted] F xis ~ beta 
If 'beta' is a touchable unification variable not already bound in the TyBinds 
then we'd like to create a binding for it, effectively "defaulting" it to be 'F xis'.

When is it ok to do so? 
    1) 'beta' must not already be defaulted to something. Example: 

           [Wanted] F Int  ~ beta   <~ Will default [beta := F Int]
           [Wanted] F Char ~ beta   <~ Already defaulted, can't default again. We 
                                       have to report this as unsolved.

    2) However, we must still do an occurs check when defaulting (F xis ~ beta), to 
       set [beta := F xis] only if beta is not among the free variables of xis.

    3) Notice that 'beta' can't be bound in ty binds already because we rewrite RHS 
       of type family equations. See Inert Set invariants in TcInteract. 


*********************************************************************************
*                                                                               * 
*                          Defaulting and disamgiguation                        *
*                                                                               *
*********************************************************************************

Basic plan behind applyDefaulting rules: 
 
 Step 1:  
    Split wanteds into defaultable groups, `groups' and the rest `rest_wanted' 
    For each defaultable group, do: 
      For each possible substitution for [alpha |-> tau] where `alpha' is the 
      group's variable, do: 
        1) Make up new TcEvBinds
        2) Extend TcS with (groupVariable 
        3) given_inert <- solveOne inert (given : a ~ tau) 
        4) (final_inert,unsolved) <- solveWanted (given_inert) (group_constraints)
        5) if unsolved == empty then 
                 sneakyUnify a |-> tau 
                 write the evidence bins
                 return (final_inert ++ group_constraints,[]) 
                      -- will contain the info (alpha |-> tau)!!
                 goto next defaultable group 
           if unsolved <> empty then 
                 throw away evidence binds
                 try next substitution 
     If you've run out of substitutions for this group, too bad, you failed 
                 return (inert,group) 
                 goto next defaultable group
 
 Step 2: 
   Collect all the (canonical-cts, wanteds) gathered this way. 
   - Do a solveGiven over the canonical-cts to make sure they are inert 
------------------------------------------------------------------------------------------


\begin{code}
applyDefaultingRules :: Cts      -- All wanteds
                     -> TcS Cts  -- All wanteds again!
-- Return some *extra* givens, which express the 
-- type-class-default choice
applyDefaultingRules wanteds
  | isEmptyBag wanteds 
  = return emptyBag
  | otherwise
  = do { traceTcS "applyDefaultingRules { " $ 
                  text "wanteds =" <+> ppr wanteds
       ; untch <- getUntouchables
       ; tv_cts <- mapM (defaultTyVar untch) $
                   varSetElems (tyVarsOfCDicts wanteds)

       ; info@(_, default_tys, _) <- getDefaultInfo
       ; let groups = findDefaultableGroups info untch wanteds
       ; traceTcS "findDefaultableGroups" $ vcat [ text "groups=" <+> ppr groups
                                                 , text "untouchables=" <+> ppr  untch 
                                                 , text "info=" <+> ppr info ]
       ; deflt_cts <- mapM (disambigGroup default_tys) groups

       ; traceTcS "applyDefaultingRules }" $ 
                  vcat [ text "Tyvar defaults =" <+> ppr tv_cts
                       , text "Type defaults =" <+> ppr deflt_cts]

       ; return (unionManyBags deflt_cts `unionBags` unionManyBags tv_cts) }
\end{code}

Note [tryTcS in defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

defaultTyVar and disambigGroup create new evidence variables for
default equations, and hence update the EvVar cache. However, after
applyDefaultingRules we will try to solve these default equations
using solveInteractCts, which will consult the cache and solve those
EvVars from themselves! That's wrong.

To avoid this problem we guard defaulting under a @tryTcS@ which leaves
the original cache unmodified.

There is a second reason for @tryTcS@ in defaulting: disambGroup does
some constraint solving to determine if a default equation is
``useful'' in solving some wanted constraints, but we want to
discharge all evidence and unifications that may have happened during
this constraint solving.

Finally, @tryTcS@ importantly does not inherit the original cache from
the higher level but makes up a new cache, the reason is that disambigGroup
will call solveInteractCts so the new derived and the wanteds must not be 
in the cache!


\begin{code}
------------------
defaultTyVar :: TcsUntouchables -> TcTyVar -> TcS Cts
-- defaultTyVar is used on any un-instantiated meta type variables to
-- default the kind of OpenKind and ArgKind etc to *.  This is important to
-- ensure that instance declarations match.  For example consider
--	instance Show (a->b)
--	foo x = show (\_ -> True)
-- Then we'll get a constraint (Show (p ->q)) where p has kind ArgKind, 
-- and that won't match the typeKind (*) in the instance decl.  
-- See test tc217.
--
-- We look only at touchable type variables. No further constraints
-- are going to affect these type variables, so it's time to do it by
-- hand.  However we aren't ready to default them fully to () or
-- whatever, because the type-class defaulting rules have yet to run.

defaultTyVar untch the_tv 
  | isTouchableMetaTyVar_InRange untch the_tv
  , not (k `eqKind` default_k)
  = tryTcS $ -- Why tryTcS? See Note [tryTcS in defaulting]
    do { let loc = CtLoc DefaultOrigin (getSrcSpan the_tv) [] -- Yuk
             fl  = Wanted loc
       ; eqv <- TcSMonad.newKindConstraint the_tv default_k fl
       ; if isNewEvVar eqv then 
             return $ unitBag (CNonCanonical { cc_id = evc_the_evvar eqv
                                             , cc_flavor = fl, cc_depth = 0 })
         else return emptyBag }
  | otherwise            
  = return emptyBag	 -- The common case
  where
    k = tyVarKind the_tv
    default_k = defaultKind k


----------------
findDefaultableGroups 
    :: ( SimplContext 
       , [Type]
       , (Bool,Bool) )  -- (Overloaded strings, extended default rules)
    -> TcsUntouchables	-- Untouchable
    -> Cts	-- Unsolved
    -> [[(Ct,TcTyVar)]]
findDefaultableGroups (ctxt, default_tys, (ovl_strings, extended_defaults)) 
                      untch wanteds
  | not (performDefaulting ctxt) = []
  | null default_tys             = []
  | otherwise = filter is_defaultable_group (equivClasses cmp_tv unaries)
  where 
    unaries     :: [(Ct, TcTyVar)]  -- (C tv) constraints
    non_unaries :: [Ct]             -- and *other* constraints
    
    (unaries, non_unaries) = partitionWith find_unary (bagToList wanteds)
        -- Finds unary type-class constraints
    find_unary cc@(CDictCan { cc_tyargs = [ty] })
        | Just tv <- tcGetTyVar_maybe ty
        = Left (cc, tv)
    find_unary cc = Right cc  -- Non unary or non dictionary 

    bad_tvs :: TcTyVarSet  -- TyVars mentioned by non-unaries 
    bad_tvs = foldr (unionVarSet . tyVarsOfCt) emptyVarSet non_unaries 

    cmp_tv (_,tv1) (_,tv2) = tv1 `compare` tv2

    is_defaultable_group ds@((_,tv):_)
        = let b1 = isTyConableTyVar tv	-- Note [Avoiding spurious errors]
              b2 = not (tv `elemVarSet` bad_tvs)
              b3 = isTouchableMetaTyVar_InRange untch tv 
              b4 = defaultable_classes [cc_class cc | (cc,_) <- ds]
          in (b1 && b2 && b3 && b4)
          {- pprTrace "is_defaultable_group" (vcat [ text "isTyConable   " <+> ppr tv <+> ppr b1 
                                                   , text "is not in bad " <+> ppr tv <+> ppr b2 
                                                   , text "is touchable  " <+> ppr tv <+> ppr b3
                                                   , text "is defaultable" <+> ppr tv <+> ppr b4 ]) -}
    is_defaultable_group [] = panic "defaultable_group"

    defaultable_classes clss 
        | extended_defaults = any isInteractiveClass clss
        | otherwise         = all is_std_class clss && (any is_num_class clss)

    -- In interactive mode, or with -XExtendedDefaultRules,
    -- we default Show a to Show () to avoid graututious errors on "show []"
    isInteractiveClass cls 
        = is_num_class cls || (classKey cls `elem` [showClassKey, eqClassKey, ordClassKey])

    is_num_class cls = isNumericClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
    -- is_num_class adds IsString to the standard numeric classes, 
    -- when -foverloaded-strings is enabled

    is_std_class cls = isStandardClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
    -- Similarly is_std_class

------------------------------
disambigGroup :: [Type]           -- The default types 
              -> [(Ct, TcTyVar)]  -- All classes of the form (C a)
	      	 		  --  sharing same type variable
              -> TcS Cts

disambigGroup []  _grp
  = return emptyBag
disambigGroup (default_ty:default_tys) group
  = do { traceTcS "disambigGroup" (ppr group $$ ppr default_ty)
       ; success <- tryTcS $ -- Why tryTcS? See Note [tryTcS in defaulting]
                    do { let der_flav = mk_derived_flavor (cc_flavor the_ct) 
                       ; derived_eq <- tryTcS $
                                       -- I need a new tryTcS because we will call solveInteractCts below!
                                       do { eqv <- TcSMonad.newEqVar der_flav (mkTyVarTy the_tv) default_ty
                                          ; return [ CNonCanonical { cc_id = evc_the_evvar eqv
                                                                   , cc_flavor = der_flav, cc_depth = 0 } ] }
                       ; traceTcS "disambigGroup (solving) {" 
                                  (text "trying to solve constraints along with default equations ...") 
                       ; solveInteractCts (derived_eq ++ wanteds)
                       ; (_,unsolved) <- extractUnsolvedTcS 
                       ; traceTcS "disambigGroup (solving) }"
                                  (text "disambigGroup unsolved =" <+> ppr (keepWanted unsolved))
                       ; if isEmptyBag (keepWanted unsolved) then -- Don't care about Derived's
                             return (Just $ listToBag derived_eq) 
                         else 
                             return Nothing 
                       }
       ; case success of
           Just cts -> -- Success: record the type variable binding, and return
                    do { wrapWarnTcS $ warnDefaulting wanteds default_ty
                       ; traceTcS "disambigGroup succeeded" (ppr default_ty)
                       ; return cts }
           Nothing -> -- Failure: try with the next type
                    do { traceTcS "disambigGroup failed, will try other default types"
                                  (ppr default_ty)
                       ; disambigGroup default_tys group } }
  where
    ((the_ct,the_tv):_) = group
    wanteds             = map fst group
    mk_derived_flavor :: CtFlavor -> CtFlavor
    mk_derived_flavor (Wanted loc) = Derived loc
    mk_derived_flavor _ = panic "Asked  to disambiguate given or derived!"
\end{code}

Note [Avoiding spurious errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When doing the unification for defaulting, we check for skolem
type variables, and simply don't default them.  For example:
   f = (*)	-- Monomorphic
   g :: Num a => a -> a
   g x = f x x
Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num a) context arising from f's definition;
we try to unify a with Int (to default it), but find that it's
already been unified with the rigid variable from g's type sig



*********************************************************************************
*                                                                               * 
*                   Utility functions
*                                                                               *
*********************************************************************************

\begin{code}
newFlatWanteds :: CtOrigin -> ThetaType -> TcM [Ct]
newFlatWanteds orig theta
  = do { loc <- getCtLoc orig
       ; mapM (inst_to_wanted loc) theta }
  where inst_to_wanted loc pty 
          = do { v <- newWantedEvVar pty 
               ; return $ 
                 CNonCanonical { cc_id = v
                               , cc_flavor = Wanted loc
                               , cc_depth = 0 } }
\end{code}