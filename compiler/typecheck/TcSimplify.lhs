\begin{code}
module TcSimplify( 
       simplifyInfer, simplifySuperClass,
       simplifyDefault, simplifyDeriv, simplifyBracket,
       simplifyRule, simplifyTop, simplifyInteractive
  ) where

#include "HsVersions.h"

import HsSyn	       
import TcRnMonad
import TcErrors
import TcCanonical
import TcMType
import TcType 
import TcSMonad 
import TcInteract
import Inst
import Var
import VarSet
import Name
import NameEnv	( emptyNameEnv )
import Bag
import ListSetOps
import Util
import PrelInfo
import PrelNames
import Class		( classKey )
import BasicTypes	( RuleName )
import Data.List	( partition )
import Outputable
import FastString
\end{code}


*********************************************************************************
*                                                                               * 
*                           External interface                                  *
*                                                                               *
*********************************************************************************

\begin{code}
simplifyTop :: WantedConstraints -> TcM (Bag EvBind)
-- Simplify top-level constraints
-- Usually these will be implications, when there is
--   nothing to quanitfy we don't wrap in a degenerate implication,
--   so we do that here instead
simplifyTop wanteds 
  = simplifyCheck SimplCheck wanteds

------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds 
  = simplifyCheck SimplInteractive wanteds

------------------
simplifyDefault :: ThetaType	-- Wanted; has no type variables in it
                -> TcM ()	-- Succeeds iff the constraint is soluble
simplifyDefault theta
  = do { loc <- getCtLoc DefaultOrigin
       ; wanted <- newWantedEvVars theta
       ; let wanted_bag = listToBag [WcEvVar (WantedEvVar w loc) | w <- wanted]
       ; _ignored_ev_binds <- simplifyCheck SimplCheck wanted_bag
       ; return () }
\end{code}

simplifyBracket is used when simplifying the constraints arising from
a Template Haskell bracket [| ... |].  We want to check that there aren't
any constraints that can't be satisfied (e.g. Show Foo, where Foo has no
Show instance), but we aren't otherwise interested in the results.
Nor do we care about ambiguous dictionaries etc.  We will type check
this bracket again at its usage site.

\begin{code}
simplifyBracket :: WantedConstraints -> TcM ()
simplifyBracket wanteds
  = do	{ zonked_wanteds <- mapBagM zonkWanted wanteds 
        ; _ <- simplifyAsMuchAsPossible SimplInfer zonked_wanteds
	; return () }
\end{code}


*********************************************************************************
*                                                                                 * 
*                            Deriving
*                                                                                 *
***********************************************************************************

\begin{code}
simplifyDeriv :: CtOrigin
		-> [TyVar]	
		-> ThetaType		-- Wanted
	        -> TcM ThetaType	-- Needed
-- Given  instance (wanted) => C inst_ty 
-- Simplify 'wanted' as much as possibles
simplifyDeriv orig tvs theta 
  = do { tvs_skols <- tcInstSkolTyVars InstSkol tvs -- Skolemize 
       	 	   -- One reason is that the constraint solving machinery
		   -- expects *TcTyVars* not TyVars.  Another is that
		   -- when looking up instances we don't want overlap
		   -- of type variables

       ; let skol_subst = zipTopTvSubst tvs $ map mkTyVarTy tvs_skols
             
       ; loc    <- getCtLoc orig
       ; wanted <- newWantedEvVars (substTheta skol_subst theta)
       ; let wanted_bag = listToBag [WcEvVar (WantedEvVar w loc) | w <- wanted]

       ; traceTc "simlifyDeriv" (ppr tvs $$ ppr theta $$ ppr wanted)
       ; (unsolved, _binds) <- simplifyAsMuchAsPossible SimplInfer wanted_bag

       ; let (good, bad) = partition validDerivPred $
                           foldrBag ((:) . wantedEvVarPred) [] unsolved
		-- See Note [Exotic derived instance contexts]
             subst_skol = zipTopTvSubst tvs_skols $ map mkTyVarTy tvs 

       ; reportUnsolvedDeriv bad loc
       ; return $ substTheta subst_skol good }
\end{code}

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

\begin{code}
simplifyInfer :: Bool		    -- Apply monomorphism restriction
              -> TcTyVarSet         -- These type variables are free in the
                                    -- types to be generalised
              -> WantedConstraints
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints
                      TcEvBinds)    -- ... binding these evidence variables
simplifyInfer apply_mr tau_tvs wanted
  | isEmptyBag wanted	  -- Trivial case is quite common
  = do { zonked_tau_tvs <- zonkTcTyVarsAndFV tau_tvs
       ; gbl_tvs        <- tcGetGlobalTyVars	     -- Already zonked
       ; qtvs <- zonkQuantifiedTyVars (varSetElems (zonked_tau_tvs `minusVarSet` gbl_tvs))
       ; return (qtvs, [], emptyTcEvBinds) }

  | otherwise
  = do { zonked_wanted <- mapBagM zonkWanted wanted 
       ; traceTc "simplifyInfer {"  $ vcat
             [ ptext (sLit "apply_mr =") <+> ppr apply_mr
             , ptext (sLit "wanted =") <+> ppr zonked_wanted
             , ptext (sLit "tau_tvs =") <+> ppr tau_tvs
             ]

	     -- Make a guess at the quantified type variables
	     -- Then split the constraints on the baisis of those tyvars
	     -- to avoid unnecessarily simplifying a class constraint
	     -- See Note [Avoid unecessary constraint simplification]
       ; gbl_tvs <- tcGetGlobalTyVars
       ; zonked_tau_tvs <- zonkTcTyVarsAndFV tau_tvs
       ; let proto_qtvs = zonked_tau_tvs `minusVarSet` gbl_tvs
             (perhaps_bound, surely_free) 
                  = partitionBag (quantifyMeWC proto_qtvs) zonked_wanted
       ; emitConstraints surely_free

       	      -- Now simplify the possibly-bound constraints
       ; (simplified_perhaps_bound, tc_binds) 
              <- simplifyAsMuchAsPossible SimplInfer perhaps_bound

	      -- Sigh: must re-zonk because because simplifyAsMuchAsPossible
	      --       may have done some unification
       ; gbl_tvs <- tcGetGlobalTyVars
       ; zonked_tau_tvs <- zonkTcTyVarsAndFV tau_tvs
       ; zonked_simples <- mapBagM zonkWantedEvVar simplified_perhaps_bound
       ; let qtvs = findQuantifiedTyVars apply_mr zonked_simples zonked_tau_tvs gbl_tvs
             (bound, free) | apply_mr  = (emptyBag, zonked_simples)
                           | otherwise = partitionBag (quantifyMe qtvs) zonked_simples

       ; traceTc "end simplifyInfer }" $
              vcat [ ptext (sLit "apply_mr =") <+> ppr apply_mr
                   , text "wanted = " <+> ppr zonked_wanted
                   , text "qtvs =   " <+> ppr qtvs
                   , text "free =   " <+> ppr free
                   , text "bound =  " <+> ppr bound ]

       -- Turn the quantified meta-type variables into real type variables 
       ; emitConstraints (mapBag WcEvVar free)
       ; qtvs_to_return <- zonkQuantifiedTyVars (varSetElems qtvs) 
       ; let bound_evvars = bagToList $ mapBag wantedEvVarToVar bound
       ; return (qtvs_to_return, bound_evvars, EvBinds tc_binds) }

------------------------
simplifyAsMuchAsPossible :: SimplContext -> WantedConstraints
                         -> TcM (Bag WantedEvVar, Bag EvBind) 
-- We use this function when inferring the type of a function
-- The wanted constraints are already zonked
simplifyAsMuchAsPossible ctxt wanteds
  = do { let untch = emptyVarSet
	     	 -- We allow ourselves to unify environment 
		 -- variables; hence *no untouchables*

       ; ((unsolved_flats, unsolved_implics), ev_binds) 
           <- runTcS ctxt untch $
              simplifyApproxLoop 0 wanteds

	      -- Report any errors
       ; reportUnsolved (emptyBag, unsolved_implics)

       ; let final_wanted_evvars = mapBag deCanonicaliseWanted unsolved_flats
       ; return (final_wanted_evvars, ev_binds) }

  where 
    simplifyApproxLoop :: Int -> WantedConstraints
                       -> TcS (CanonicalCts, Bag Implication)
    simplifyApproxLoop n wanteds
     | n > 10 
     = pprPanic "simplifyApproxLoop loops!" (ppr n <+> text "iterations") 
     | otherwise 
     = do { traceTcS "simplifyApproxLoop" (vcat 
               [ ptext (sLit "Wanted = ") <+> ppr wanteds ])
          ; (unsolved_flats, unsolved_implics) <- solveWanteds emptyInert wanteds

	  ; let (extra_flats, thiner_unsolved_implics) 
                    = approximateImplications unsolved_implics
                unsolved 
                    = mkWantedConstraints unsolved_flats thiner_unsolved_implics

          ;-- If no new work was produced then we are done with simplifyApproxLoop
            if isEmptyBag extra_flats
            then do { traceTcS "simplifyApproxLoopRes" (vcat 
              		      [ ptext (sLit "Wanted = ") <+> ppr wanteds
                              , ptext (sLit "Result = ") <+> ppr unsolved_flats ])
                    ; return (unsolved_flats, unsolved_implics) }

            else -- Produced new flat work wanteds, go round the loop
                simplifyApproxLoop (n+1) (extra_flats `unionBags` unsolved)
          }     

approximateImplications :: Bag Implication -> (WantedConstraints, Bag Implication) 
-- (wc1, impls2) <- approximateImplications impls 
-- splits 'impls' into two parts
--    wc1:    a bag of constraints that do not mention any skolems 
--    impls2: a bag of *thiner* implication constraints
approximateImplications impls 
  = splitBag (do_implic emptyVarSet) impls
  where 
    ------------------
    do_wanted :: TcTyVarSet -> WantedConstraint
              -> (WantedConstraints, WantedConstraints) 
    do_wanted skols (WcImplic impl) 
        = let (fl_w, mb_impl) = do_implic skols impl 
          in (fl_w, mapBag WcImplic mb_impl)
    do_wanted skols wc@(WcEvVar wev) 
      | tyVarsOfWantedEvVar wev `disjointVarSet` skols = (unitBag wc, emptyBag) 
      | otherwise                                      = (emptyBag, unitBag wc) 
     
    ------------------
    do_implic :: TcTyVarSet -> Implication
              -> (WantedConstraints, Bag Implication)
    do_implic skols impl@(Implic { ic_skols = skols', ic_wanted = wanted })
     = (floatable_wanted, if isEmptyBag rest_wanted then emptyBag
                          else unitBag impl{ ic_wanted = rest_wanted } ) 
     where
        (floatable_wanted, rest_wanted) 
            = splitBag (do_wanted (skols `unionVarSet` skols')) wanted

    ------------------
    splitBag :: (a -> (WantedConstraints, Bag a))
             -> Bag a -> (WantedConstraints, Bag a)
    splitBag f bag = foldrBag do_one (emptyBag,emptyBag) bag
       where
         do_one x (b1,b2) 
           = (wcs `unionBags` b1, imps `unionBags` b2)
	   where
              (wcs, imps) = f x 
\end{code}

\begin{code}
findQuantifiedTyVars :: Bool		-- Apply the MR
                     -> Bag WantedEvVar	-- Simplified constraints from RHS
                     -> TyVarSet	-- Free in tau-type of definition
                     -> TyVarSet	-- Free in the envt
		     -> TyVarSet	-- Quantify over these

findQuantifiedTyVars apply_mr wanteds tau_tvs gbl_tvs
  | isEmptyBag wanteds = init_tvs
  | apply_mr           = init_tvs `minusVarSet` constrained_tvs
  | otherwise          = fixVarSet mk_next init_tvs
  where
    init_tvs    = tau_tvs `minusVarSet` gbl_tvs
    mk_next tvs = foldrBag grow_one tvs wanteds

    grow_one wev tvs = tvs `unionVarSet` (extra_tvs `minusVarSet` gbl_tvs)
       where
         extra_tvs = growPredTyVars (wantedEvVarPred wev) tvs

    constrained_tvs = tyVarsOfWantedEvVars wanteds

--------------------
quantifyMe :: TyVarSet      -- Quantifying over these
	   -> WantedEvVar
	   -> Bool	    -- True <=> quantify over this wanted
quantifyMe qtvs wev
  | isIPPred pred = True  -- Note [Inheriting implicit parameters]
  | otherwise	  = tyVarsOfPred pred `intersectsVarSet` qtvs
  where
    pred = wantedEvVarPred wev

quantifyMeWC :: TyVarSet -> WantedConstraint -> Bool
quantifyMeWC qtvs (WcImplic implic)
  = anyBag (quantifyMeWC (qtvs `minusVarSet` ic_skols implic)) (ic_wanted implic)
quantifyMeWC qtvs (WcEvVar wev)
  = quantifyMe qtvs wev
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
*                             Superclasses                                        *
*                                                                                 *
***********************************************************************************

When constructing evidence for superclasses in an instance declaration, 
  * we MUST have the "self" dictionary available, but
  * we must NOT have its superclasses derived from "self"

Moreover, we must *completely* solve the constraints right now,
not wrap them in an implication constraint to solve later.  Why?
Because when that implication constraint is solved there may
be some unrelated other solved top-level constraints that
recursively depend on the superclass we are building. Consider
   class Ord a => C a where
   instance C [Int] where ...
Then we get
   dCListInt :: C [Int]
   dCListInt = MkC $cNum ...

   $cNum :: Ord [Int] -- The superclass
   $cNum = let self = dCListInt in <solve Ord [Int]>

Now, if there is some *other* top-level constraint solved
looking like
	foo :: Ord [Int]
        foo = scsel dCInt
we must not solve the (Ord [Int]) wanted from foo!!

\begin{code}
simplifySuperClass :: EvVar	-- The "self" dictionary
		   -> WantedConstraints
		   -> TcM ()
simplifySuperClass self wanteds
  = do { wanteds <- mapBagM zonkWanted wanteds
       ; loc <- getCtLoc NoScSkol
       ; (unsolved, ev_binds) 
             <- runTcS SimplCheck emptyVarSet $
         	do { can_self <- canGivens loc [self]
         	   ; let inert = foldlBag updInertSet emptyInert can_self
	 	     -- No need for solveInteract; we know it's inert

	 	   ; solveWanteds inert wanteds }

       ; ASSERT2( isEmptyBag ev_binds, ppr ev_binds )
         reportUnsolved unsolved }
\end{code}


*********************************************************************************
*                                                                                 * 
*                             RULES                                               *
*                                                                                 *
***********************************************************************************

Note [Simplifying RULE lhs constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the LHS of transformation rules we only simplify only equalitis,
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
  = do { zonked_lhs <- mapBagM zonkWanted lhs_wanted
       ; (lhs_residual, lhs_binds) <- simplifyAsMuchAsPossible SimplRuleLhs zonked_lhs

       -- Don't quantify over equalities (judgement call here)
       ; let (eqs, dicts) = partitionBag (isEqPred . wantedEvVarPred) lhs_residual
             lhs_dicts    = map wantedEvVarToVar (bagToList dicts)  
	     	     	       	 -- Dicts and implicit parameters
       ; reportUnsolvedWantedEvVars eqs

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
       ; loc        <- getCtLoc (RuleSkol name)
       ; rhs_binds1 <- simplifyCheck SimplCheck $ unitBag $ WcImplic $ 
             Implic { ic_untch = emptyVarSet	  -- No untouchables
             	    , ic_env = emptyNameEnv
             	    , ic_skols = mkVarSet tv_bndrs
             	    , ic_scoped = panic "emitImplication"
             	    , ic_given = lhs_dicts
             	    , ic_wanted = rhs_wanted
             	    , ic_binds = rhs_binds_var
             	    , ic_loc = loc }
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
  = do { wanteds <- mapBagM zonkWanted wanteds

       ; traceTc "simplifyCheck {" (vcat
             [ ptext (sLit "wanted =") <+> ppr wanteds ])

       ; (unsolved, ev_binds) <- runTcS ctxt emptyVarSet $
                                 solveWanteds emptyInert wanteds

       ; traceTc "simplifyCheck }" $
             ptext (sLit "unsolved =") <+> ppr unsolved

       ; reportUnsolved unsolved

       ; return ev_binds }

----------------
solveWanteds :: InertSet	       -- Given 
             -> WantedConstraints      -- Wanted
             -> TcS (CanonicalCts,     -- Unsolved flats
                     Bag Implication)  -- Unsolved implications
-- solveWanteds iterates when it is able to float equalities
-- out of one or more of the implications 
solveWanteds inert wanteds
  = do { let (flat_wanteds, implic_wanteds) = splitWanteds wanteds
       ; can_flats <- canWanteds $ bagToList flat_wanteds
       ; traceTcS "solveWanteds {" $
                 vcat [ text "wanteds =" <+> ppr wanteds
                      , text "inert =" <+> ppr inert ]
       ; (unsolved_flats, unsolved_implics) 
               <- simpl_loop 1 can_flats implic_wanteds
       ; traceTcS "solveWanteds }" $
                 vcat [ text "wanteds =" <+> ppr wanteds
                      , text "unsolved_flats =" <+> ppr unsolved_flats
                      , text "unsolved_implics =" <+> ppr unsolved_implics ]
       ; return (unsolved_flats, unsolved_implics)  }
  where
    simpl_loop :: Int 
               -> CanonicalCts	-- May inlude givens (in the recursive call)
               -> Bag Implication
               -> TcS (CanonicalCts, Bag Implication)
    simpl_loop n can_ws implics
      | n>10
      = trace "solveWanteds: loop" $	-- Always bleat
        do { traceTcS "solveWanteds: loop" (ppr inert)  -- Bleat more informatively
           ; return (can_ws, implics) }

      | otherwise
      = do { inert1 <- solveInteract inert can_ws
           ; let (inert2, unsolved_flats) = extractUnsolved inert1

           ; traceTcS "solveWanteds/done flats"  $ 
                 vcat [ text "inerts =" <+> ppr inert2
                      , text "unsolved =" <+> ppr unsolved_flats ]

                   -- See Note [Preparing inert set for implications]
           ; inert_for_implics <- solveInteract inert2 (makeGivens unsolved_flats)
           ; (implic_eqs, unsolved_implics) 
                <- flatMapBagPairM (solveImplication inert_for_implics) implics

		-- Apply defaulting rules if and only if there 
		-- no floated equalities.  If there are, they may
		-- solve the remaining wanteds, so don't do defaulting.
           ; final_eqs <- if not (isEmptyBag implic_eqs)
			  then return implic_eqs
                          else applyDefaultingRules inert2 unsolved_flats
	        -- default_eqs are *givens*, so simpl_loop may 
		-- recurse with givens in the argument

           ; if isEmptyBag final_eqs then
                 return (unsolved_flats, unsolved_implics)
             else 
                 do { traceTcS ("solveWanteds iteration " ++ show n) $ vcat
                        [ text "floated_unsolved_eqs =" <+> ppr final_eqs
                        , text "unsolved_implics = " <+> ppr unsolved_implics ]
                    ; simpl_loop (n+1) 
                             	 (unsolved_flats `unionBags` final_eqs)
                             	 unsolved_implics 
           }        }

solveImplication :: InertSet     -- Given 
                    -> Implication  -- Wanted 
                    -> TcS (CanonicalCts,	-- Unsolved unification var = type
                            Bag Implication) 	-- Unsolved rest (always empty or singleton)
-- Returns: 
--  1. A bag of floatable wanted constraints, not mentioning any skolems, 
--     that are of the form unification var = type
-- 
--  2. Maybe a unsolved implication, empty if entirely solved! 
-- 
-- Precondition: everything is zonked by now
solveImplication inert 
     imp@(Implic { ic_untch  = untch 
                 , ic_binds  = ev_binds
                 , ic_skols  = skols 
                 , ic_given  = givens
                 , ic_wanted = wanteds
                 , ic_loc    = loc })
  = nestImplicTcS ev_binds untch $
    do { traceTcS "solveImplication {" (ppr imp) 

         -- Solve flat givens
       ; can_givens  <- canGivens loc givens
       ; given_inert <- solveInteract inert can_givens

         -- Simplify the wanteds
       ; (unsolved_flats, unsolved_implics) <- solveWanteds given_inert wanteds

       ; let (res_flat_free, res_flat_bound) 
                      = floatEqualities skols givens unsolved_flats
             unsolved = mkWantedConstraints res_flat_bound unsolved_implics

       ; traceTcS "solveImplication end }" $ vcat
             [ text "res_flat_free =" <+> ppr res_flat_free
             , text "res_flat_bound =" <+> ppr res_flat_bound
             , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; let res_bag | isEmptyBag unsolved = emptyBag
                     | otherwise           = unitBag (imp { ic_wanted  = unsolved })

       ; return (res_flat_free, res_bag) }

floatEqualities :: TcTyVarSet -> [EvVar]
                -> CanonicalCts -> (CanonicalCts, CanonicalCts)
floatEqualities skols can_given wanteds
  | hasEqualities can_given = (emptyBag, wanteds)
  | otherwise               = partitionBag is_floatable wanteds
  where
    is_floatable :: CanonicalCt -> Bool
    is_floatable (CTyEqCan { cc_tyvar = tv, cc_rhs = ty })
      | isMetaTyVar tv || isMetaTyVarTy ty
      = skols `disjointVarSet` (extendVarSet (tyVarsOfType ty) tv)
    is_floatable _ = False
\end{code}

Note [Preparing inert set for implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before solving the nested implications, we convert any unsolved flat wanteds
to givens, and add them to the inert set.  Reasons:
  a) In checking mode, suppresses unnecessary errors.  We already have 
     on unsolved-wanted error; adding it to the givens prevents any 
     consequential errors from showing uop
  b) More importantly, in inference mode, we are going to quantify over this
     constraint, and we *don't* want to quantify over any constraints that
     are deducible from it.

The unsolved wanteds are *canonical* but they may not be *inert*,
because when made into a given they might interact with other givens.
Hence the call to solveInteract.  Example:

 Original inert set = (d :_g D a) /\ (co :_w  a ~ [beta]) 

We were not able to solve (a ~w [beta]) but we can't just assume it as
given because the resulting set is not inert. Hence we have to do a
'solveInteract' step first

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
applyDefaultingRules :: InertSet
                     -> CanonicalCts 	-- All wanteds
                     -> TcS CanonicalCts
-- Return some *extra* givens, which express the 
-- type-class-default choice

applyDefaultingRules inert wanteds
  | isEmptyBag wanteds 
  = return emptyBag
  | otherwise
  = do { untch <- getUntouchablesTcS
       ; tv_cts <- mapM (defaultTyVar untch) $
                   varSetElems (tyVarsOfCanonicals wanteds)

       ; info@(_, default_tys, _) <- getDefaultInfo
       ; let groups = findDefaultableGroups info untch wanteds
       ; deflt_cts <- mapM (disambigGroup default_tys untch inert) groups

       ; traceTcS "deflt2" (vcat [ text "Tyvar defaults =" <+> ppr tv_cts
                                 , text "Type defaults =" <+> ppr deflt_cts])

       ; return (unionManyBags deflt_cts `andCCan` unionManyBags tv_cts) }

------------------
defaultTyVar :: TcTyVarSet -> TcTyVar -> TcS CanonicalCts
-- defaultTyVar is used on any un-instantiated meta type variables to
-- default the kind of ? and ?? etc to *.  This is important to ensure
-- that instance declarations match.  For example consider
--	instance Show (a->b)
--	foo x = show (\_ -> True)
-- Then we'll get a constraint (Show (p ->q)) where p has argTypeKind (printed ??), 
-- and that won't match the typeKind (*) in the instance decl.  
-- See test tc217.
--
-- We look only at touchable type variables. No further constraints
-- are going to affect these type variables, so it's time to do it by
-- hand.  However we aren't ready to default them fully to () or
-- whatever, because the type-class defaulting rules have yet to run.

defaultTyVar untch the_tv 
  | isMetaTyVar the_tv
  , not (the_tv `elemVarSet` untch)
  , not (k `eqKind` default_k)
  = do { (ev, better_ty) <- TcSMonad.newKindConstraint (mkTyVarTy the_tv) default_k
       ; let loc = CtLoc DefaultOrigin (getSrcSpan the_tv) [] -- Yuk
       	     	   -- 'DefaultOrigin' is strictly the declaration, but it's convenient
             wanted_eq  = CTyEqCan { cc_id     = ev
                                   , cc_flavor = Wanted loc
                                   , cc_tyvar  = the_tv
           	                   , cc_rhs    = better_ty }
       ; return (unitBag wanted_eq) }

  | otherwise            
  = return emptyCCan	 -- The common case
  where
    k = tyVarKind the_tv
    default_k = defaultKind k


----------------
findDefaultableGroups 
    :: ( SimplContext 
       , [Type]
       , (Bool,Bool) )  -- (Overloaded strings, extended default rules)
    -> TcTyVarSet	-- Untouchable
    -> CanonicalCts	-- Unsolved
    -> [[(CanonicalCt,TcTyVar)]]
findDefaultableGroups (ctxt, default_tys, (ovl_strings, extended_defaults)) 
                      untch wanteds
  | not (performDefaulting ctxt) = []
  | null default_tys             = []
  | otherwise = filter is_defaultable_group (equivClasses cmp_tv unaries)
  where 
    unaries     :: [(CanonicalCt, TcTyVar)]  -- (C tv) constraints
    non_unaries :: [CanonicalCt]             -- and *other* constraints
    
    (unaries, non_unaries) = partitionWith find_unary (bagToList wanteds)
        -- Finds unary type-class constraints
    find_unary cc@(CDictCan { cc_tyargs = [ty] })
        | Just tv <- tcGetTyVar_maybe ty
        = Left (cc, tv)
    find_unary cc = Right cc  -- Non unary or non dictionary 

    bad_tvs :: TcTyVarSet  -- TyVars mentioned by non-unaries 
    bad_tvs = foldr (unionVarSet . tyVarsOfCanonical) emptyVarSet non_unaries 

    cmp_tv (_,tv1) (_,tv2) = tv1 `compare` tv2

    is_defaultable_group ds@((_,tv):_)
        = isTyConableTyVar tv	-- Note [Avoiding spurious errors]
        && not (tv `elemVarSet` bad_tvs)
        && not (tv `elemVarSet` untch)    -- Non untouchable
        && defaultable_classes [cc_class cc | (cc,_) <- ds]
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
disambigGroup :: [Type]                    -- The default types 
	      -> TcTyVarSet		   -- Untouchables
              -> InertSet                  -- Given inert 
              -> [(CanonicalCt, TcTyVar)]  -- All classes of the form (C a)
	      	 		           --  sharing same type variable
              -> TcS CanonicalCts

disambigGroup [] _inert _untch _grp 
  = return emptyBag
disambigGroup (default_ty:default_tys) untch inert group
  = do { traceTcS "disambigGroup" (ppr group $$ ppr default_ty)
       ; ev <- newGivOrDerCoVar (mkTyVarTy the_tv) default_ty default_ty -- Refl 
		       	 -- We know this equality is canonical,
			 -- so we directly construct it as such
       ; let given_eq = CTyEqCan { cc_id     = ev
                                 , cc_flavor = mkGivenFlavor (cc_flavor the_ct) UnkSkol
                               	 , cc_tyvar  = the_tv
           	                 , cc_rhs    = default_ty }

       ; success <- tryTcS (extendVarSet untch the_tv) $ 
           	    do { given_inert <- solveOne inert given_eq
           	       ; final_inert <- solveInteract given_inert (listToBag wanteds)
           	       ; let (_, unsolved) = extractUnsolved final_inert
           	       ; return (isEmptyBag unsolved) }

       ; case success of
           True  ->   -- Success: record the type variable binding, and return
                    do { setWantedTyBind the_tv default_ty
		       ; wrapWarnTcS $ warnDefaulting wanted_ev_vars default_ty
		       ; traceTcS "disambigGoup succeeded" (ppr default_ty)
                       ; return (unitBag given_eq) }
           False ->    -- Failure: try with the next type
		    do { traceTcS "disambigGoup succeeded" (ppr default_ty)
                       ; disambigGroup default_tys untch inert group } }
  where
    ((the_ct,the_tv):_) = group
    wanteds = map fst group
    wanted_ev_vars = map deCanonicaliseWanted wanteds
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


