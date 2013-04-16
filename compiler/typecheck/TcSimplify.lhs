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
import Type     ( classifyPredType, PredTree(..), isIPPred_maybe )
import Var
import Unique
import VarSet
import VarEnv 
import TcEvidence
import TypeRep
import Name
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
import TrieMap () -- DV: for now
import DynFlags
import Data.Maybe ( mapMaybe )
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
  = do { ev_binds_var <- newTcEvBinds
                         
       ; zonked_wanteds <- zonkWC wanteds
       ; wc_first_go <- solveWantedsWithEvBinds ev_binds_var zonked_wanteds
       ; cts <- applyTyVarDefaulting wc_first_go 
                -- See Note [Top-level Defaulting Plan]
                
       ; let wc_for_loop = wc_first_go { wc_flat = wc_flat wc_first_go `unionBags` cts }
                           
       ; traceTc "simpl_top_loop {" $ text "zonked_wc =" <+> ppr zonked_wanteds
       ; simpl_top_loop ev_binds_var wc_for_loop }
    
  where simpl_top_loop ev_binds_var wc
          | isEmptyWC wc 
          = do { traceTc "simpl_top_loop }" empty
               ; TcRnMonad.getTcEvBinds ev_binds_var }
          | otherwise
          = do { wc_residual <- solveWantedsWithEvBinds ev_binds_var wc
               ; let wc_flat_approximate = approximateWC wc_residual
               ; (dflt_eqs,_unused_bind) <- runTcS $
                                            applyDefaultingRules wc_flat_approximate
                                            -- See Note [Top-level Defaulting Plan]
               ; if isEmptyBag dflt_eqs then 
                   do { traceTc "simpl_top_loop }" empty
                      ; report_and_finish ev_binds_var wc_residual }
                 else
                   simpl_top_loop ev_binds_var $ 
                   wc_residual { wc_flat = wc_flat wc_residual `unionBags` dflt_eqs } }

        report_and_finish ev_binds_var wc_residual 
          = do { eb1 <- TcRnMonad.getTcEvBinds ev_binds_var
               ; traceTc "reportUnsolved {" empty
                   -- See Note [Deferring coercion errors to runtime]
               ; runtimeCoercionErrors <- doptM Opt_DeferTypeErrors
               ; eb2 <- reportUnsolved runtimeCoercionErrors wc_residual
               ; traceTc "reportUnsolved }" empty
               ; return (eb1 `unionBags` eb2) }
\end{code}

Note [Top-level Defaulting Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have considered two design choices for where/when to apply defaulting.   
   (i) Do it in SimplCheck mode only /whenever/ you try to solve some 
       flat constraints, maybe deep inside the context of implications.
       This used to be the case in GHC 7.4.1.
   (ii) Do it in a tight loop at simplifyTop, once all other constraint has 
        finished. This is the current story.

Option (i) had many disadvantages: 
   a) First it was deep inside the actual solver, 
   b) Second it was dependent on the context (Infer a type signature, 
      or Check a type signature, or Interactive) since we did not want 
      to always start defaulting when inferring (though there is an exception to  
      this see Note [Default while Inferring])
   c) It plainly did not work. Consider typecheck/should_compile/DfltProb2.hs:
          f :: Int -> Bool
          f x = const True (\y -> let w :: a -> a
                                      w a = const a (y+1)
                                  in w y)
      We will get an implication constraint (for beta the type of y):
               [untch=beta] forall a. 0 => Num beta
      which we really cannot default /while solving/ the implication, since beta is
      untouchable.

Instead our new defaulting story is to pull defaulting out of the solver loop and
go with option (i), implemented at SimplifyTop. Namely:
     - First have a go at solving the residual constraint of the whole program
     - Try to approximate it with a flat constraint
     - Figure out derived defaulting equations for that flat constraint
     - Go round the loop again if you did manage to get some equations

Now, that has to do with class defaulting. However there exists type variable /kind/
defaulting. Again this is done at the top-level and the plan is:
     - At the top-level, once you had a go at solving the constraint, do 
       figure out /all/ the touchable unification variables of the wanted contraints.
     - Apply defaulting to their kinds

More details in Note [DefaultTyVar].

\begin{code}

------------------
simplifyAmbiguityCheck :: Name -> WantedConstraints -> TcM (Bag EvBind)
simplifyAmbiguityCheck name wanteds
  = traceTc "simplifyAmbiguityCheck" (text "name =" <+> ppr name) >> 
    simplifyTop wanteds  -- NB: must be simplifyTop not simplifyCheck, so that we
                         --     do ambiguity resolution.  
                         -- See Note [Impedence matching] in TcBinds.
 
------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds 
  = traceTc "simplifyInteractive" empty >>
    simplifyTop wanteds 

------------------
simplifyDefault :: ThetaType	-- Wanted; has no type variables in it
                -> TcM ()	-- Succeeds iff the constraint is soluble
simplifyDefault theta
  = do { traceTc "simplifyInteractive" empty
       ; wanted <- newFlatWanteds DefaultOrigin theta
       ; _ignored_ev_binds <- simplifyCheck (mkFlatWC wanted)
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
  = do { (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
      	 	-- The constraint solving machinery 
		-- expects *TcTyVars* not TyVars.  
		-- We use *non-overlappable* (vanilla) skolems
		-- See Note [Overlap and deriving]

       ; let subst_skol = zipTopTvSubst tvs_skols $ map mkTyVarTy tvs
             skol_set   = mkVarSet tvs_skols
	     doc = ptext (sLit "deriving") <+> parens (ppr pred)

       ; wanted <- newFlatWanteds orig (substTheta skol_subst theta)

       ; traceTc "simplifyDeriv" $ 
         vcat [ pprTvBndrs tvs $$ ppr theta $$ ppr wanted, doc ]
       ; (residual_wanted, _ev_binds1)
             <- solveWanteds (mkFlatWC wanted)

       ; let (good, bad) = partitionBagWith get_good (wc_flat residual_wanted)
                         -- See Note [Exotic derived instance contexts]
             get_good :: Ct -> Either PredType Ct
             get_good ct | validDerivPred skol_set p 
                         , isWantedCt ct  = Left p 
                         -- NB: residual_wanted may contain unsolved
                         -- Derived and we stick them into the bad set
                         -- so that reportUnsolved may decide what to do with them
                         | otherwise = Right ct
                         where p = ctPred ct

       -- We never want to defer these errors because they are errors in the
       -- compiler! Hence the `False` below
       ; _ev_binds2 <- reportUnsolved False (residual_wanted { wc_flat = bad })

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
              -> (Untouchables, WantedConstraints)
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints
		      Bool,	    -- The monomorphism restriction did something
		      		    --   so the results type is not as general as
				    --   it could be
                      TcEvBinds)    -- ... binding these evidence variables
simplifyInfer _top_lvl apply_mr name_taus (untch,wanteds)
  | isEmptyWC wanteds
  = do { gbl_tvs     <- tcGetGlobalTyVars            -- Already zonked
       ; zonked_taus <- zonkTcTypes (map snd name_taus)
       ; let tvs_to_quantify = varSetElems (tyVarsOfTypes zonked_taus `minusVarSet` gbl_tvs)
       	     		       -- tvs_to_quantify can contain both kind and type vars
       	                       -- See Note [Which variables to quantify]
       ; qtvs <- zonkQuantifiedTyVars tvs_to_quantify
       ; return (qtvs, [], False, emptyTcEvBinds) }

  | otherwise
  = do { runtimeCoercionErrors <- doptM Opt_DeferTypeErrors
       ; gbl_tvs        <- tcGetGlobalTyVars
       ; zonked_tau_tvs <- zonkTyVarsAndFV (tyVarsOfTypes (map snd name_taus))
       ; zonked_wanteds <- zonkWC wanteds

       ; traceTc "simplifyInfer {"  $ vcat
             [ ptext (sLit "names =") <+> ppr (map fst name_taus)
             , ptext (sLit "taus =") <+> ppr (map snd name_taus)
             , ptext (sLit "tau_tvs (zonked) =") <+> ppr zonked_tau_tvs
             , ptext (sLit "gbl_tvs =") <+> ppr gbl_tvs
             , ptext (sLit "closed =") <+> ppr _top_lvl
             , ptext (sLit "apply_mr =") <+> ppr apply_mr
             , ptext (sLit "untch =") <+> ppr untch
             , ptext (sLit "wanted =") <+> ppr zonked_wanteds
             ]

              -- Historical note: Before step 2 we used to have a
              -- HORRIBLE HACK described in Note [Avoid unecessary
              -- constraint simplification] but, as described in Trac
              -- #4361, we have taken in out now.  That's why we start
              -- with step 2!

              -- Step 2) First try full-blown solving 

              -- NB: we must gather up all the bindings from doing
              -- this solving; hence (runTcSWithEvBinds ev_binds_var).
              -- And note that since there are nested implications,
              -- calling solveWanteds will side-effect their evidence
              -- bindings, so we can't just revert to the input
              -- constraint.
       ; ev_binds_var <- newTcEvBinds
       ; wanted_transformed <- solveWantedsWithEvBinds ev_binds_var zonked_wanteds

              -- Step 3) Fail fast if there is an insoluble constraint,
              -- unless we are deferring errors to runtime
       ; when (not runtimeCoercionErrors && insolubleWC wanted_transformed) $ 
         do { _ev_binds <- reportUnsolved False wanted_transformed; failM }

              -- Step 4) Candidates for quantification are an approximation of wanted_transformed
       ; let quant_candidates = approximateWC wanted_transformed               
              -- NB: Already the fixpoint of any unifications that may have happened                                
              -- NB: We do not do any defaulting when inferring a type, this can lead
              -- to less polymorphic types, see Note [Default while Inferring]
              -- NB: quant_candidates here are wanted or derived, we filter the wanteds later, anyway
 
              -- Step 5) Minimize the quantification candidates                             
       ; (quant_candidates_transformed, _extra_binds)   
             <- solveWanteds $ WC { wc_flat  = quant_candidates
                                  , wc_impl  = emptyBag
                                  , wc_insol = emptyBag }

              -- Step 6) Final candidates for quantification                
       ; let final_quant_candidates :: [PredType]
             final_quant_candidates = map ctPred $ bagToList $
                                      keepWanted (wc_flat quant_candidates_transformed)
             -- NB: Already the fixpoint of any unifications that may have happened
             -- But need to zonk out the flatten-skolems
       ; final_quant_candidates <- mapM zonkTcType final_quant_candidates
                  
       ; gbl_tvs        <- tcGetGlobalTyVars -- TODO: can we just use untch instead of gbl_tvs?
       ; zonked_tau_tvs <- zonkTyVarsAndFV zonked_tau_tvs
       
       ; traceTc "simplifyWithApprox" $
         vcat [ ptext (sLit "final_quant_candidates =") <+> ppr final_quant_candidates
              , ptext (sLit "gbl_tvs=") <+> ppr gbl_tvs
              , ptext (sLit "zonked_tau_tvs=") <+> ppr zonked_tau_tvs ]
         
       ; let init_tvs  = zonked_tau_tvs `minusVarSet` gbl_tvs
             poly_qtvs = growThetaTyVars final_quant_candidates init_tvs 
                         `minusVarSet` gbl_tvs
             pbound    = filter (quantifyPred poly_qtvs) final_quant_candidates
             
       ; traceTc "simplifyWithApprox" $
         vcat [ ptext (sLit "pbound =") <+> ppr pbound
              , ptext (sLit "init_qtvs =") <+> ppr init_tvs 
              , ptext (sLit "poly_qtvs =") <+> ppr poly_qtvs ]
         
	     -- Monomorphism restriction
       ; let mr_qtvs  	     = init_tvs `minusVarSet` constrained_tvs
             constrained_tvs = tyVarsOfTypes final_quant_candidates
	     mr_bites        = apply_mr && not (null pbound)

             (qtvs, bound)
                | mr_bites  = (mr_qtvs,   [])
                | otherwise = (poly_qtvs, pbound)
             

       ; if isEmptyVarSet qtvs && null bound
         then do { traceTc "} simplifyInfer/no quantification" empty                   
                 ; emitConstraints wanted_transformed
                    -- Includes insolubles (if -fdefer-type-errors)
                    -- as well as flats and implications
                 ; return ([], [], mr_bites, TcEvBinds ev_binds_var) }
         else do

       { traceTc "simplifyApprox" $ 
         ptext (sLit "bound are =") <+> ppr bound 
         
            -- Step 4, zonk quantified variables 
       ; let minimal_flat_preds = mkMinimalBySCs bound
             skol_info = InferSkol [ (name, mkSigmaTy [] minimal_flat_preds ty)
                                   | (name, ty) <- name_taus ]
                        -- Don't add the quantified variables here, because
                        -- they are also bound in ic_skols and we want them to be
                        -- tidied uniformly

       ; qtvs_to_return <- zonkQuantifiedTyVars (varSetElems qtvs)

            -- Step 7) Emit an implication
       ; minimal_bound_ev_vars <- mapM TcMType.newEvVar minimal_flat_preds
       ; lcl_env <- getLclTypeEnv
       ; gloc <- getCtLoc skol_info
       ; let implic = Implic { ic_untch    = untch 
                             , ic_env      = lcl_env
                             , ic_skols    = qtvs_to_return
                             , ic_given    = minimal_bound_ev_vars
                             , ic_wanted   = wanted_transformed 
                             , ic_insol    = False
                             , ic_binds    = ev_binds_var
                             , ic_loc      = gloc }
       ; emitImplication implic
         
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
             vcat [ ptext (sLit "implic =") <+> ppr implic
                       -- ic_skols, ic_given give rest of result
                  , ptext (sLit "qtvs =") <+> ppr qtvs_to_return
                  , ptext (sLit "spb =") <+> ppr final_quant_candidates
                  , ptext (sLit "bound =") <+> ppr bound ]

       ; return ( qtvs_to_return, minimal_bound_ev_vars
                , mr_bites,  TcEvBinds ev_binds_var) } }
    where 
\end{code}


Note [Default while Inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our current plan is that defaulting only happens at simplifyTop and
not simplifyInfer.  This may lead to some insoluble deferred constraints
Example:

instance D g => C g Int b 

constraint inferred = (forall b. 0 => C gamma alpha b) /\ Num alpha
type inferred       = gamma -> gamma 

Now, if we try to default (alpha := Int) we will be able to refine the implication to 
  (forall b. 0 => C gamma Int b) 
which can then be simplified further to 
  (forall b. 0 => D gamma)
Finally we /can/ approximate this implication with (D gamma) and infer the quantified
type:  forall g. D g => g -> g

Instead what will currently happen is that we will get a quantified type 
(forall g. g -> g) and an implication:
       forall g. 0 => (forall b. 0 => C g alpha b) /\ Num alpha

which, even if the simplifyTop defaults (alpha := Int) we will still be left with an 
unsolvable implication:
       forall g. 0 => (forall b. 0 => D g)

The concrete example would be: 
       h :: C g a s => g -> a -> ST s a
       f (x::gamma) = (\_ -> x) (runST (h x (undefined::alpha)) + 1)

But it is quite tedious to do defaulting and resolve the implication constraints and
we have not observed code breaking because of the lack of defaulting in inference so 
we don't do it for now.



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
approximateWC :: WantedConstraints -> Cts
-- Postcondition: Wanted or Derived Cts 
approximateWC wc = float_wc emptyVarSet wc
  where 
    float_wc :: TcTyVarSet -> WantedConstraints -> Cts
    float_wc skols (WC { wc_flat = flat, wc_impl = implic }) = floats1 `unionBags` floats2
      where floats1 = do_bag (float_flat skols) flat
            floats2 = do_bag (float_implic skols) implic
                                 
    float_implic :: TcTyVarSet -> Implication -> Cts
    float_implic skols imp
      = float_wc (skols `extendVarSetList` ic_skols imp) (ic_wanted imp)
            
    float_flat :: TcTyVarSet -> Ct -> Cts
    float_flat skols ct
      | tyVarsOfCt ct `disjointVarSet` skols 
      = singleCt ct
      | otherwise = emptyCts
        
    do_bag :: (a -> Bag c) -> Bag a -> Bag c
    do_bag f = foldrBag (unionBags.f) emptyBag
\end{code}

Note [Avoid unecessary constraint simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -------- NB NB NB (Jun 12) ------------- 
    This note not longer applies; see the notes with Trac #4361.
    But I'm leaving it in here so we remember the issue.)
    ----------------------------------------
When inferring the type of a let-binding, with simplifyInfer,
try to avoid unnecessarily simplifying class constraints.
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


*********************************************************************************
*                                                                                 * 
*                             RULES                                               *
*                                                                                 *
***********************************************************************************

See note [Simplifying RULE consraints] in TcRule

Note [RULE quanfification over equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Decideing which equalities to quantify over is tricky:
 * We do not want to quantify over insoluble equalities (Int ~ Bool)
    (a) because we prefer to report a LHS type error
    (b) because if such things end up in 'givens' we get a bogus
        "inaccessible code" error

 * But we do want to quantify over things like (a ~ F b), where
   F is a type function.

The difficulty is that it's hard to tell what is insoluble!
So we see whether the simplificaiotn step yielded any type errors,
and if so refrain from quantifying over *any* equalites.

\begin{code}
simplifyRule :: RuleName 
             -> WantedConstraints	-- Constraints from LHS
             -> WantedConstraints	-- Constraints from RHS
             -> TcM ([EvVar], WantedConstraints)   -- LHS evidence varaibles
-- See Note [Simplifying RULE constraints] in TcRule
simplifyRule name lhs_wanted rhs_wanted
  = do { zonked_all <- zonkWC (lhs_wanted `andWC` rhs_wanted)
       ; let doc = ptext (sLit "LHS of rule") <+> doubleQuotes (ftext name)
             
             	 -- We allow ourselves to unify environment 
		 -- variables: runTcS runs with NoUntouchables
       ; (resid_wanted, _) <- solveWanteds zonked_all

       ; zonked_lhs <- zonkWC lhs_wanted

       ; let (q_cts, non_q_cts) = partitionBag quantify_me (wc_flat zonked_lhs)
             quantify_me  -- Note [RULE quantification over equalities]
               | insolubleWC resid_wanted = quantify_insol
               | otherwise                = quantify_normal

             quantify_insol ct = not (isEqPred (ctPred ct))

             quantify_normal ct
               | EqPred t1 t2 <- classifyPredType (ctPred ct)
               = not (t1 `eqType` t2)
               | otherwise
               = True
             
       ; traceTc "simplifyRule" $
         vcat [ doc
              , text "zonked_lhs" <+> ppr zonked_lhs 
              , text "q_cts"      <+> ppr q_cts ]

       ; return ( map (ctEvId . ctEvidence) (bagToList q_cts)
                , zonked_lhs { wc_flat = non_q_cts }) }
\end{code}


*********************************************************************************
*                                                                                 * 
*                                 Main Simplifier                                 *
*                                                                                 *
***********************************************************************************

\begin{code}
simplifyCheck :: WantedConstraints	-- Wanted
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
simplifyCheck wanteds
  = do { wanteds <- zonkWC wanteds

       ; traceTc "simplifyCheck {" (vcat
             [ ptext (sLit "wanted =") <+> ppr wanteds ])

       ; (unsolved, eb1) <- solveWanteds wanteds

       ; traceTc "simplifyCheck }" $ ptext (sLit "unsolved =") <+> ppr unsolved

       ; traceTc "reportUnsolved {" empty
       -- See Note [Deferring coercion errors to runtime]
       ; runtimeCoercionErrors <- doptM Opt_DeferTypeErrors
       ; eb2 <- reportUnsolved runtimeCoercionErrors unsolved 
       ; traceTc "reportUnsolved }" empty

       ; return (eb1 `unionBags` eb2) }
\end{code}

Note [Deferring coercion errors to runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:

  module Main where

  a :: Int
  a = 'a'

  main = print "b"

Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.

Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in TcUnify, we normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into

  co :: Int ~ Char
  co = ...

  a :: Int
  a = 'a' `cast` co

The constraint solver would realize that `co` is an insoluble constraint, and
emit an error with `reportUnsolved`. But we can also replace the right-hand side
of `co` with `error "Deferred type error: Int ~ Char"`. This allows the program
to compile, and it will run fine unless we evaluate `a`. This is what
`deferErrorsToRuntime` does.

It does this by keeping track of which errors correspond to which coercion
in TcErrors (with ErrEnv). TcErrors.reportTidyWanteds does not print the errors
and does not fail if -fwarn-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.

\begin{code}

solveWanteds :: WantedConstraints -> TcM (WantedConstraints, Bag EvBind)
-- Return the evidence binds in the BagEvBinds result
solveWanteds wanted = runTcS $ solve_wanteds wanted 

solveWantedsWithEvBinds :: EvBindsVar -> WantedConstraints -> TcM WantedConstraints
-- Side-effect the EvBindsVar argument to add new bindings from solving
solveWantedsWithEvBinds ev_binds_var wanted
  = runTcSWithEvBinds ev_binds_var $ solve_wanteds wanted


solve_wanteds :: WantedConstraints -> TcS WantedConstraints 
-- NB: wc_flats may be wanted /or/ derived now
solve_wanteds wanted@(WC { wc_flat = flats, wc_impl = implics, wc_insol = insols }) 
  = do { traceTcS "solveWanteds {" (ppr wanted)

         -- Try the flat bit, including insolubles. Solving insolubles a 
         -- second time round is a bit of a waste but the code is simple 
         -- and the program is wrong anyway, and we don't run the danger 
         -- of adding Derived insolubles twice; see 
         -- TcSMonad Note [Do not add duplicate derived insolubles] 
       ; let all_flats = flats `unionBags` insols
                         
       ; impls_from_flats <- solveInteractCts $ bagToList all_flats

       -- solve_wanteds iterates when it is able to float equalities 
       -- out of one or more of the implications. 
       ; unsolved_implics <- simpl_loop 1 (implics `unionBags` impls_from_flats)

       ; is <- getTcSInerts 
       ; let insoluble_flats = getInertInsols is
             unsolved_flats  = getInertUnsolved is

       ; bb <- getTcEvBindsMap
       ; tb <- getTcSTyBindsMap

       ; traceTcS "solveWanteds }" $
                 vcat [ text "unsolved_flats   =" <+> ppr unsolved_flats
                      , text "unsolved_implics =" <+> ppr unsolved_implics
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb)
                      , text "current tybinds  =" <+> vcat (map ppr (varEnvElts tb))
                      ]

       ; let wc =  WC { wc_flat  = unsolved_flats
                      , wc_impl  = unsolved_implics
                      , wc_insol = insoluble_flats }


       ; traceTcS "solveWanteds finished with" $
                 vcat [ text "wc (unflattened) =" <+> ppr wc ]

       ; unFlattenWC wc }



simpl_loop :: Int
           -> Bag Implication
           -> TcS (Bag Implication)
simpl_loop n implics
  | n > 10 
  = traceTcS "solveWanteds: loop!" empty >> return implics
  | otherwise 
  = do { (implic_eqs, unsolved_implics) <- solveNestedImplications implics

       ; let improve_eqs = implic_eqs
             -- NB: improve_eqs used to contain defaulting equations HERE but 
             -- defaulting now happens only at simplifyTop and not deep inside 
             -- simpl_loop! See Note [Top-level Defaulting Plan]

       ; unsolved_flats <- getTcSInerts >>= (return . getInertUnsolved) 
       ; traceTcS "solveWanteds: simpl_loop end" $
             vcat [ text "improve_eqs      =" <+> ppr improve_eqs
                  , text "unsolved_flats   =" <+> ppr unsolved_flats
                  , text "unsolved_implics =" <+> ppr unsolved_implics ]


       ; if isEmptyBag improve_eqs then return unsolved_implics 
         else do { impls_from_eqs <- solveInteractCts $ bagToList improve_eqs
                 ; simpl_loop (n+1) (unsolved_implics `unionBags` 
                                                 impls_from_eqs)} }


solveNestedImplications :: Bag Implication
                        -> TcS (Cts, Bag Implication)
-- Precondition: the TcS inerts may contain unsolved flats which have 
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return (emptyBag, emptyBag)
  | otherwise 
  = do { inerts <- getTcSInerts
       ; traceTcS "solveNestedImplications starting, inerts are:" $ ppr inerts         
       ; let (pushed_givens, thinner_inerts) = splitInertsForImplications inerts
  
       ; traceTcS "solveNestedImplications starting, more info:" $ 
         vcat [ text "original inerts = " <+> ppr inerts
              , text "pushed_givens   = " <+> ppr pushed_givens
              , text "thinner_inerts  = " <+> ppr thinner_inerts ]
         
       ; (implic_eqs, unsolved_implics)
           <- doWithInert thinner_inerts $ 
              do { let tcs_untouchables 
                         = foldr (unionVarSet . tyVarsOfCt) emptyVarSet pushed_givens
                                          -- Typically pushed_givens is very small, consists
                                          -- only of unsolved equalities, so no inefficiency 
                                          -- danger.
                                                                                    
                                          
                 -- See Note [Preparing inert set for implications]
	         -- Push the unsolved wanteds inwards, but as givens
                 ; traceTcS "solveWanteds: preparing inerts for implications {" $ 
                   vcat [ppr tcs_untouchables, ppr pushed_givens]
                 ; impls_from_givens <- solveInteractCts pushed_givens
                                        
                 ; MASSERT (isEmptyBag impls_from_givens)
                       -- impls_from_givens must be empty, since we are reacting givens
                       -- with givens, and they can never generate extra implications 
                       -- from decomposition of ForAll types. (Whereas wanteds can, see
                       -- TcCanonical, canEq ForAll-ForAll case)
                   
                 ; traceTcS "solveWanteds: } now doing nested implications {" empty
                 ; flatMapBagPairM (solveImplication tcs_untouchables) implics }

       -- ... and we are back in the original TcS inerts 
       -- Notice that the original includes the _insoluble_flats so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveWanteds: done nested implications }" $
                  vcat [ text "implic_eqs ="       <+> ppr implic_eqs
                       , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return (implic_eqs, unsolved_implics) }

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
  = shadowIPs givens $    -- See Note [Shadowing of Implicit Parameters]
    nestImplicTcS ev_binds (untch, tcs_untouchables) $
    recoverTcS (return (emptyBag, emptyBag)) $
       -- Recover from nested failures.  Even the top level is
       -- just a bunch of implications, so failing at the first one is bad
    do { traceTcS "solveImplication {" (ppr imp) 

         -- Solve flat givens
       ; impls_from_givens <- solveInteractGiven loc givens 
       ; MASSERT (isEmptyBag impls_from_givens)
         
         -- Simplify the wanteds
       ; WC { wc_flat = unsolved_flats
            , wc_impl = unsolved_implics
            , wc_insol = insols } <- solve_wanteds wanteds

       ; let (res_flat_free, res_flat_bound)
                 = floatEqualities skols givens unsolved_flats

       ; let res_wanted = WC { wc_flat  = res_flat_bound
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

\end{code}


\begin{code}
floatEqualities :: [TcTyVar] -> [EvVar] -> Cts -> (Cts, Cts)
-- Post: The returned FlavoredEvVar's are only Wanted or Derived
-- and come from the input wanted ev vars or deriveds 
floatEqualities skols can_given wantders
  | hasEqualities can_given = (emptyBag, wantders)
          -- Note [Float Equalities out of Implications]
  | otherwise = partitionBag is_floatable wantders
  where skol_set = mkVarSet skols
        is_floatable :: Ct -> Bool
        is_floatable ct
          | ct_predty <- ctPred ct
          , isEqPred ct_predty
          = skol_set `disjointVarSet` tvs_under_fsks ct_predty
        is_floatable _ct = False

        tvs_under_fsks :: Type -> TyVarSet
        -- ^ NB: for type synonyms tvs_under_fsks does /not/ expand the synonym
        tvs_under_fsks (TyVarTy tv)     
          | not (isTcTyVar tv)               = unitVarSet tv
          | FlatSkol ty <- tcTyVarDetails tv = tvs_under_fsks ty
          | otherwise                        = unitVarSet tv
        tvs_under_fsks (TyConApp _ tys) = unionVarSets (map tvs_under_fsks tys)
        tvs_under_fsks (LitTy {})       = emptyVarSet
        tvs_under_fsks (FunTy arg res)  = tvs_under_fsks arg `unionVarSet` tvs_under_fsks res
        tvs_under_fsks (AppTy fun arg)  = tvs_under_fsks fun `unionVarSet` tvs_under_fsks arg
        tvs_under_fsks (ForAllTy tv ty) -- The kind of a coercion binder 
        	     	       	        -- can mention type variables!
          | isTyVar tv		      = inner_tvs `delVarSet` tv
          | otherwise  {- Coercion -} = -- ASSERT( not (tv `elemVarSet` inner_tvs) )
                                        inner_tvs `unionVarSet` tvs_under_fsks (tyVarKind tv)
          where
            inner_tvs = tvs_under_fsks ty

shadowIPs :: [EvVar] -> TcS a -> TcS a
shadowIPs gs m
  | null shadowed = m
  | otherwise     = do is <- getTcSInerts
                       doWithInert (purgeShadowed is) m
  where
  shadowed  = mapMaybe isIP gs

  isIP g    = do p <- evVarPred_maybe g
                 (x,_) <- isIPPred_maybe p
                 return x

  isShadowedCt ct = isShadowedEv (ctEvidence ct)
  isShadowedEv ev = case isIPPred_maybe (ctEvPred ev) of
                      Just (x,_) -> x `elem` shadowed
                      _          -> False

  purgeShadowed is = is { inert_cans = purgeCans (inert_cans is)
                        , inert_solved_dicts = purgeSolved (inert_solved_dicts is)
                        }

  purgeDicts    = snd . partitionCCanMap isShadowedCt
  purgeCans ics = ics { inert_dicts = purgeDicts (inert_dicts ics) }
  purgeSolved   = filterSolved (not . isShadowedEv)
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

Whenever we are solving a bunch of flat constraints, they may contain 
the following sorts of 'touchable' unification variables:
   
   (i)   Born-touchables in that scope
 
   (ii)  Simplifier-generated unification variables, such as unification 
         flatten variables

   (iii) Touchables that have been floated out from some nested 
         implications, see Note [Float Equalities out of Implications]. 

Now, once we are done with solving these flats and have to move inwards to 
the nested implications (perhaps for a second time), we must consider all the
extra variables (categories (ii) and (iii) above) as untouchables for the 
implication. Otherwise we have the danger or double unifications, as well
as the danger of not ``seing'' some unification. Example (from Trac #4494):

   (F Int ~ uf)  /\  [untch=beta](forall a. C a => F Int ~ beta) 

In this example, beta is touchable inside the implication. The 
first solveInteract step leaves 'uf' ununified. Then we move inside 
the implication where a new constraint
       uf  ~  beta  
emerges. We may spontaneously solve it to get uf := beta, so the whole
implication disappears but when we pop out again we are left with (F
Int ~ uf) which will be unified by our final solveCTyFunEqs stage and
uf will get unified *once more* to (F Int).

The solution is to record the unification variables of the flats, 
and make them untouchables for the nested implication. In the 
example above uf would become untouchable, so beta would be forced 
to be unified as beta := uf.

Note [Float Equalities out of Implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
For ordinary pattern matches (including existentials) we float 
equalities out of implications, for instance: 
     data T where 
       MkT :: Eq a => a -> T 
     f x y = case x of MkT _ -> (y::Int)
We get the implication constraint (x::T) (y::alpha): 
     forall a. [untouchable=alpha] Eq a => alpha ~ Int
We want to float out the equality into a scope where alpha is no
longer untouchable, to solve the implication!  

But we cannot float equalities out of implications whose givens may
yield or contain equalities:

      data T a where 
        T1 :: T Int
        T2 :: T Bool
        T3 :: T a 
        
      h :: T a -> a -> Int
      
      f x y = case x of 
                T1 -> y::Int
                T2 -> y::Bool
                T3 -> h x y

We generate constraint, for (x::T alpha) and (y :: beta): 
   [untouchables = beta] (alpha ~ Int => beta ~ Int)   -- From 1st branch
   [untouchables = beta] (alpha ~ Bool => beta ~ Bool) -- From 2nd branch
   (alpha ~ beta)                                      -- From 3rd branch 

If we float the equality (beta ~ Int) outside of the first implication and 
the equality (beta ~ Bool) out of the second we get an insoluble constraint.
But if we just leave them inside the implications we unify alpha := beta and
solve everything.

Principle: 
    We do not want to float equalities out which may need the given *evidence*
    to become soluble.

Consequence: classes with functional dependencies don't matter (since there is 
no evidence for a fundep equality), but equality superclasses do matter (since 
they carry evidence).

Notice that, due to Note [Extra TcSTv Untouchables], the free unification variables 
of an equality that is floated out of an implication become effectively untouchables
for the leftover implication. This is absolutely necessary. Consider the following 
example. We start with two implications and a class with a functional dependency. 

class C x y | x -> y
instance C [a] [a]
      
(I1)      [untch=beta]forall b. 0 => F Int ~ [beta]
(I2)      [untch=beta]forall b. 0 => F Int ~ [[alpha]] /\ C beta [b]

We float (F Int ~ [beta]) out of I1, and we float (F Int ~ [[alpha]]) out of I2. 
They may react to yield that (beta := [alpha]) which can then be pushed inwards 
the leftover of I2 to get (C [alpha] [a]) which, using the FunDep, will mean that
(alpha := a). In the end we will have the skolem 'b' escaping in the untouchable
beta! Concrete example is in indexed_types/should_fail/ExtraTcsUntch.hs:

class C x y | x -> y where 
 op :: x -> y -> ()

instance C [a] [a]

type family F a :: *

h :: F Int -> ()
h = undefined

data TEx where 
  TEx :: a -> TEx 


f (x::beta) = 
    let g1 :: forall b. b -> ()
        g1 _ = h [x]
        g2 z = case z of TEx y -> (h [[undefined]], op x [y])
    in (g1 '3', g2 undefined)

Note [Shadowing of Implicit Parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char


Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in
two incompatible ways:  either by using the parameter from the
signature, or by using the local definition.  Our intention is
that the local definition should "shadow" the parameter of the
signature, and we implement this as follows: when we nest implications,
we remove any implicit parameters in the outer implication, that
have the same name as givens of the inner implication.

Here is another variation of the example:

f :: (?x :: Int) => Char
f = let ?x = 'x' in ?x

This program should also be accepted: the two constraints `?x :: Int`
and `?x :: Char` never exist in the same context, so they don't get to
interact to cause failure.
\begin{code}



unFlattenWC :: WantedConstraints -> TcS WantedConstraints
unFlattenWC wc 
  = do { (subst, remaining_unsolved_flats) <- solveCTyFunEqs (wc_flat wc)
                -- See Note [Solving Family Equations]
                -- NB: remaining_flats has already had subst applied
       ; return $ 
         WC { wc_flat  = mapBag (substCt subst) remaining_unsolved_flats
            , wc_impl  = mapBag (substImplication subst) (wc_impl wc) 
            , wc_insol = mapBag (substCt subst) (wc_insol wc) }
       }
  where 
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
        solve_one (Wanted { ctev_evar = cv }, tv, ty) 
          = setWantedTyBind tv ty >> setEvBind cv (EvCoercion (mkTcReflCo ty))
        solve_one (Derived {}, tv, ty)
          = setWantedTyBind tv ty
        solve_one arg
          = pprPanic "solveCTyFunEqs: can't solve a /given/ family equation!" $ ppr arg

------------
type FunEqBinds = (TvSubstEnv, [(CtEvidence, TcTyVar, TcType)])
  -- The TvSubstEnv is not idempotent, but is loop-free
  -- See Note [Non-idempotent substitution] in Unify
emptyFunEqBinds :: FunEqBinds
emptyFunEqBinds = (emptyVarEnv, [])

extendFunEqBinds :: FunEqBinds -> CtEvidence -> TcTyVar -> TcType -> FunEqBinds
extendFunEqBinds (tv_subst, cv_binds) fl tv ty
  = (extendVarEnv tv_subst tv ty, (fl, tv, ty):cv_binds)

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
               (CFunEqCan { cc_ev = fl
                          , cc_fun = tc
                          , cc_tyargs = xis
                          , cc_rhs = xi })
      | Just tv <- tcGetTyVar_maybe xi      -- RHS is a type variable

      , isTouchableMetaTyVar_InRange untch tv
           -- And it's a *touchable* unification variable

      , typeKind xi `tcIsSubKind` tyVarKind tv
         -- Must do a small kind check since TcCanonical invariants 
         -- on family equations only impose compatibility, not subkinding

      , not (tv `elemVarEnv` tv_subst)
           -- Check not in extra_binds
           -- See Note [Solving Family Equations], Point 1

      , not (tv `elemVarSet` niSubstTvSet tv_subst (tyVarsOfTypes xis))
           -- Occurs check: see Note [Solving Family Equations], Point 2
      = ASSERT ( not (isGiven fl) )
        (cts_in, extendFunEqBinds feb fl tv (mkTyConApp tc xis))

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
\begin{code}
applyDefaultingRules :: Cts      -- Wanteds or Deriveds
                     -> TcS Cts  -- Derived equalities 
-- Return some extra derived equalities, which express the
-- type-class default choice. 
applyDefaultingRules wanteds
  | isEmptyBag wanteds 
  = return emptyBag
  | otherwise
  = do { traceTcS "applyDefaultingRules { " $ 
                  text "wanteds =" <+> ppr wanteds
                  
       ; info@(default_tys, _) <- getDefaultInfo
       ; let groups = findDefaultableGroups info wanteds
       ; traceTcS "findDefaultableGroups" $ vcat [ text "groups=" <+> ppr groups
                                                 , text "info=" <+> ppr info ]
       ; deflt_cts <- mapM (disambigGroup default_tys) groups

       ; traceTcS "applyDefaultingRules }" $ 
                  vcat [ text "Type defaults =" <+> ppr deflt_cts]

       ; return (unionManyBags deflt_cts) }
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
touchablesOfWC :: WantedConstraints -> TcTyVarSet
-- See Note [Extra Tcs Untouchables] to see why we carry a TcsUntouchables 
-- instead of just using the Untouchable range have in our hands.
touchablesOfWC = go (NoUntouchables, emptyVarSet)
  where go :: TcsUntouchables -> WantedConstraints -> TcTyVarSet
        go untch (WC { wc_flat = flats, wc_impl = impls }) 
          = filterVarSet is_touchable flat_tvs `unionVarSet`
              foldrBag (unionVarSet . (go_impl $ untch_for_impls untch)) emptyVarSet impls 
          where is_touchable = isTouchableMetaTyVar_InRange untch
                flat_tvs = tyVarsOfCts flats
                untch_for_impls (r,uset) = (r, uset `unionVarSet` flat_tvs)
        go_impl (_rng,set) implic = go (ic_untch implic,set) (ic_wanted implic)

applyTyVarDefaulting :: WantedConstraints -> TcM Cts
applyTyVarDefaulting wc = runTcS do_dflt >>= (return . fst)
  where do_dflt = do { tv_cts <- mapM defaultTyVar $ 
                                 varSetElems (touchablesOfWC wc)
                     ; return (unionManyBags tv_cts) }

defaultTyVar :: TcTyVar -> TcS Cts
-- Precondition: a touchable meta-variable
defaultTyVar the_tv
  | not (k `eqKind` default_k)
  -- Why tryTcS? See Note [tryTcS in defaulting]
  = tryTcS $
    do { let loc = CtLoc DefaultOrigin (getSrcSpan the_tv) [] -- Yuk
       ; ty_k <- instFlexiTcSHelperTcS (tyVarName the_tv) default_k
       ; md <- newDerived loc (mkTcEqPred (mkTyVarTy the_tv) ty_k)
             -- Why not directly newDerived loc (mkTcEqPred k default_k)? 
             -- See Note [DefaultTyVar]
       ; let cts
              | Just der_ev <- md = [mkNonCanonical der_ev]
              | otherwise = []
       
       ; implics_from_defaulting <- solveInteractCts cts
       ; MASSERT (isEmptyBag implics_from_defaulting)
         
       ; unsolved <- getTcSInerts >>= (return . getInertUnsolved)
       ; if isEmptyBag (keepWanted unsolved) then return (listToBag cts)
         else return emptyBag }
  | otherwise = return emptyBag	 -- The common case
  where
    k = tyVarKind the_tv
    default_k = defaultKind k
\end{code}

Note [DefaultTyVar]
~~~~~~~~~~~~~~~~~~~
defaultTyVar is used on any un-instantiated meta type variables to
default the kind of OpenKind and ArgKind etc to *.  This is important 
to ensure that instance declarations match.  For example consider

     instance Show (a->b)
     foo x = show (\_ -> True)

Then we'll get a constraint (Show (p ->q)) where p has kind ArgKind,
and that won't match the typeKind (*) in the instance decl.  See tests
tc217 and tc175.

We look only at touchable type variables. No further constraints
are going to affect these type variables, so it's time to do it by
hand.  However we aren't ready to default them fully to () or
whatever, because the type-class defaulting rules have yet to run.

An important point is that if the type variable tv has kind k and the
default is default_k we do not simply generate [D] (k ~ default_k) because:

   (1) k may be ArgKind and default_k may be * so we will fail

   (2) We need to rewrite all occurrences of the tv to be a type
       variable with the right kind and we choose to do this by rewriting 
       the type variable /itself/ by a new variable which does have the 
       right kind.

\begin{code}


----------------
findDefaultableGroups 
    :: ( [Type]
       , (Bool,Bool) )  -- (Overloaded strings, extended default rules)
    -> Cts	        -- Unsolved (wanted or derived)
    -> [[(Ct,TcTyVar)]]
findDefaultableGroups (default_tys, (ovl_strings, extended_defaults)) wanteds
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
              b4 = defaultable_classes [cc_class cc | (cc,_) <- ds]
          in (b1 && b2 && b4)
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
                    do { derived_eq <- tryTcS $ 
                       -- I need a new tryTcS because we will call solveInteractCts below!
                            do { md <- newDerived (ctev_wloc the_fl) 
                                                  (mkTcEqPred (mkTyVarTy the_tv) default_ty)
                                                  -- ctev_wloc because constraint is not Given!
                               ; case md of 
                                    Nothing   -> return []
                                    Just ctev -> return [ mkNonCanonical ctev ] }
                            
                       ; traceTcS "disambigGroup (solving) {" $
                         text "trying to solve constraints along with default equations ..."
                       ; implics_from_defaulting <- 
                                    solveInteractCts (derived_eq ++ wanteds)
                       ; MASSERT (isEmptyBag implics_from_defaulting)
                           -- I am not certain if any implications can be generated
                           -- but I am letting this fail aggressively if this ever happens.
                                     
                       ; unsolved <- getTcSInerts >>= (return . getInertUnsolved)  
                       ; traceTcS "disambigGroup (solving) }" $
                         text "disambigGroup unsolved =" <+> ppr (keepWanted unsolved)
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
    the_fl              = cc_ev the_ct
    wanteds             = map fst group
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
  where 
    inst_to_wanted loc pty 
          = do { v <- TcMType.newWantedEvVar pty 
               ; return $ 
                 CNonCanonical { cc_ev = Wanted { ctev_evar = v
                                                , ctev_wloc = loc
                                                , ctev_pred = pty }
                               , cc_depth = 0 } }
\end{code}
