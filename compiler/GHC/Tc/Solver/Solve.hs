{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Solve (
     simplifyWantedsTcM,
     solveWanteds,        -- Solves WantedConstraints
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds,  -- Solves Cts
     trySolveImplication,

     setImplicationStatus
  ) where

import GHC.Prelude

import GHC.Tc.Solver.Dict
import GHC.Tc.Solver.Equality( solveEquality )
import GHC.Tc.Solver.Irred( solveIrred )
import GHC.Tc.Solver.Rewrite( rewrite, rewriteType )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.CtLoc( ctLocEnv, ctLocOrigin, setCtLocOrigin )
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( mkGivenLoc )
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad  as TcS
import qualified GHC.Tc.Utils.Monad   as TcM
import qualified GHC.Tc.Zonk.TcType   as TcM

import GHC.Core.Predicate
import GHC.Core.Reduction
import GHC.Core.Coercion
import GHC.Core.TyCo.FVs( coVarsOfCos )
import GHC.Core.Class( classHasSCs )

import GHC.Types.Id(  idType )
import GHC.Types.Var( EvVar, tyVarKind )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic ( IntWithInf, intGtLimit )
import GHC.Types.Unique.Set( nonDetStrictFoldUniqSet )

import GHC.Data.Bag
import GHC.Data.Maybe

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Driver.Session


import Control.Monad

import Data.List( deleteFirstsBy )
import qualified Data.Semigroup as S
import Data.Void( Void )

{- ********************************************************************************
*                                                                                 *
*                                 Main Simplifier                                 *
*                                                                                 *
******************************************************************************** -}

simplifyWantedsTcM :: [CtEvidence] -> TcM WantedConstraints
-- Solve the specified Wanted constraints
-- Discard the evidence binds
-- Postcondition: fully zonked
simplifyWantedsTcM wanted
  = do { TcM.traceTc "simplifyWantedsTcM {" (ppr wanted)
       ; (result, _) <- runTcS (solveWanteds (mkSimpleWC wanted))
       ; result <- TcM.liftZonkM $ TcM.zonkWC result
       ; TcM.traceTc "simplifyWantedsTcM }" (ppr result)
       ; return result }

solveWanteds :: WantedConstraints -> TcS WantedConstraints
solveWanteds wc@(WC { wc_errors = errs })
  = do { cur_lvl <- TcS.getTcLevel
       ; traceTcS "solveWanteds {" $
         vcat [ text "Level =" <+> ppr cur_lvl
              , ppr wc ]

       ; dflags <- getDynFlags
       ; solved_wc <- simplify_loop 0 (solverIterations dflags) True wc

       ; errs' <- simplifyDelayedErrors errs
       ; let final_wc = solved_wc { wc_errors = errs' }

       ; ev_binds_var <- getTcEvBindsVar
       ; bb <- TcS.getTcEvBindsMap ev_binds_var
       ; traceTcS "solveWanteds }" $
                 vcat [ text "final wc =" <+> ppr final_wc
                      , text "ev_binds_var =" <+> ppr ev_binds_var
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb) ]

       ; return final_wc }

simplify_loop :: Int -> IntWithInf -> Bool
              -> WantedConstraints -> TcS WantedConstraints
-- Do a round of solving, and call maybe_simplify_again to iterate
-- The 'definitely_redo_implications' flags is False if the only reason we
-- are iterating is that we have added some new Wanted superclasses
-- hoping for fundeps to help us; see Note [Superclass iteration]
--
-- Does not affect wc_holes at all; reason: wc_holes never affects anything
-- else, so we do them once, at the end in solveWanteds
simplify_loop n limit definitely_redo_implications
              wc@(WC { wc_simple = simples, wc_impl = implics })
  | isSolvedWC wc  -- Fast path
  = return wc
  | otherwise
  = do { csTraceTcS $
         text "simplify_loop iteration=" <> int n
         <+> (parens $ hsep [ text "definitely_redo =" <+> ppr definitely_redo_implications <> comma
                            , int (lengthBag simples) <+> text "simples to solve" ])
       ; traceTcS "simplify_loop: wc =" (ppr wc)

       ; (simple_unif_happened, wc1)
             <- reportCoarseGrainUnifications $  -- See Note [Superclass iteration]
                solveSimpleWanteds simples
                -- Any insoluble constraints are in 'simples' and so get rewritten
                -- See Note [Rewrite insolubles] in GHC.Tc.Solver.InertSet

       -- Next, solve implications from wc_impl
       ; (impl_unif_happened, implics')
             <- if not (definitely_redo_implications   -- See Note [Superclass iteration]
                        || simple_unif_happened)       -- for this conditional
                then return (False, implics)
                else reportCoarseGrainUnifications $
                     solveNestedImplications implics

       ; let wc' = wc1 { wc_impl = wc_impl wc1 `unionBags` implics' }

       ; csTraceTcS $ text "unif_happened" <+> ppr impl_unif_happened

         -- We iterate the loop only if the /implications/ did some relevant
         -- unification.  Even if the /simples/ did unifications we don't need
         -- to re-do them.
       ; maybe_simplify_again (n+1) limit impl_unif_happened wc' }

data NextAction
  = NA_Stop                 -- Just return the WantedConstraints
  | NA_TryAgain             -- Try again with the given wc
        WantedConstraints   --    with these WantedConstraints
        Bool                -- See `definitely_redo_implications` in the comment
                            --    for `simplify_loop`

maybe_simplify_again :: Int -> IntWithInf -> Bool
                     -> WantedConstraints -> TcS WantedConstraints
maybe_simplify_again n limit unif_happened wc@(WC { wc_simple = simples })
  = do { -- Look for reasons to stop or continue
         --   Nothing     => I don't have an opinion
         --   Just action => Do this action
         result <- firstJustsM [ check_limit
                               , check_unif_happened
                               , try_expanding_superclasses ]
       ; case result of
           Nothing      -> return wc
           Just NA_Stop -> return wc
           Just (NA_TryAgain updated_wc redo_implics)
                       -> simplify_loop n limit redo_implics updated_wc }
  where
    check_limit :: TcS (Maybe NextAction)
    check_limit
      | n `intGtLimit` limit
      = do { -- Add an error (not a warning) if we blow the limit,
             -- Typically if we blow the limit we are going to report some other error
             -- (an unsolved constraint), and we don't want that error to suppress
             -- the iteration limit warning!
             addErrTcS $ TcRnSimplifierTooManyIterations limit wc
           ; return (Just NA_Stop) }

      | otherwise
      = return Nothing

    check_unif_happened :: TcS (Maybe NextAction)
    check_unif_happened
      | unif_happened = return (Just (NA_TryAgain wc True))
      | otherwise     = return Nothing

    try_expanding_superclasses :: TcS (Maybe NextAction)
    try_expanding_superclasses
      | superClassesMightHelp wc    -- Returns False quickly if wc is solved
      = -- We still have unsolved goals, and apparently no way to solve them,
        -- so try expanding superclasses at this level, both Given and Wanted
        do { pending_given <- getPendingGivenScs
           ; let (pending_wanted, simples1) = getPendingWantedScs simples
           ; if null pending_given && null pending_wanted
               then return Nothing  -- After all, superclasses did not help
               else
        do { new_given  <- makeSuperClasses pending_given
           ; new_wanted <- makeSuperClasses pending_wanted
           ; solveSimpleGivens new_given -- Add the new Givens to the inert set
           ; traceTcS "try_expanding_superclasses"
               (vcat [ text "pending_given" <+> ppr pending_given
                     , text "new_given" <+> ppr new_given
                     , text "pending_wanted" <+> ppr pending_wanted
                     , text "new_wanted" <+> ppr new_wanted ])
           ; let updated_wc =
                    wc { wc_simple = simples1 `unionBags` listToBag new_wanted }
           ; return (Just (NA_TryAgain updated_wc (not (null pending_given)))) }}
             -- (not (null pending_given)): see Note [Superclass iteration]

      | otherwise
      = return Nothing

{- Note [Superclass iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this implication constraint
    forall a.
       [W] d: C Int beta
       forall b. blah
where
  class D a b | a -> b
  class D a b => C a b
We will expand d's superclasses, giving [W] D Int beta, in the hope of geting
fundeps to unify beta.  Doing so is usually fruitless (no useful fundeps),
and if so it seems a pity to waste time iterating the implications (forall b. blah)
(If we add new Given superclasses it's a different matter: it's really worth looking
at the implications.)

Hence the definitely_redo_implications flag to simplify_loop.  It's usually
True, but False in the case where the only reason to iterate is new Wanted
superclasses.  In that case we check whether the new Wanteds actually led to
any new unifications, and iterate the implications only if so.

Note [When to iterate the solver: unifications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a deep tree of implication constraints
   forall[1] a.                              -- Outer-implic
      C alpha[1]                               -- Simple
      forall[2] c. ....(C alpha[1])....        -- Implic-1
      forall[2] b. ....(alpha[1] ~ Int)....    -- Implic-2

The (C alpha) is insoluble until we know alpha.  We solve alpha
by unifying alpha:=Int somewhere deep inside Implic-2. But then we
must try to solve the Outer-implic all over again. This time we can
solve (C alpha) both in Outer-implic, and nested inside Implic-1.

When should we iterate solving a level-n implication?
Answer: if any unification of a tyvar at level n takes place
        in the ic_implics of that implication.

* What if a unification takes place at level n-1? Then don't iterate
  level n, because we'll iterate level n-1, and that will in turn iterate
  level n.

* What if a unification takes place at level n, in the ic_simples of
  level n?  No need to track this, because the kick-out mechanism deals
  with it.  (We can't drop kick-out in favour of iteration, because kick-out
  works for skolem-equalities, not just unifications.)

So the monad-global `WhatUnifications` flag, kept in `tcs_what` keeps
track of whether any unifications at all have taken place, and if so, what
is the outermost level that has seen a unification. Seee GHC.Tc.Utils.Unify
Note [WhatUnifications].

The iteration is done in the simplify_loop/maybe_simplify_again loop.

It is helpful not to iterate unless there is a chance of progress.  #8474 is
an example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [W] alpha[1] ~ Int,
    so we can unify.

  * It's better not to iterate the inner implications, but go all the
    way out to level 1 before iterating -- because iterating level 1
    will iterate the inner levels anyway.

(In the olden days when we "floated" these Derived constraints, this was
much, much more important -- we got exponential behaviour, as each iteration
produced the same Derived constraint.)

Note [Expanding Recursive Superclasses and ExpansionFuel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the class declaration (T21909)

    class C [a] => C a where
       foo :: a -> Int

and suppose during type inference we obtain an implication constraint:

    forall a. C a => C [[a]]

To solve this implication constraint, we first expand one layer of the superclass
of Given constraints, but not for Wanted constraints.
(See Note [Eagerly expand given superclasses] and Note [Why adding superclasses can help]
in GHC.Tc.Solver.Dict.) We thus get:

    [G] g1 :: C a
    [G] g2 :: C [a]    -- new superclass layer from g1
    [W] w1 :: C [[a]]

Now, we cannot solve `w1` directly from `g1` or `g2` as we may not have
any instances for C. So we expand a layer of superclasses of each Wanteds and Givens
that we haven't expanded yet.
This is done in `maybe_simplify_again`. And we get:

    [G] g1 :: C a
    [G] g2 :: C [a]
    [G] g3 :: C [[a]]    -- new superclass layer from g2, can solve w1
    [W] w1 :: C [[a]]
    [W] w2 :: C [[[a]]]  -- new superclass layer from w1, not solvable

Now, although we can solve `w1` using `g3` (obtained from expanding `g2`),
we have a new wanted constraint `w2` (obtained from expanding `w1`) that cannot be solved.
We thus make another go at solving in `maybe_simplify_again` by expanding more
layers of superclasses. This looping is futile as Givens will never be able to catch up with Wanteds.

Side Note: In principle we don't actually need to /solve/ `w2`, as it is a superclass of `w1`
but we only expand it to expose any functional dependencies (see Note [The superclass story])
But `w2` is a wanted constraint, so we will try to solve it like any other,
even though ultimately we will discard its evidence.

Solution: Simply bound the maximum number of layers of expansion for
Givens and Wanteds, with ExpansionFuel.  Give the Givens more fuel
(say 3 layers) than the Wanteds (say 1 layer). Now the Givens will
win.  The Wanteds don't need much fuel: we are only expanding at all
to expose functional dependencies, and wantedFuel=1 means we will
expand a full recursive layer.  If the superclass hierarchy is
non-recursive (the normal case) one layer is therefore full expansion.

The default value for wantedFuel = Constants.max_WANTEDS_FUEL = 1.
The default value for givenFuel  = Constants.max_GIVENS_FUEL = 3.
Both are configurable via the `-fgivens-fuel` and `-fwanteds-fuel`
compiler flags.

There are two preconditions for the default fuel values:
   (1) default givenFuel >= default wantedsFuel
   (2) default givenFuel < solverIterations

Precondition (1) ensures that we expand givens at least as many times as we expand wanted constraints
preferably givenFuel > wantedsFuel to avoid issues like T21909 while
the precondition (2) ensures that we do not reach the solver iteration limit and fail with a
more meaningful error message (see T19627)

This also applies for quantified constraints; see `-fqcs-fuel` compiler flag and `QCI.qci_pend_sc` field.
-}

{- ********************************************************************************
*                                                                                 *
*                    Solving implication constraints                              *
*                                                                                 *
******************************************************************************** -}

solveNestedImplications :: Bag Implication
                        -> TcS (Bag Implication)
-- Precondition: the TcS inerts may contain unsolved simples which have
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return emptyBag
  | otherwise
  = do { traceTcS "solveNestedImplications starting {" empty
       ; unsolved_implics <- mapBagM solveImplication implics

       -- ... and we are back in the original TcS inerts
       -- Notice that the original includes the _insoluble_simples so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveNestedImplications end }" $
                  vcat [ text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return unsolved_implics }

{- Note [trySolveImplication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`trySolveImplication` may be invoked while solving simple wanteds, notably from
`solveWantedForAll`.  It returns a Bool to say if solving succeeded or failed.

It uses `nestImplicTcS` to build a nested scope.  One subtle point is that
`nestImplicTcS` uses the `inert_givens` (not the `inert_cans`) of the current
inert set to initialse the `InertSet` of the nested scope.  It is super-important not
to pollute the sub-solving problem with the unsolved Wanteds of the current scope.

Whenever we do `solveSimpleGivens`, we snapshot the `inert_cans` into `inert_givens`.
(At that moment there should be no Wanteds.)
-}

trySolveImplication :: Implication -> TcS Bool
-- See Note [trySolveImplication]
trySolveImplication (Implic { ic_tclvl  = tclvl
                            , ic_binds  = ev_binds_var
                            , ic_given  = given_ids
                            , ic_wanted = wanteds
                            , ic_env    = ct_loc_env
                            , ic_info   = info })
  = nestImplicTcS ev_binds_var tclvl $
    do { let loc    = mkGivenLoc tclvl info ct_loc_env
             givens = mkGivens loc given_ids
       ; solveSimpleGivens givens
       ; residual_wanted <- solveWanteds wanteds
       ; return (isSolvedWC residual_wanted) }

solveImplication :: Implication     -- Wanted
                 -> TcS Implication -- Simplified implication
-- Precondition: The TcS monad contains an empty worklist and given-only inerts
-- which after trying to solve this implication we must restore to their original value
solveImplication imp@(Implic { ic_tclvl  = tclvl
                             , ic_binds  = ev_binds_var
                             , ic_given  = given_ids
                             , ic_wanted = wanteds
                             , ic_info   = info
                             , ic_env    = ct_loc_env
                             , ic_status = status })
  | isSolvedStatus status
  = return imp  -- Do nothing

  | otherwise  -- Even for IC_Insoluble it is worth doing more work
               -- The insoluble stuff might be in one sub-implication
               -- and other unsolved goals in another; and we want to
               -- solve the latter as much as possible
  = do { inerts <- getInertSet
       ; traceTcS "solveImplication {" (ppr imp $$ text "Inerts" <+> ppr inerts)

       -- commented out; see `where` clause below
       -- ; when debugIsOn check_tc_level

         -- Solve the nested constraints
       ; (has_given_eqs, given_insols, residual_wanted)
            <- nestImplicTcS ev_binds_var tclvl $
               do { let loc    = mkGivenLoc tclvl info ct_loc_env
                        givens = mkGivens loc given_ids
                  ; solveSimpleGivens givens

                  ; residual_wanted <- solveWanteds wanteds

                  ; (has_eqs, given_insols) <- getHasGivenEqs tclvl
                        -- Call getHasGivenEqs /after/ solveWanteds, because
                        -- solveWanteds can augment the givens, via expandSuperClasses,
                        -- to reveal given superclass equalities

                  ; return (has_eqs, given_insols, residual_wanted) }

       ; traceTcS "solveImplication 2"
           (ppr given_insols $$ ppr residual_wanted)

       ; evbinds <- TcS.getTcEvBindsMap ev_binds_var
       ; traceTcS "solveImplication 3" $ vcat
             [ text "ev_binds_var" <+> ppr ev_binds_var
             , text "implication evbinds =" <+> ppr (evBindMapBinds evbinds) ]

       ; let final_wanted = residual_wanted `addInsols` given_insols
             -- Don't lose track of the insoluble givens,
             -- which signal unreachable code; put them in ic_wanted

       ; res_implic <- setImplicationStatus (imp { ic_given_eqs = has_given_eqs
                                                 , ic_wanted = final_wanted })

       ; evbinds <- TcS.getTcEvBindsMap ev_binds_var
       ; tcvs    <- TcS.getTcEvTyCoVars ev_binds_var
       ; traceTcS "solveImplication end }" $ vcat
             [ text "has_given_eqs =" <+> ppr has_given_eqs
             , text "res_implic =" <+> ppr res_implic
             , text "implication evbinds =" <+> ppr (evBindMapBinds evbinds)
             , text "implication tvcs =" <+> ppr tcvs ]

       ; return res_implic }

    -- TcLevels must be strictly increasing (see (ImplicInv) in
    -- Note [TcLevel invariants] in GHC.Tc.Utils.TcType),
    -- and in fact I think they should always increase one level at a time.

    -- Though sensible, this check causes lots of testsuite failures. It is
    -- remaining commented out for now.
    {-
    check_tc_level = do { cur_lvl <- TcS.getTcLevel
                        ; massertPpr (tclvl == pushTcLevel cur_lvl)
                                     (text "Cur lvl =" <+> ppr cur_lvl $$ text "Imp lvl =" <+> ppr tclvl) }
    -}

----------------------
setImplicationStatus :: Implication -> TcS Implication
-- Finalise the implication returned from solveImplication,
--   * Set the ic_status field
--   * Prune unnecessary evidence bindings
--   * Prune unnecessary child implications
-- Precondition: the ic_status field is not already IC_Solved
setImplicationStatus implic@(Implic { ic_status = old_status
                                    , ic_info   = info
                                    , ic_wanted = wc })
 = assertPpr (not (isSolvedStatus old_status)) (ppr info) $
   -- Precondition: we only set the status if it is not already solved
   do { traceTcS "setImplicationStatus {" (ppr implic)

      ; let solved = isSolvedWC wc
      ; new_implic    <- neededEvVars implic
      ; bad_telescope <- if solved then checkBadTelescope implic
                                   else return False

      ; let new_status | insolubleWC wc = IC_Insoluble
                       | not solved     = IC_Unsolved
                       | bad_telescope  = IC_BadTelescope
                       | otherwise      = IC_Solved { ics_dead = dead_givens }
            dead_givens = findRedundantGivens new_implic
            new_wc      = pruneImplications wc

            final_implic = new_implic { ic_status = new_status
                                      , ic_wanted = new_wc }

      ; traceTcS "setImplicationStatus }" (ppr final_implic)
      ; return final_implic }

pruneImplications :: WantedConstraints -> WantedConstraints
-- We have now recorded the `ic_need` variables of the child
-- implications (in `ic_need_implics` of the parent) so we can
-- delete any unnecessary children.
pruneImplications wc@(WC { wc_impl = implics })
  = wc { wc_impl = filterBag keep_me implics }
         -- Do not prune holes; these should be reported
  where
    keep_me :: Implication -> Bool
    keep_me (Implic { ic_status = status, ic_wanted = wanted })
      | IC_Solved { ics_dead = dead_givens } <- status -- Fully solved
      , null dead_givens                               -- No redundant givens to report
      , isEmptyBag (wc_impl wanted)                    -- No children that might have things to report
      = False
      | otherwise
      = True        -- Otherwise, keep it

findRedundantGivens :: Implication -> [EvVar]
findRedundantGivens (Implic { ic_info = info, ic_need = need, ic_given = givens })
  | not (warnRedundantGivens info)   -- Don't report redundant constraints at all
  = []                    -- See (TRC4) of Note [Tracking redundant constraints]

  | not (null unused_givens)         -- Some givens are literally unused
  = unused_givens

  -- Only try this if unused_givens is empty: see (TRC2a)
  | otherwise                       -- All givens are used, but some might
  = redundant_givens                -- still be redundant e.g. (Eq a, Ord a)

  where
    in_instance_decl = case info of { InstSkol {} -> True; _ -> False }
                       -- See Note [Redundant constraints in instance decls]

    unused_givens = filterOut is_used givens

    needed_givens_ignoring_default_methods = ens_fvs need
    is_used given =  is_type_error given
                  || given `elemVarSet` needed_givens_ignoring_default_methods
                  || (in_instance_decl && is_improving (idType given))

    minimal_givens = mkMinimalBySCs evVarPred givens  -- See (TRC2)

    is_minimal = (`elemVarSet` mkVarSet minimal_givens)
    redundant_givens
      | in_instance_decl = []
      | otherwise        = filterOut is_minimal givens

    -- See #15232
    is_type_error id = containsUserTypeError False (idType id)
      -- False <=> do not look under ty-fam apps, AppTy etc.
      -- See (UTE1) in Note [Custom type errors in constraints].

    is_improving pred -- (transSuperClasses p) does not include p
      = any isImprovementPred (pred : transSuperClasses pred)

warnRedundantGivens :: SkolemInfoAnon -> Bool
warnRedundantGivens (SigSkol ctxt _ _)
  = case ctxt of
       FunSigCtxt _ rrc -> reportRedundantConstraints rrc
       ExprSigCtxt rrc  -> reportRedundantConstraints rrc
       _                -> False

warnRedundantGivens (InstSkol from _)
 -- Do not report redundant constraints for quantified constraints
 -- See (TRC4) in Note [Tracking redundant constraints]
 -- Fortunately it is easy to spot implications constraints that arise
 -- from quantified constraints, from their SkolInfo
 = case from of
      IsQC {}      -> False
      IsClsInst {} -> True

  -- To think about: do we want to report redundant givens for
  -- pattern synonyms, PatSynSigSkol? c.f #9953, comment:21.
warnRedundantGivens _ = False

{- Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instance declarations are special in two ways:

* We don't report unused givens if they can give rise to improvement.
  Example (#10100):
    class Add a b ab | a b -> ab, a ab -> b
    instance Add Zero b b
    instance Add a b ab => Add (Succ a) b (Succ ab)
  The context (Add a b ab) for the instance is clearly unused in terms
  of evidence, since the dictionary has no fields.  But it is still
  needed!  With the context, a wanted constraint
     Add (Succ Zero) beta (Succ Zero)
  we will reduce to (Add Zero beta Zero), and thence we get beta := Zero.
  But without the context we won't find beta := Zero.

  This only matters in instance declarations.

* We don't report givens that are a superclass of another given. E.g.
       class Ord r => UserOfRegs r a where ...
       instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where
  The (Ord r) is not redundant, even though it is a superclass of
  (UserOfRegs r CmmReg).  See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance.

  Again this is specific to instance declarations.
-}


checkBadTelescope :: Implication -> TcS Bool
-- True <=> the skolems form a bad telescope
-- See Note [Checking telescopes] in GHC.Tc.Types.Constraint
checkBadTelescope (Implic { ic_info  = info
                          , ic_skols = skols })
  | checkTelescopeSkol info
  = do{ skols <- mapM TcS.zonkTyCoVarKind skols
      ; return (go emptyVarSet (reverse skols))}

  | otherwise
  = return False

  where
    go :: TyVarSet   -- skolems that appear *later* than the current ones
       -> [TcTyVar]  -- ordered skolems, in reverse order
       -> Bool       -- True <=> there is an out-of-order skolem
    go _ [] = False
    go later_skols (one_skol : earlier_skols)
      | tyCoVarsOfType (tyVarKind one_skol) `intersectsVarSet` later_skols
      = True
      | otherwise
      = go (later_skols `extendVarSet` one_skol) earlier_skols

neededEvVars :: Implication -> TcS Implication
-- Find all the evidence variables that are "needed",
-- /and/ delete dead evidence bindings
--
--   See Note [Tracking redundant constraints]
--   See Note [Delete dead Given evidence bindings]
--
--   - Start from initial_seeds (from nested implications)
--
--   - Add free vars of RHS of all Wanted evidence bindings
--     and coercion variables accumulated in tcvs (all Wanted)
--
--   - Generate 'needed', the needed set of EvVars, by doing transitive
--     closure through Given bindings
--     e.g.   Needed {a,b}
--            Given  a = sc_sel a2
--            Then a2 is needed too
--
--   - Prune out all Given bindings that are not needed

neededEvVars implic@(Implic { ic_info        = info
                            , ic_binds       = ev_binds_var
                            , ic_wanted      = WC { wc_impl = implics }
                            , ic_need_implic = old_need_implic    -- See (TRC1)
                    })
 = do { ev_binds <- TcS.getTcEvBindsMap ev_binds_var
      ; used_cos <- TcS.getTcEvTyCoVars ev_binds_var

      ; let -- Find the variables needed by `implics`
            new_need_implic@(ENS { ens_dms = dm_seeds, ens_fvs = other_seeds })
                = foldr add_implic old_need_implic implics
                  -- Start from old_need_implic!  See (TRC1)

            -- Get the variables needed by the solved bindings
            -- (It's OK to use a non-deterministic fold here
            --  because add_wanted is commutative.)
            used_covars = coVarsOfCos used_cos
            seeds_w = nonDetStrictFoldEvBindMap add_wanted used_covars ev_binds

            need_ignoring_dms = findNeededGivenEvVars ev_binds (other_seeds `unionVarSet` seeds_w)
            need_from_dms     = findNeededGivenEvVars ev_binds dm_seeds
            need_full         = need_ignoring_dms `unionVarSet` need_from_dms

            -- `need`: the Givens from outer scopes that are used in this implication
            -- is_dm_skol: see (TRC5)
            need | is_dm_skol info = ENS { ens_dms = trim ev_binds need_full
                                         , ens_fvs = emptyVarSet }
                 | otherwise       = ENS { ens_dms = trim ev_binds need_from_dms
                                         , ens_fvs = trim ev_binds need_ignoring_dms }

      -- Delete dead Given evidence bindings
      -- See Note [Delete dead Given evidence bindings]
      ; let live_ev_binds = filterEvBindMap (needed_ev_bind need_full) ev_binds
      ; TcS.setTcEvBindsMap ev_binds_var live_ev_binds

      ; traceTcS "neededEvVars" $
        vcat [ text "old_need_implic:" <+> ppr old_need_implic
             , text "new_need_implic:" <+> ppr new_need_implic
             , text "used_covars:" <+> ppr used_covars
             , text "need_ignoring_dms:" <+> ppr need_ignoring_dms
             , text "need_from_dms:"     <+> ppr need_from_dms
             , text "need:" <+> ppr need
             , text "ev_binds:" <+> ppr ev_binds
             , text "live_ev_binds:" <+> ppr live_ev_binds ]
      ; return (implic { ic_need        = need
                       , ic_need_implic = new_need_implic }) }
 where
    trim :: EvBindMap -> VarSet -> VarSet
    -- Delete variables bound by Givens or bindings
    trim ev_binds needs = needs `varSetMinusEvBindMap` ev_binds

    add_implic :: Implication -> EvNeedSet -> EvNeedSet
    add_implic (Implic { ic_given = givens, ic_need = need }) acc
       = (need `delGivensFromEvNeedSet` givens) `unionEvNeedSet` acc

    needed_ev_bind needed (EvBind { eb_lhs = ev_var, eb_info = info })
      | EvBindGiven{} <- info = ev_var `elemVarSet` needed
      | otherwise             = True   -- Keep all wanted bindings

    add_wanted :: EvBind -> VarSet -> VarSet
    add_wanted (EvBind { eb_info = info, eb_rhs = rhs }) needs
      | EvBindGiven{} <- info = needs  -- Add the rhs vars of the Wanted bindings only
      | otherwise = nestedEvIdsOfTerm rhs `unionVarSet` needs

    is_dm_skol :: SkolemInfoAnon -> Bool
    is_dm_skol (MethSkol _ is_dm) = is_dm
    is_dm_skol _                  = False

findNeededGivenEvVars :: EvBindMap -> VarSet -> VarSet
-- Find all the Given evidence needed by seeds,
-- looking transitively through bindings for Givens (only)
findNeededGivenEvVars ev_binds seeds
  = transCloVarSet also_needs seeds
  where
   also_needs :: VarSet -> VarSet
   also_needs needs = nonDetStrictFoldUniqSet add emptyVarSet needs
     -- It's OK to use a non-deterministic fold here because we immediately
     -- forget about the ordering by creating a set

   add :: Var -> VarSet -> VarSet
   add v needs
     | Just ev_bind <- lookupEvBind ev_binds v
     , EvBind { eb_info = EvBindGiven, eb_rhs = rhs } <- ev_bind
       -- Look at Given bindings only
     = nestedEvIdsOfTerm rhs `unionVarSet` needs
     | otherwise
     = needs

-------------------------------------------------
simplifyDelayedErrors :: Bag DelayedError -> TcS (Bag DelayedError)
-- Simplify any delayed errors: e.g. type and term holes
-- NB: At this point we have finished with all the simple
--     constraints; they are in wc_simple, not in the inert set.
--     So those Wanteds will not rewrite these delayed errors.
--     That's probably no bad thing.
--
--     However if we have [W] alpha ~ Maybe a, [W] alpha ~ Int
--     and _ : alpha, then we'll /unify/ alpha with the first of
--     the Wanteds we get, and thereby report (_ : Maybe a) or
--     (_ : Int) unpredictably, depending on which we happen to see
--     first.  Doesn't matter much; there is a type error anyhow.
--     T17139 is a case in point.
simplifyDelayedErrors = mapMaybeBagM simpl_err
  where
    simpl_err :: DelayedError -> TcS (Maybe DelayedError)
    simpl_err (DE_Hole hole) = Just . DE_Hole <$> simpl_hole hole
    simpl_err err@(DE_NotConcrete {}) = return $ Just err
    simpl_err (DE_Multiplicity mult_co loc)
      = do { mult_co' <- TcS.zonkCo mult_co
           ; if isReflexiveCo mult_co' then
               return Nothing
             else
               return $ Just (DE_Multiplicity mult_co' loc) }

    simpl_hole :: Hole -> TcS Hole

     -- See Note [Do not simplify ConstraintHoles]
    simpl_hole h@(Hole { hole_sort = ConstraintHole }) = return h

     -- other wildcards should be simplified for printing
     -- we must do so here, and not in the error-message generation
     -- code, because we have all the givens already set up
    simpl_hole h@(Hole { hole_ty = ty, hole_loc = loc })
      = do { ty' <- rewriteType loc ty
           ; traceTcS "simpl_hole" (ppr ty $$ ppr ty')
           ; return (h { hole_ty = ty' }) }

{- Note [Delete dead Given evidence bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a result of superclass expansion, we speculatively
generate evidence bindings for Givens. E.g.
   f :: (a ~ b) => a -> b -> Bool
   f x y = ...
We'll have
   [G] d1 :: (a~b)
and we'll speculatively generate the evidence binding
   [G] d2 :: (a ~# b) = sc_sel d

Now d2 is available for solving.  But it may not be needed!  Usually
such dead superclass selections will eventually be dropped as dead
code, but:

 * It won't always be dropped (#13032).  In the case of an
   unlifted-equality superclass like d2 above, we generate
       case heq_sc d1 of d2 -> ...
   and we can't (in general) drop that case expression in case
   d1 is bottom.  So it's technically unsound to have added it
   in the first place.

 * Simply generating all those extra superclasses can generate lots of
   code that has to be zonked, only to be discarded later.  Better not
   to generate it in the first place.

   Moreover, if we simplify this implication more than once
   (e.g. because we can't solve it completely on the first iteration
   of simpl_loop), we'll generate all the same bindings AGAIN!

Easy solution: take advantage of the work we are doing to track dead
(unused) Givens, and use it to prune the Given bindings too.  This is
all done by neededEvVars.

This led to a remarkable 25% overall compiler allocation decrease in
test T12227.

But we don't get to discard all redundant equality superclasses, alas;
see #15205.

Note [Do not simplify ConstraintHoles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before printing the inferred value for a type hole (a _ wildcard in
a partial type signature), we simplify it w.r.t. any Givens. This
makes for an easier-to-understand diagnostic for the user.

However, we do not wish to do this for extra-constraint holes. Here is
the example for why (partial-sigs/should_compile/T12844):

  bar :: _ => FooData rngs
  bar = foo

  data FooData rngs

  class Foo xs where foo :: (Head xs ~ '(r,r')) => FooData xs

  type family Head (xs :: [k]) where Head (x ': xs) = x

GHC correctly infers that the extra-constraints wildcard on `bar`
should be (Head rngs ~ '(r, r'), Foo rngs). It then adds this
constraint as a Given on the implication constraint for `bar`. (This
implication is emitted by emitResidualConstraints.) The Hole for the _
is stored within the implication's WantedConstraints.  When
simplifyHoles is called, that constraint is already assumed as a
Given. Simplifying with respect to it turns it into ('(r, r') ~ '(r,
r'), Foo rngs), which is disastrous.

Furthermore, there is no need to simplify here: extra-constraints wildcards
are filled in with the output of the solver, in chooseInferredQuantifiers
(choose_psig_context), so they are already simplified. (Contrast to normal
type holes, which are just bound to a meta-variable.) Avoiding the poor output
is simple: just don't simplify extra-constraints wildcards.

This is the only reason we need to track ConstraintHole separately
from TypeHole in HoleSort.

See also Note [Extra-constraint holes in partial type signatures]
in GHC.Tc.Gen.HsType.

Note [Tracking redundant constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Opt_WarnRedundantConstraints, GHC can report which constraints of a type
signature (or instance declaration) are redundant, and can be omitted.  Here is
an overview of how it works.

This is all tested in typecheck/should_compile/T20602 (among others).

How tracking works:

* We maintain the `ic_need` field in an implication:
     ic_need: the set of Given evidence variables that are needed somewhere
              inside this implication; and are bound either by this implication
              or by an enclosing one.

* `setImplicationStatus` does all the work:
  - When the constraint solver finishes solving all the wanteds in
    an implication, it sets its status to IC_Solved

  - `neededEvVars`: computes which evidence variables are needed by an
    implication in `setImplicationStatus`.  A variable is needed if

      a) It is in the ic_need field of this implication, computed in
         a previous call to `setImplicationStatus`; see (TRC1)

      b) It is in the ics_need of a nested implication; see `add_implic`
         in `neededEvVars`

      c) It is free in the RHS of any /Wanted/ EvBind; each such binding
         solves a Wanted, so we want them all.  See `add_wanted` in
         `neededEvVars`

      d) It is free in the RHS of a /Given/ EvBind whose LHS is needed:
         see `findNeededGivenEvVars` called from `neededEvVars`.

  - Next, if the final status is IC_Solved, `setImplicationStatus` uses
    `findRedundantGivens` to decide which of this implication's Givens
    are redundant.

  - It also uses `pruneImplications` to discard any now-unnecessary child
    implications.

* GHC.Tc.Errors does the actual warning, in `warnRedundantConstraints`.


Wrinkles:

(TRC1) `pruneImplications` drops any sub-implications of an Implication
  that are irrelevant for error reporting:
      - no unsolved wanteds
      - no sub-implications
      - no redundant givens to report
  But in doing so we must not lose track of the variables that those implications
  needed!  So we track the ic_needs of all child implications in `ic_need_implics`.
  Crucially, this set includes things need by child implications that have been
  discarded by `pruneImplications`.

(TRC2) A Given can be redundant because it is implied by other Givens
         f :: (Eq a, Ord a)     => blah   -- Eq a unnecessary
         g :: (Eq a, a~b, Eq b) => blah   -- Either Eq a or Eq b unnecessary
   We nail this by using `mkMinimalBySCs` in `findRedundantGivens`.
   (TRC2a) But NOTE that we only attempt this mkMinimalBySCs stuff if all Givens
   used by evidence bindings.  Example:
      f :: (Eq a, Ord a) => a -> Bool
      f x = x == x
   We report (Ord a) as unused because it is. But we must not also report (Eq a)
   as unused because it is a superclass of Ord!

(TRC3) When two Givens are the same, prefer one that does not involve superclass
  selection, or more generally has shallower superclass-selection depth:
  see 2(b,c) in Note [Replacement vs keeping] in GHC.Tc.Solver.InertSet.
    e.g        f :: (Eq a, Ord a) => a -> Bool
               f x = x == x
  Eager superclass expansion gives us two [G] Eq a constraints. We want to keep
  the one from the user-written Eq a, not the superclass selection. This means
  we report the Ord a as redundant with -Wredundant-constraints, not the Eq a.
  Getting this wrong was #20602.

(TRC4) We don't compute redundant givens for *every* implication; only
  for those which reply True to `warnRedundantGivens`:

   - For example, in a class declaration, the default method *can*
     use the class constraint, but it certainly doesn't *have* to,
     and we don't want to report an error there.  Ditto instance decls.

   - More subtly, in a function definition
       f :: (Ord a, Ord a, Ix a) => a -> a
       f x = rhs
     we do an ambiguity check on the type (which would find that one
     of the Ord a constraints was redundant), and then we check that
     the definition has that type (which might find that both are
     redundant).  We don't want to report the same error twice, so we
     disable it for the ambiguity check.  Hence using two different
     FunSigCtxts, one with the warn-redundant field set True, and the
     other set False in
        - GHC.Tc.Gen.Bind.tcSpecPrag
        - GHC.Tc.Gen.Bind.tcTySig

   - We do not want to report redundant constraints for implications
     that come from quantified constraints.  Example #23323:
        data T a
        instance Show (T a) where ...  -- No context!
        foo :: forall f c. (forall a. c a => Show (f a)) => Proxy c -> f Int -> Int
        bar = foo @T @Eq

     The call to `foo` gives us
       [W] d : (forall a. Eq a => Show (T a))
     To solve this, GHC.Tc.Solver.Solve.solveForAll makes an implication constraint:
       forall a. Eq a =>  [W] ds : Show (T a)
     and because of the degnerate instance for `Show (T a)`, we don't need the `Eq a`
     constraint.  But we don't want to report it as redundant!

(TRC5) Consider this (#25992), where `op2` has a default method
        class C a where { op1, op2 :: a -> a
                        ; op2 = op1 . op1 }
        instance C a => C [a] where
          op1 x = x

  Plainly the (C a) constraint is unused; but the expanded decl will look like
        $dmop2 :: C a => a -> a
        $dmop2 = op1 . op1

        $fCList :: forall a. C a => C [a]
        $fCList @a (d::C a) = MkC (\(x:a).x) ($dmop2 @a d)

   Notice that `d` gets passed to `$dmop`: it is "needed".  But it's only
   /really/ needed if some /other/ method (in this case `op1`) uses it.

   So, rather than one set of "needed Givens" we use `EvNeedSet` to track
   a /pair/ of sets:
      ens_dms: needed /only/ by default-method calls
      ens_fvs: needed by something other than a default-method call
   It's a bit of a palaver, but not really difficult.
   All the logic is localised in `neededEvVars`.

   But NOTE that this only applies to /vanilla/ default methods.
   For /generic/ default methods, like
            class D a where { op1 :: blah
                            ; default op1 :: Eq a => blah2 }
   the (Eq a) constraint really is needed (e.g. class NFData and #25992).
   Hence the `Bool` field of `MethSkol` indicates a /vanilla/ default method.

----- Examples

    f, g, h :: (Eq a, Ord a) => a -> Bool
    f x = x == x
    g x = x > x
    h x = x == x && x > x

    All of f,g,h will discover that they have two [G] Eq a constraints: one as
    given and one extracted from the Ord a constraint. They will both discard
    the latter; see (TRC3).

    The body of f uses the [G] Eq a, but not the [G] Ord a. It will report a
    redundant Ord a.

    The body of g uses the [G] Ord a, but not the [G] Eq a. It will report a
    redundant Eq a.

    The body of h uses both [G] Ord a and [G] Eq a; each is used in a solved
    Wanted evidence binding.  But (TRC2) kicks in and discovers the Eq a
    is redundant.

----- Shortcomings

Shortcoming 1.  Consider

  j :: (Eq a, a ~ b) => a -> Bool
  j x = x == x

  k :: (Eq a, b ~ a) => a -> Bool
  k x = x == x

Currently (Nov 2021), j issues no warning, while k says that b ~ a
is redundant. This is because j uses the a ~ b constraint to rewrite
everything to be in terms of b, while k does none of that. This is
ridiculous, but I (Richard E) don't see a good fix.

Shortcoming 2.  Removing a redundant constraint can cause clients to fail to
compile, by making the function more polymorphic. Consider (#16154)

  f :: (a ~ Bool) => a -> Int
  f x = 3

  g :: String -> Int
  g s = f (read s)

The constraint in f's signature is redundant; not used to typecheck
`f`.  And yet if you remove it, `g` won't compile, because there'll
be an ambiguous variable in `g`.


**********************************************************************
*                                                                    *
*                      Main Solver                                   *
*                                                                    *
**********************************************************************

Note [Basic Simplifier Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Pick an element from the WorkList if there exists one with depth
   less than our context-stack depth.

2. Run it down the 'stage' pipeline. Stages are:
      - canonicalization
      - inert reactions
      - spontaneous reactions
      - top-level interactions
   Each stage returns a StopOrContinue and may have sideeffected
   the inerts or worklist.

   The threading of the stages is as follows:
      - If (Stop) is returned by a stage then we start again from Step 1.
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to
        the next stage in the pipeline.
4. If the element has survived (i.e. ContinueWith x) the last stage
   then we add it in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack
depth and will simply fail.
-}

solveSimpleGivens :: [Ct] -> TcS ()
solveSimpleGivens givens
  | null givens  -- Shortcut for common case
  = return ()
  | otherwise
  = do { traceTcS "solveSimpleGivens {" (ppr givens)
       ; go givens

       -- Capture the Givens in the inert_givens of the inert set
       -- for use by subsequent calls of nestImplicTcS
       -- See Note [trySolveImplication]
       ; updInertSet (\is -> is { inert_givens = inert_cans is })

       ; cans <- getInertCans
       ; traceTcS "End solveSimpleGivens }" (ppr cans) }
  where
    go givens = do { solveSimples (listToBag givens)
                   ; new_givens <- runTcPluginsGiven
                   ; when (notNull new_givens) $
                     go new_givens }

solveSimpleWanteds :: Cts -> TcS WantedConstraints
-- Returns unsolved constraints, mostly just flat ones (Cts),
-- but also any unsolved implications arising from forall-constraints
-- The result is not necessarily zonked
solveSimpleWanteds simples
  = do { mode   <- getTcSMode
       ; dflags <- getDynFlags
       ; inerts <- getInertSet
       ; let max_iter = solverIterations dflags

       ; traceTcS "solveSimpleWanteds {" $
         vcat [ text "Mode:" <+> ppr mode
              , text "Inerts:" <+> ppr inerts
              , text "Wanteds to solve:" <+> ppr simples ]

       ; let wc = emptyWC { wc_simple = simples }
       ; wc' <- iterateToFixpoint max_iter do_solve_and_plugins wc

       ; traceTcS "solveSimpleWanteds end }" $
             vcat [ text "residual =" <+> ppr wc' ]

       ; return wc' }
  where
    do_solve_and_plugins :: WantedConstraints -> TcS (Bool,WantedConstraints)
    do_solve_and_plugins wc
      = do { wc1 <- simple_solver wc
           ; (rerun_plugin, simples2) <- runTcPluginsWanted (wc_simple wc1)
           ; return (rerun_plugin, wc1 { wc_simple = simples2 }) }

    simple_solver :: WantedConstraints -> TcS WantedConstraints
    -- Try solving the wc_simple part of these constraints, once
    -- Affects the unification state (of course) but not the inert set
    -- The result is not necessarily zonked
    simple_solver wc@(WC { wc_simple = simples, wc_impl = implics })
      | isEmptyBag simples
      = return wc
      | otherwise
      = nestTcS $
        do { solveSimples simples
           ; simples1 <- getUnsolvedInerts
               -- Now try to solve any Wanted quantified
               -- constraints (i.e. QCInsts) in `simples1`
           ; (simples2, extra_implics) <- solveWantedQCIs simples1
           ; return (wc { wc_simple = simples2
                        , wc_impl   = implics `unionBags` extra_implics }) }

iterateToFixpoint :: IntWithInf
                  -> (WantedConstraints -> TcS (Bool,WantedConstraints))
                  -> WantedConstraints -> TcS WantedConstraints
-- See Note [The solveSimpleWanteds loop]
iterateToFixpoint max_iter do_it wc_orig
  = go 1 wc_orig
  where
    go :: Int -> WantedConstraints -> TcS WantedConstraints
    go n wc
      | n `intGtLimit` max_iter
      = failTcS (TcRnSimplifierTooManyIterations max_iter wc_orig)

      | otherwise
      = do { (something_happened, wc1) <- do_it wc
           ; if something_happened
             then go (n+1) wc1
             else return wc1 }


{- Note [The solveSimpleWanteds loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a bunch of simple constraints is done in a loop,
(the 'go' loop of 'solveSimpleWanteds'):
  1. Try to solve them
  2. Try the plugin
  3. If the plugin wants to run again, go back to step 1
-}

{-
************************************************************************
*                                                                      *
           Solving flat constraints: solveSimples
*                                                                      *
********************************************************************* -}

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------

solveSimples :: Cts -> TcS ()
-- Solve this bag of constraints,
-- returning the final InertSet in TcS
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { emitWork cts; solve_loop }
  where
    solve_loop
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem
           ; case sel of
              Nothing -> return ()
              Just ct -> do { solveOne ct
                            ; solve_loop } }

solveOne :: Ct -> TcS ()  -- Solve one constraint
solveOne workItem
  = do { wl      <- getWorkList
       ; inerts  <- getInertSet
       ; tclevel <- TcS.getTcLevel
       ; traceTcS "----------------------------- " empty
       ; traceTcS "Start solver pipeline {" $
                  vcat [ text "tclevel =" <+> ppr tclevel
                       , text "work item =" <+> ppr workItem
                       , text "inerts =" <+> ppr inerts
                       , text "rest of worklist =" <+> ppr wl ]

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; solve workItem }
  where
    solve :: Ct -> TcS ()
    solve ct
      = do { traceTcS "solve {" (text "workitem = " <+> ppr ct)
           ; res <- runSolverStage (solveCt ct)
           ; traceTcS "end solve }" (ppr res)
           ; case res of
               StartAgain ct -> do { traceTcS "Go round again" (ppr ct)
                                   ; solve ct }

               Stop ev s -> do { traceFireTcS ev s
                               ; traceTcS "End solver pipeline }" empty
                               ; return () }

               -- ContinueWith can't happen: res :: SolverStage Void
               -- solveCt either solves the constraint, or puts
               -- the unsolved constraint in the inert set.
            }

{- *********************************************************************
*                                                                      *
*              Solving one constraint: solveCt
*                                                                      *
************************************************************************

Note [Canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~
Canonicalization converts a simple constraint to a canonical form. It is
unary (i.e. treats individual constraints one at a time).

Constraints originating from user-written code come into being as
CNonCanonicals. We know nothing about these constraints. So, first:

     Classify CNonCanoncal constraints, depending on whether they
     are equalities, class predicates, or other.

Then proceed depending on the shape of the constraint. Generally speaking,
each constraint gets rewritten and then decomposed into one of several forms
(see type Ct in GHC.Tc.Types).

When an already-canonicalized constraint gets kicked out of the inert set,
it must be recanonicalized. But we know a bit about its shape from the
last time through, so we can skip the classification step.
-}

solveCt :: Ct -> SolverStage Void
-- The Void result tells us that solveCt cannot return
-- a ContinueWith; it must return Stop or StartAgain.
solveCt (CNonCanonical ev)                   = solveNC ev
solveCt (CIrredCan (IrredCt { ir_ev = ev })) = solveNC ev

solveCt (CEqCan (EqCt { eq_ev = ev, eq_eq_rel = eq_rel
                      , eq_lhs = lhs, eq_rhs = rhs }))
  = solveEquality ev eq_rel (canEqLHSType lhs) rhs

solveCt (CQuantCan qci@(QCI { qci_ev = ev }))
  = do { ev' <- rewriteEvidence ev
         -- It is (much) easier to rewrite and re-classify than to
         -- rewrite the pieces and build a Reduction that will rewrite
         -- the whole constraint
       ; case classifyPredType (ctEvPred ev') of
           ForAllPred tvs theta body_pred
             -> solveForAll (qci { qci_ev = ev', qci_tvs = tvs
                                 , qci_theta = theta, qci_body = body_pred })
           _ -> pprPanic "SolveCt" (ppr ev) }

solveCt (CDictCan (DictCt { di_ev = ev, di_pend_sc = pend_sc }))
  = do { ev <- rewriteEvidence ev
         -- It is easier to rewrite and re-classify than to rewrite
         -- the pieces and build a Reduction that will rewrite the
         -- whole constraint
       ; case classifyPredType (ctEvPred ev) of
           ClassPred cls tys
             -> solveDict (DictCt { di_ev = ev, di_cls = cls
                                  , di_tys = tys, di_pend_sc = pend_sc })
           _ -> pprPanic "solveCt" (ppr ev) }

------------------
solveNC :: CtEvidence -> SolverStage Void
solveNC ev
  = -- Instead of rewriting the evidence before classifying, it's possible we
    -- can make progress without the rewrite. Try this first.
    -- For insolubles (all of which are equalities), do /not/ rewrite the arguments
    -- In #14350 doing so led entire-unnecessary and ridiculously large
    -- type function expansion.  Instead, canEqNC just applies
    -- the substitution to the predicate, and may do decomposition;
    --    e.g. a ~ [a], where [G] a ~ [Int], can decompose
    case classifyPredType (ctEvPred ev) of {
        EqPred eq_rel ty1 ty2 -> solveEquality ev eq_rel ty1 ty2 ;
        _ ->

    -- Do rewriting on the constraint, especially zonking
    do { ev <- rewriteEvidence ev

    -- And then re-classify
       ; case classifyPredType (ctEvPred ev) of
           ClassPred cls tys     -> solveDictNC ev cls tys
           ForAllPred tvs th p   -> solveForAllNC ev tvs th p
           IrredPred {}          -> solveIrred (IrredCt { ir_ev = ev, ir_reason = IrredShapeReason })
           EqPred eq_rel ty1 ty2 -> solveEquality ev eq_rel ty1 ty2
              -- EqPred only happens if (say) `c` is unified with `a ~# b`,
              -- but that is rare because it requires c :: CONSTRAINT UnliftedRep

    }}


{- *********************************************************************
*                                                                      *
*                      Quantified constraints
*                                                                      *
********************************************************************* -}

{- Note [Quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The -XQuantifiedConstraints extension allows type-class contexts like this:

  data Rose f x = Rose x (f (Rose f x))

  instance (Eq a, forall b. Eq b => Eq (f b))
        => Eq (Rose f a)  where
    (Rose x1 rs1) == (Rose x2 rs2) = x1==x2 && rs1 == rs2

Note the (forall b. Eq b => Eq (f b)) in the instance contexts.
This quantified constraint is needed to solve the
 [W] (Eq (f (Rose f x)))
constraint which arises form the (==) definition.

The wiki page is
  https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints
which in turn contains a link to the GHC Proposal where the change
is specified, and a Haskell Symposium paper about it.

We implement two main extensions to the design in the paper:

 1. We allow a variable in the instance head, e.g.
      f :: forall m a. (forall b. m b) => D (m a)
    Notice the 'm' in the head of the quantified constraint, not
    a class.

 2. We support superclasses to quantified constraints.
    For example (contrived):
      f :: (Ord b, forall b. Ord b => Ord (m b)) => m a -> m a -> Bool
      f x y = x==y
    Here we need (Eq (m a)); but the quantified constraint deals only
    with Ord.  But we can make it work by using its superclass.

Here are the moving parts
  * Language extension {-# LANGUAGE QuantifiedConstraints #-}
    and add it to ghc-boot-th:GHC.LanguageExtensions.Type.Extension

  * A new form of evidence, EvDFun, that is used to discharge
    such wanted constraints

  * checkValidType gets some changes to accept forall-constraints
    only in the right places.

  * Predicate.Pred gets a new constructor ForAllPred, and
    and `classifyPredType` analyses a `PredType` to decompose
    the new forall-constraints

  * GHC.Tc.Solver.Monad.InertCans gets an extra field, `inert_qcis`,
    which holds all the Given forall-constraints.  In effect,
    such Given constraints are like local instance decls.

  * `inert_qcis` also temporarily holds Wanted quantified constraints
    (see Note [Solving a Wanted forall-constraint])

  * When trying to solve a class constraint, GHC.Tc.Solver.Dict.matchLocalInst
    (note "local" inst) uses the Given `inert_qcis` to solve the constraint.

  * `solveForAll` deals with solving a forall-constraint.  See
       * Note [Solving a Given forall-constraint]
       * Note [Solving a Wanted forall-constraint]

Note that a quantified constraint is never /inferred/
(by GHC.Tc.Solver.simplifyInfer).  A function can only have a
quantified constraint in its type if it is given an explicit
type signature.

Note [Solving a Given forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a Given constraint
  [G] df :: forall ab. (Eq a, Ord b) => C x a b
we just add it to TcS's local InstEnv of known instances, `inert_qcis`,
via addInertQCI.  Then, if we look up (C x Int Bool), say,
we'll find a match in the `inert_qcis`.

Note [Solving a Wanted forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a wanted forall (quantified) constraint
  [W] df :: forall a b. (Eq a, Ord b) => C x a b
is delightfully easy in principle.   Just build an implication constraint
    forall ab. (g1::Eq a, g2::Ord b) => [W] d :: C x a
and discharge df thus:
    df = /\ab. \g1 g2. let <binds> in d
where <binds> is filled in by solving the implication constraint.

What we actually do is this:

* In `solveForAll` we see if we have an identical quantified constraint
  to solve it (using tryInertQCs).  In particular, solve a Wanted QCI
  from an identical Given.  This is more than a simple optimisation:
  see Note [Solving Wanted QCs from Given QCs]

  If not, just stash it in `inert_qcis :: [QCInst]`. (If it's a Given
  we can use it to solve other constraints; if a Wanted we will solve
  it later using `solveWantedQCIs`.)

* In the main `solveSimpleWanteds` (specifically `solve_one`):

  - We attempt to solve the `wc_simple` constraints with `solveSimples`
    Unsolved quantified constraints just accumulate in the `inert_qcis` field
    of the `InertSet`.

  - Then we use `solveWantedQCIs` to solve any quantified constraints. That
    often turns the `QCInst` into an `Implication`; but not invariably (WFA4)

Wrinkles:

(WFA2) Termination: see #19690.  We want to maintain the invariant (QC-INV):

    (QC-INV) Every quantified constraint returns a non-bottom dictionary

  just as every top-level instance declaration guarantees to return a non-bottom
  dictionary.  But as #19690 shows, it is possible to get a bottom dictionary
  by superclass selection if we aren't careful.  The situation is very similar
  to that described in Note [Recursive superclasses] in GHC.Tc.TyCl.Instance;
  and we use the same solution:

  * Give the Givens a CtOrigin of (GivenOrigin (InstSkol IsQC head_size))
  * Give the Wanted a CtOrigin of (ScOrigin IsQC NakedSc)

  Both of these things are done in `solveWantedQCI`.  Now the mechanism described
  in Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance takes over.

(WFA3) Error messages. Suppose we are trying to solve the quantified constraint
            forall a. Eq a => Eq (c a)
  We don't just want to say "No instance for Eq (c a)".  It /really/ helps to
  say what quantified constraint we were trying to solve.

  So the `IsQC` origin carries that info, and `GHC.Tc.Errors.Ppr.pprQCOriginExtra`
  prints the extra info.

(WFA4) When `tcsmFullySolveQCIs` is on, we adopt an all-or-nothing strategy:
   either solve the forall-constraint /fully/ or do nothing at all.
   Why?  See (NFS1) in Note [Handling new-form SPECIALISE pragmas] in GHC.Tc.Gen.Sig

(WFA5) Why not /always/ us the all-or-nothing strategy, so we don't need a
  flag?  Several reasons:

  * Less efficient; `tcsmFullySolveQCIs` abandons the work done on the constraint,
    so we might do it again next time around.

  * More importantly, we would get worse results from `deriving`: #26315.
    In that code the `deriving` mechanism was trying to solve
           [W] df :: forall n. Eq (Const i n)
    If we turn it into an implication, we can simplfy that `Const` to get
    the residual implication
           forall n.  [W] d :: Eq i
    And then `approximateWC` can extract the (Eq i) as a plausible context for
    the instance.

  * Very much the same issue came up for the inferred type of a function that
    lacks a type signature #26376.  Again, if the forall-constraint is not
    turned into an implication `approximateWC` gives a less-good answer.

Note [Solving Wanted QCs from Given QCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are about to solve a Wanted quantified constraint, and there is a
Given quantified constraint with the same type, we should directly solve the
Wanted from the Given (instead of building an implication).

Not only is this more direct and efficient, sometimes it is also /necessary/.
Consider:

  f :: forall a k. (Eq a, forall x. Eq x => Eq k x) => a -> blah
  {-# SPECIALISE (f :: forall k. (forall x. Eq x => Eq k x) => Int -> blah #-}

Here we specialise the `a` parameter to `f`, leaving the quantified constraint
untouched.  We want to get a rule like:

  RULE  forall @k (d :: forall x. Eq x => Eq k x).
            f @Int @k d = $sf @k d

But when we typecheck that expression-with-a-type-signature, if we don't solve
Wanted forall constraints directly, we will do so indirectly and end up with
this as the LHS of the RULE:

  (/\k \(df::forall x.Eq x => Eq k x). f @Int @k (/\x \(d:Eq x). df @x d))
     @kk dd

We run the simple optimiser on that, which eliminates the beta-redex. However,
it may not eta-reduce that `/\x \(d:Eq x)...`, because we are cautious about
eta-reduction. So we may be left with an over-complicated and hard-to-match
RULE LHS. It's all a bit silly, because the implication constraint is /identical/;
we just need to spot it.

This came up while implementing GHC proposal 493 (allowing expresions in
SPECIALISE pragmas).
-}

-- | Solve a quantified constraint that came from @CNonCanonical@ (which means
-- that superclasses have not yet been expanded).
--
-- Precondition: the constraint has already been rewritten by the inert set.
solveForAllNC :: CtEvidence -> [TcTyVar] -> TcThetaType -> TcPredType
              -> SolverStage Void
solveForAllNC ev tvs theta body_pred
  = do { fuel <- simpleStage mk_super_classes
       ; solveForAll (QCI { qci_ev = ev, qci_tvs = tvs, qci_theta = theta
                          , qci_body = body_pred, qci_pend_sc = fuel }) }

  where
    mk_super_classes :: TcS ExpansionFuel
    mk_super_classes
       | Just (cls,tys) <- getClassPredTys_maybe body_pred
       , classHasSCs cls
       = do { dflags <- getDynFlags
            -- Either expand superclasses (Givens) or provide fuel to do so (Wanteds)
            ; if isGiven ev
              then
                -- See Note [Eagerly expand given superclasses]
                -- givensFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
                do { sc_cts <- mkStrictSuperClasses (givensFuel dflags) ev tvs theta cls tys
                   ; emitWork (listToBag sc_cts)
                   ; return doNotExpand }
              else
                -- See invariants (a) and (b) in QCI.qci_pend_sc
                -- qcsFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
                -- See Note [Quantified constraints]
                return (qcsFuel dflags)
            }

       | otherwise
       = return doNotExpand

-- | Solve a canonical quantified constraint.
--
-- Precondition: the constraint has already been rewritten by the inert set.
solveForAll :: QCInst -> SolverStage Void
solveForAll qci@(QCI { qci_ev = ev })
  = case ev of
      CtGiven {} ->
        -- See Note [Solving a Given forall-constraint]
        do { simpleStage (addInertQCI qci)
           ; stopWithStage ev "Given forall-constraint" }
      CtWanted {} ->
        -- See Note [Solving a Wanted forall-constraint]
        do { tryInertQCs qci
           ; simpleStage (addInertQCI qci)
           ; stopWithStage ev "Wanted forall-constraint" }

tryInertQCs :: QCInst -> SolverStage ()
tryInertQCs qc
  = Stage $
    do { inerts <- getInertCans
       ; try_inert_qcs qc (inert_qcis inerts) }

try_inert_qcs :: QCInst -> [QCInst] -> TcS (StopOrContinue ())
try_inert_qcs (QCI { qci_ev = ev_w }) inerts =
  case mapMaybe matching_inert inerts of
    [] -> do { traceTcS "tryInertQCs:nothing" (ppr ev_w $$ ppr inerts)
             ; continueWith () }
    ev_i:_ ->
      do { traceTcS "tryInertQCs:KeepInert" (ppr ev_i)
         ; setEvBindIfWanted ev_w EvCanonical (ctEvTerm ev_i)
         ; stopWith ev_w "Solved Wanted forall-constraint from inert" }
  where
    matching_inert (QCI { qci_ev = ev_i })
      | ctEvPred ev_i `tcEqType` ctEvPred ev_w
      = Just ev_i
      | otherwise
      = Nothing

solveWantedQCIs :: Cts -> TcS (Cts, Bag Implication)
solveWantedQCIs wanteds
  = do { mode <- getTcSMode
       ; bag_of_eithers <- mapBagM (solveWantedQCI mode) wanteds
         -- bag_of_eithers :: Bag (Either Ct Implication)
       ; return (partitionBagWith id bag_of_eithers) }

solveWantedQCI :: TcSMode
               -> Ct   -- Definitely a Wanted
               -> TcS (Either Ct Implication)
-- Try to solve a quantified constraint, `ct`
-- Returns
--    (Left ct) if `ct` is not a quantified constraint
--    (Right implic) if we can solve a quantified constraint `ct` by creating
--                   an implication and fully or partly solving it
--    (Left ct) for a quantified constraint that can't be /fully solved/,
--              but mode is tcsmFullySolveQCIs
-- See Note [Solving a Wanted forall-constraint]
solveWantedQCI mode ct@(CQuantCan (QCI { qci_ev =  ev, qci_tvs = tvs
                                       , qci_theta = theta, qci_body = body_pred }))
  | CtWanted (WantedCt { ctev_pred = forall_pred, ctev_dest = dest
                       , ctev_rewriters = rewriters, ctev_loc = loc }) <- ev
  , let is_qc = IsQC forall_pred (ctLocOrigin loc)
        empty_subst = mkEmptySubst $ mkInScopeSet $
                      tyCoVarsOfTypes (body_pred:theta) `delVarSetList` tvs
  = TcS.setSrcSpan (getCtLocEnvLoc $ ctLocEnv loc) $
    -- This setSrcSpan is important: the TcM.newImplication uses that
    -- TcLclEnv for the implication, and that in turn sets the location
    -- for the Givens when solving the constraint (#21006)
    do { -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
         -- Very like the code in tcSkolDFunType
       ; rec { skol_info <- mkSkolemInfo skol_info_anon
             ; (subst, skol_tvs) <- tcInstSkolTyVarsX skol_info empty_subst tvs
             ; let inst_pred  = substTy    subst body_pred
                   inst_theta = substTheta subst theta
                   skol_info_anon = InstSkol is_qc (pSizeHead inst_pred) }

       ; given_ev_vars <- mapM TcS.newEvVar inst_theta
       ; (lvl, wanted_ev)
             <- pushLevelNoWorkList (ppr skol_info) $
                do { let loc' = setCtLocOrigin loc (ScOrigin is_qc NakedSc)
                         -- Set the thing to prove to have a ScOrigin, so we are
                         -- careful about its termination checks.
                         -- See (QC-INV) in Note [Solving a Wanted forall-constraint]
                   ; newWantedNC loc' rewriters inst_pred }

       ; ev_binds_var <- TcS.newTcEvBinds
       ; let imp :: Implication
             imp = (implicationPrototype (ctLocEnv loc))
                     { ic_tclvl  = lvl
                     , ic_skols  = skol_tvs
                     , ic_given  = given_ev_vars
                     , ic_wanted = mkSimpleWC [CtWanted wanted_ev]
                     , ic_binds  = ev_binds_var
                     , ic_warn_inaccessible = False
                     , ic_info   = skol_info_anon }

      ; imp' <- solveImplication imp


      ; if | tcsmFullySolveQCIs mode
           , not (isSolvedStatus (ic_status imp'))
           -> -- Not fully solved, but mode says that we must fully
              -- solve quantified constraints; so abandon the attempt
              -- See (WFA4) in Note [Solving a Wanted forall-constraint]
              return (Left ct)

           | otherwise
           -> -- Commit to the (partly or fully solved) implication
              -- See (WFA5) in Note [Solving a Wanted forall-constraint]
              -- Record evidence and return residual implication
              -- NB: even if it is fully solved we must return it, because it is
              --     carrying a record of which evidence variables are used
              --     See Note [Free vars of EvFun] in GHC.Tc.Types.Evidence
             do { setWantedEvTerm dest EvCanonical $
                  EvFun { et_tvs = skol_tvs, et_given = given_ev_vars
                        , et_binds = TcEvBinds ev_binds_var
                        , et_body = wantedCtEvEvId wanted_ev }

                ; return (Right imp') }
    }

  | otherwise  -- A Given QCInst
  = pprPanic "wantedQciToImplic: found a Given QCI" (ppr ct)

-- No-op on all Ct's other than CQuantCan
solveWantedQCI _ ct = return (Left ct)


{-
************************************************************************
*                                                                      *
                  Evidence transformation
*                                                                      *
************************************************************************
-}

rewriteEvidence :: CtEvidence -> SolverStage CtEvidence
-- (rewriteEvidence old_ev new_pred co do_next)
-- Main purpose: create new evidence for new_pred;
--                 unless new_pred is cached already
-- * Calls do_next with (new_ev :: new_pred), with same wanted/given flag as old_ev
-- * If old_ev was wanted, create a binding for old_ev, in terms of new_ev
-- * If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
-- * Stops if new_ev is already cached
--
--        Old evidence    New predicate is               Return new evidence
--        flavour                                        of same flavor
--        -------------------------------------------------------------------
--        Wanted          Already solved or in inert     Stop
--                        Not                            do_next new_evidence
--
--        Given           Already in inert               Stop
--                        Not                            do_next new_evidence

{- Note [Rewriting with Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the coercion is just reflexivity then you may re-use the same
evidence variable.  But be careful!  Although the coercion is Refl, new_pred
may reflect the result of unification alpha := ty, so new_pred might
not _look_ the same as old_pred, and it's vital to proceed from now on
using new_pred.

The rewriter preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.

If we are rewriting with Refl, then there are no new rewriters to add to
the rewriter set. We check this with an assertion.
 -}


rewriteEvidence ev
  = Stage $ do { traceTcS "rewriteEvidence" (ppr ev)
               ; (redn, rewriters) <- rewrite ev (ctEvPred ev)
               ; finish_rewrite ev redn rewriters }

finish_rewrite :: CtEvidence   -- ^ old evidence
               -> Reduction    -- ^ new predicate + coercion, of type <type of old evidence> ~ new predicate
               -> RewriterSet  -- ^ See Note [Wanteds rewrite Wanteds]
                               -- in GHC.Tc.Types.Constraint
               -> TcS (StopOrContinue CtEvidence)
finish_rewrite old_ev (Reduction co new_pred) rewriters
  | isReflCo co -- See Note [Rewriting with Refl]
  = assert (isEmptyRewriterSet rewriters) $
    continueWith (setCtEvPredType old_ev new_pred)

finish_rewrite
  ev@(CtGiven (GivenCt { ctev_evar = old_evar }))
  (Reduction co new_pred)
  rewriters
  = assert (isEmptyRewriterSet rewriters) $ -- this is a Given, not a wanted
    do { let loc = ctEvLoc ev
             -- mkEvCast optimises ReflCo
             ev_rw_role = ctEvRewriteRole ev
             new_tm = assert (coercionRole co == ev_rw_role)
                      evCast (evId old_evar) $   -- evCast optimises ReflCo
                      downgradeRole Representational ev_rw_role co
       ; new_ev <- newGivenEv loc (new_pred, new_tm)
       ; continueWith $ CtGiven new_ev }

finish_rewrite
  ev@(CtWanted (WantedCt { ctev_rewriters = rewriters, ctev_dest = dest }))
  (Reduction co new_pred)
  new_rewriters
  = do { let loc = ctEvLoc ev
             rewriters' = rewriters S.<> new_rewriters
             ev_rw_role = ctEvRewriteRole ev
       ; mb_new_ev <- newWanted loc rewriters' new_pred
       ; massert (coercionRole co == ev_rw_role)
       ; setWantedEvTerm dest EvCanonical $
         evCast (getEvExpr mb_new_ev)     $
         downgradeRole Representational ev_rw_role (mkSymCo co)
       ; case mb_new_ev of
            Fresh  new_ev -> continueWith $ CtWanted new_ev
            Cached _      -> stopWith ev "Cached wanted" }

{- *******************************************************************
*                                                                    *
*                      Typechecker plugins
*                                                                    *
******************************************************************* -}

-- | Extract the (inert) givens and invoke the plugins on them.
-- Remove solved givens from the inert set and emit insolubles, but
-- return new work produced so that 'solveSimpleGivens' can feed it back
-- into the main solver.
runTcPluginsGiven :: TcS [Ct]
runTcPluginsGiven
  = do { solvers <- getTcPluginSolvers
       ; if null solvers then return [] else
    do { givens <- getInertGivens
       ; if null givens then return [] else
    do { traceTcS "runTcPluginsGiven {" (ppr givens)
       ; p <- runTcPluginSolvers solvers (givens,[])
       ; let (solved_givens, _) = pluginSolvedCts p
             insols             = map (ctIrredCt PluginReason) (pluginBadCts p)
       ; updInertCans (removeInertCts solved_givens .
                       updIrreds (addIrreds insols) )
       ; traceTcS "runTcPluginsGiven }" $
         vcat [ text "solved_givens:" <+> ppr solved_givens
              , text "insols:" <+> ppr insols
              , text "new:" <+> ppr (pluginNewCts p) ]
       ; return (pluginNewCts p) } } }

-- | Given a bag of (rewritten, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: Cts -> TcS (Bool, Cts)
runTcPluginsWanted wanted
  | isEmptyBag wanted
  = return (False, wanted)
  | otherwise
  = do { solvers <- getTcPluginSolvers
       ; if null solvers then return (False, wanted) else

    do { -- Find the set of Givens to give to the plugin.
         given <- getInertGivens

         -- Plugin requires zonked input wanteds
       ; zonked_wanted <- TcS.zonkSimples wanted

       ; traceTcS "Running plugins {" (vcat [ text "Given:" <+> ppr given
                                            , text "Wanted:" <+> ppr zonked_wanted ])
       ; p <- runTcPluginSolvers solvers (given, bagToList zonked_wanted)
       ; let (_, solved_wanted)   = pluginSolvedCts p
             (_, unsolved_wanted) = pluginInputCts p
             new_wanted     = pluginNewCts p
             insols         = pluginBadCts p
             all_new_wanted = listToBag new_wanted       `andCts`
                              listToBag unsolved_wanted  `andCts`
                              listToBag insols

       ; mapM_ setEv solved_wanted

       ; traceTcS "Finished plugins }" (ppr new_wanted)
       ; return ( notNull (pluginNewCts p), all_new_wanted ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted (WantedCt { ctev_dest = dest }) -> setWantedEvTerm dest EvCanonical ev
           -- TODO: plugins should be able to signal non-canonicity
      _ -> panic "runTcPluginsWanted.setEv: attempt to solve non-wanted!"

-- | A pair of (given, wanted) constraints to pass to plugins
type SplitCts  = ([Ct], [Ct])

-- | A solved pair of constraints, with evidence for wanteds
type SolvedCts = ([Ct], [(EvTerm,Ct)])

-- | Represents collections of constraints generated by typechecker
-- plugins
data TcPluginProgress = TcPluginProgress
    { pluginInputCts  :: SplitCts
      -- ^ Original inputs to the plugins with solved/bad constraints
      -- removed, but otherwise unmodified
    , pluginSolvedCts :: SolvedCts
      -- ^ Constraints solved by plugins
    , pluginBadCts    :: [Ct]
      -- ^ Constraints reported as insoluble by plugins
    , pluginNewCts    :: [Ct]
      -- ^ New constraints emitted by plugins
    }

getTcPluginSolvers :: TcS [TcPluginSolver]
getTcPluginSolvers
  = do { tcg_env <- TcS.getGblEnv; return (tcg_tc_plugin_solvers tcg_env) }

-- | Starting from a pair of (given, wanted) constraints,
-- invoke each of the typechecker constraint-solving plugins in turn and return
--
--  * the remaining unmodified constraints,
--  * constraints that have been solved,
--  * constraints that are insoluble, and
--  * new work.
--
-- Note that new work generated by one plugin will not be seen by
-- other plugins on this pass (but the main constraint solver will be
-- re-invoked and they will see it later).  There is no check that new
-- work differs from the original constraints supplied to the plugin:
-- the plugin itself should perform this check if necessary.
runTcPluginSolvers :: [TcPluginSolver] -> SplitCts -> TcS TcPluginProgress
runTcPluginSolvers solvers all_cts
  = do { ev_binds_var <- getTcEvBindsVar
       ; foldM (do_plugin ev_binds_var) initialProgress solvers }
  where
    do_plugin :: EvBindsVar -> TcPluginProgress -> TcPluginSolver -> TcS TcPluginProgress
    do_plugin ev_binds_var p solver = do
        result <- runTcPluginTcS (uncurry (solver ev_binds_var) (pluginInputCts p))
        return $ progress p result

    progress :: TcPluginProgress -> TcPluginSolveResult -> TcPluginProgress
    progress p
      (TcPluginSolveResult
        { tcPluginInsolubleCts = bad_cts
        , tcPluginSolvedCts    = solved_cts
        , tcPluginNewCts       = new_cts
        }
      ) =
        p { pluginInputCts  = discard (bad_cts ++ map snd solved_cts) (pluginInputCts p)
          , pluginSolvedCts = add solved_cts (pluginSolvedCts p)
          , pluginNewCts    = new_cts ++ pluginNewCts p
          , pluginBadCts    = bad_cts ++ pluginBadCts p
          }

    initialProgress = TcPluginProgress all_cts ([], []) [] []

    discard :: [Ct] -> SplitCts -> SplitCts
    discard cts (xs, ys) =
        (xs `without` cts, ys `without` cts)

    without :: [Ct] -> [Ct] -> [Ct]
    without = deleteFirstsBy eq_ct

    eq_ct :: Ct -> Ct -> Bool
    eq_ct c c' = ctFlavour c == ctFlavour c'
              && ctPred c `tcEqType` ctPred c'

    add :: [(EvTerm,Ct)] -> SolvedCts -> SolvedCts
    add xs scs = foldl' addOne scs xs

    addOne :: SolvedCts -> (EvTerm,Ct) -> SolvedCts
    addOne (givens, wanteds) (ev,ct) = case ctEvidence ct of
      CtGiven  {} -> (ct:givens, wanteds)
      CtWanted {} -> (givens, (ev,ct):wanteds)
