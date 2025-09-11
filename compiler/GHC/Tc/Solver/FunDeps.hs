{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

-- | Solving Class constraints CDictCan
module GHC.Tc.Solver.FunDeps (
  tryDictFunDeps,
  tryEqFunDeps
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Solver.Solve( solveSimpleWanteds )

import GHC.Tc.Instance.FunDeps
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify( UnifyEnv(..) )
import GHC.Tc.Utils.Monad    as TcM
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint

import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.Predicate( EqRel(..) )
import GHC.Core.TyCon
import GHC.Core.Unify( tcUnifyTyForInjectivity )
import GHC.Core.Coercion.Axiom

import GHC.Builtin.Types.Literals( tryInteractTopFam, tryInteractInertFam )
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( filterOut )

import GHC.Data.Pair


{- Note [Overview of fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is our plan for dealing with functional dependencies

* When we have failed to solve a Wanted constraint, do this
  1. Generate any fundep-equalities [FunDepEqn] from that constraint.
  2. Try to solve that [FunDepEqn]
  3. If any unifications happened, send the constraint back to the
     start of the pipeline

* Step (1) How we generate those [FunDepEqn] varies:
       - tryDictFunDeps: for class constraints (C t1 .. tn)
         we look at top-level instances and inert Givens
       - tryEqFunDeps: for type-family equalities (F t1 .. tn ~ ty)
         we look at top-level family instances
                    and inert Given family equalities

* Step (2). We use `solveFunDeps` to solve the [FunDepEqn] in a nested
  solver.  Key property:

      The ONLY effect of `solveFunDeps` is possibly to perform unifications:

      - It entirely discards any unsolved fundep equalities.

      - Ite entirely discards any evidence arising from solving fundep equalities

* Step (3) if we did any unifications in Step (2), we start again with the
  current unsolved Wanted.  It might now be soluble!

* For Given constraints, things are different:
    - tryDictFunDeps: we do nothing
    - tryEqFunDeps: for type-family equalities, we can produce new
        actual evidence for built-in type families.  E.g.
             [G] co : 3 ~ x + 1
        We can produce new evidence
             [G] co' : x ~ 2
  So we generate and emit fresh Givens.  See
     `improveGivenTopFunEqs` and `improveGivenLocalFunEqs`
  No unification is involved here, just emitting new Givens.

(FD1) Consequences for error messages.
  Because we discard any unsolved FunDepEqns, we get better error messages.
  Consider   class C a b | a -> b
             instance C Int Bool
  and [W] C Int Char
  We'll get an insoluble fundep-equality  (Char ~ Bool), but it's very
  unhelpful to report it.  Much better just to say
     No instance for C Int Bool

  Similarly if had [W] C Int S, [W] C Int T, it is not helpful to
  complain about insoluble (S ~ T).

Note [FunDep and implicit parameter reactions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies (including implicit
parameters), is that we simply produce new Wanted equalities.  So for example

        class D a b | a -> b where ...
    Inert:
        [G] d1 : D Int Bool
    WorkItem:
        [W] d2 : D Int alpha

    We generate the extra work item
        [W] cv : alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        [W] d2' : D Int Bool
        d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary
using those extra equalities.

If that were the case with the same inert set and work item we might discard
d2 directly:

        [W] cv : alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b
And:
     [G] d1 : C T Int Char
     [W] d2 : C T beta Int
In this case, it's *not even possible* to solve the wanted immediately.
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse:
     [G] d1 : C T Int beta
     [W] d2: C T beta Int
Then it is solvable, but its very hard to detect this on the spot.

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

Note [Partial functional dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#12522):
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints
  [W] F (alpha, beta) ~ (Int, <some type>)

The injectivity will give rise to fundep equalities
  [W] gamma1 ~ alpha
  [W] Int ~ beta

The fresh unification variable `gamma1` comes from the fact that we can only do
"partial improvement" here; see Section 5.2 of "Injective type families for
Haskell" (HS'15).

Now it is crucial that, when solving,
  we unify    gamma1 := alpha    (YES)
  and not     alpha := gamma1    (NO)

Why?  Because if we do (YES) we'll think we have made some progress
(some unification has happened), and hence go round again; but actually all we
have done is to replace `alpha` with `gamma1`.

These "fresh unification variables" in fundep-equalities are ubituitous.
For example
      class C a b | a -> b
      instance .. => C Int [x]
If we see
      [W] C Int alpha
we'll generate a fundep-equality   [W] alpha ~ [beta1]
where `beta1` is one of those "fresh unification variables

This problem shows up in several guises; see (at the bottom)
  * Historical Note [Improvement orientation]
  * Historical Note [Fundeps with instances, and equality orientation]

The solution is super-simple:

  * A fundep-equality is described by `FunDepEqn`, whose `fd_qtvs` field explicitly
    lists the "fresh variables"

  * Function `instantiateFunDepEqn` instantiates a `FunDepEqn`, and CRUCIALLY
    gives the new unification variables a level one deeper than the current
    level.

  * Now, given `alpha ~ beta`, all the unification machinery guarantees, to
    unify the variable with the deeper level.  See GHC.Tc.Utils.Unify
    Note [Deeper level on the left].  That ensures that the fresh `gamma1`
    will be eliminated in favour of `alpha`. Hooray.

  * Better still, we solve the [FunDepEqn] with
      solveFunDeps :: CtEvidence -> [FunDepEqn] -> TcS Bool
    It uses `reportUnifications` to see if any unification happened at this
    level or outside -- that is, it does NOT report unifications to the fresh
    unification variables.  So `solveFunDeps` returns True only if it
    unifies a variable /other than/ the fresh ones.  Bingo.

Another victory for levels numbers!

Note [Do fundeps last]
~~~~~~~~~~~~~~~~~~~~~~
Consider T4254b:
  class FD a b | a -> b where { op :: a -> b }

  instance FD Int Bool

  foo :: forall a b. (a~Int,FD a b) => a -> Bool
  foo = op

(DFL1) Try local fundeps first.
  From the ambiguity check on the type signature we get
    [G] FD Int b
    [W] FD Int beta
  If we interact that Wanted with /both/ the top-level instance, /and/ the
  local Given, we'll get
      beta ~ Int   and     beta ~ b
  respectively.  That would generate (b~Bool), which would fail.  I think
  it doesn't matter which of the two we pick, but historically we have
  picked the local-fundeps first.

(DFL2) Try solving from top-level instances before fundeps.
  From the definition `foo = op` we get
    [G] FD Int b
    [W] FD Int Bool
  We solve this from the top level instance before even trying fundeps.
  If we did try fundeps, we'd generate [W] b ~ Bool, which fails.

  (DFL2) is achieved by trying fundeps only on /unsolved/ Wanteds.

Note [Weird fundeps]
~~~~~~~~~~~~~~~~~~~~
Consider   class Het a b | a -> b where
              het :: m (f c) -> a -> m b

           class GHet (a :: * -> *) (b :: * -> *) | a -> b
           instance            GHet (K a) (K [a])
           instance Het a b => GHet (K a) (K b)

The two instances don't actually conflict on their fundeps,
although it's pretty strange.  So they are both accepted. Now
try   [W] GHet (K Int) (K Bool)
This triggers fundeps from both instance decls;
      [W] K Bool ~ K [a]
      [W] K Bool ~ K beta
And there's a risk of complaining about Bool ~ [a].  But in fact
the Wanted matches the second instance, so we never get as far
as the fundeps.

#7875 is a case in point.
-}


{- *********************************************************************
*                                                                      *
*          Functional dependencies for dictionaries
*                                                                      *
********************************************************************* -}

tryDictFunDeps :: DictCt -> SolverStage ()
-- (tryDictFunDeps inst_envs cts)
--   * Generate the fundeps from interacting the
--     top-level `inst_envs` with the constraints `cts`
--   * Do the unifications and return any unsolved constraints
-- See Note [Fundeps with instances, and equality orientation]

-- doLocalFunDeps does StartAgain if there
-- are any fundeps: see (DFL1) in Note [Do fundeps last]

tryDictFunDeps dict_ct
  = do { tryDictFunDepsLocal dict_ct
       ; tryDictFunDepsTop   dict_ct }

tryDictFunDepsLocal :: DictCt -> SolverStage ()
-- Using functional dependencies, interact the DictCt with the
-- inert Givens and Wanteds, to produce new equalities
tryDictFunDepsLocal dict_ct@(DictCt { di_cls = cls, di_ev = work_ev })
  | isGiven work_ev
  = -- If work_ev is Given, there could in principle be some inert Wanteds
    -- but in practice there never are because we solve Givens first
    nopStage ()

  | otherwise
  = Stage $
    do { inerts <- getInertCans

       ; traceTcS "tryDictFunDepsLocal {" (ppr dict_ct)

       ; let eqns :: [FunDepEqn]
             eqns = foldr ((++) . do_interaction) [] $
                    findDictsByClass (inert_dicts inerts) cls
       ; imp <- solveFunDeps work_ev eqns

       ; traceTcS "tryDictFunDepsLocal }" $
         text "imp =" <+> ppr imp $$ text "eqns = " <+> ppr eqns

       ; if imp then startAgainWith (CDictCan dict_ct)
                     -- See (DFL1) of Note [Do fundeps last]
                else continueWith () }
  where
    work_pred     = ctEvPred work_ev
    work_is_given = isGiven work_ev

    do_interaction :: DictCt -> [FunDepEqn]
    do_interaction (DictCt { di_ev = inert_ev }) -- This can be Given or Wanted
      | work_is_given && isGiven inert_ev
        -- Do not create FDs from Given/Given interactions
        -- See Note [No Given/Given fundeps]
        -- It is possible for work_ev to be Given when inert_ev is Wanted:
        -- this can happen if a Given is kicked out by a unification
      = []

      | otherwise
      = improveFromAnother (ctEvPred inert_ev) work_pred

tryDictFunDepsTop :: DictCt -> SolverStage ()
tryDictFunDepsTop dict_ct@(DictCt { di_ev = ev, di_cls = cls, di_tys = xis })
  = Stage $
    do { inst_envs <- getInstEnvs

       ; traceTcS "tryDictFunDepsTop {" (ppr dict_ct)
       ; let eqns :: [FunDepEqn]
             eqns = improveFromInstEnv inst_envs cls xis
       ; imp <- solveFunDeps ev eqns
       ; traceTcS "tryDictFunDepsTop }" (text "imp =" <+> ppr imp)

       ; if imp then startAgainWith (CDictCan dict_ct)
                else continueWith () }

{- Note [No Given/Given fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not create constraints from:
* Given/Given interactions via functional dependencies or type family
  injectivity annotations.
* Given/instance fundep interactions via functional dependencies or
  type family injectivity annotations.

In this Note, all these interactions are called just "fundeps".

We ingore such fundeps for several reasons:

1. These fundeps will never serve a purpose in accepting more
   programs: Given constraints do not contain metavariables that could
   be unified via exploring fundeps. They *could* be useful in
   discovering inaccessible code. However, the constraints will be
   Wanteds, and as such will cause errors (not just warnings) if they
   go unsolved. Maybe there is a clever way to get the right
   inaccessible code warnings, but the path forward is far from
   clear. #12466 has further commentary.

2. Furthermore, here is a case where a Given/instance interaction is actively
   harmful (from dependent/should_compile/RaeJobTalk):

       type family a == b :: Bool
       type family Not a = r | r -> a where
         Not False = True
         Not True  = False

       [G] Not (a == b) ~ True

   Reacting this Given with the equations for Not produces

      [W] a == b ~ False

   This is indeed a true consequence, and would make sense as a fresh Given.
   But we don't have a way to produce evidence for fundeps, as a Wanted it
   is /harmful/: we can't prove it, and so we'll report an error and reject
   the program. (Previously fundeps gave rise to Deriveds, which
   carried no evidence, so it didn't matter that they could not be proved.)

3. #20922 showed a subtle different problem with Given/instance fundeps.
      type family ZipCons (as :: [k]) (bssx :: [[k]]) = (r :: [[k]]) | r -> as bssx where
        ZipCons (a ': as) (bs ': bss) = (a ': bs) ': ZipCons as bss
        ...

      tclevel = 4
      [G] ZipCons is1 iss ~ (i : is2) : jss

   (The tclevel=4 means that this Given is at level 4.)  The fundep tells us that
   'iss' must be of form (is2 : beta[4]) where beta[4] is a fresh unification
   variable; we don't know what type it stands for. So we would emit
      [W] iss ~ is2 : beta

   Again we can't prove that equality; and worse we'll rewrite iss to
   (is2:beta) in deeply nested constraints inside this implication,
   where beta is untouchable (under other equality constraints), leading
   to other insoluble constraints.

The bottom line: since we have no evidence for them, we should ignore Given/Given
and Given/instance fundeps entirely.
-}



{-
**********************************************************************
*                                                                    *
    Functional dependencies for type families
*                                                                    *
**********************************************************************
-}

--------------------
tryEqFunDeps :: EqCt -> SolverStage ()
tryEqFunDeps work_item@(EqCt { eq_lhs = lhs, eq_eq_rel = eq_rel })
  | NomEq <- eq_rel
  , TyFamLHS tc args <- lhs
  = do { improveLocalFunEqs tc args work_item
       ; improveTopFunEqs   tc args work_item }
  | otherwise
  = nopStage ()

--------------------
improveTopFunEqs :: TyCon -> [TcType] -> EqCt -> SolverStage ()
-- TyCon is definitely a type family
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs fam_tc args eq_ct@(EqCt { eq_ev = ev, eq_rhs = rhs_ty })
  = Stage $
    do { imp <- if isGiven ev
                then improveGivenTopFunEqs  fam_tc args ev rhs_ty
                else improveWantedTopFunEqs fam_tc args ev rhs_ty
       ; if imp then startAgainWith (CEqCan eq_ct)
                else continueWith () }

improveGivenTopFunEqs :: TyCon -> [TcType] -> CtEvidence -> Xi -> TcS Bool
-- TyCon is definitely a type family
-- Work-item is a Given
improveGivenTopFunEqs fam_tc args ev rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = do { traceTcS "improveGivenTopFunEqs" (ppr fam_tc <+> ppr args $$ ppr ev $$ ppr rhs_ty)
       ; emitNewGivens (ctEvLoc ev) $
           [ (Nominal, new_co)
           | (ax, _) <- tryInteractTopFam ops fam_tc args rhs_ty
           , let new_co = mkAxiomCo ax [given_co] ]
       ; return False }  -- False: no unifications
  | otherwise
  = return False
  where
    given_co :: Coercion = ctEvCoercion ev

improveWantedTopFunEqs :: TyCon -> [TcType] -> CtEvidence -> Xi -> TcS Bool
-- TyCon is definitely a type family
-- Work-item is a Wanted
improveWantedTopFunEqs fam_tc args ev rhs_ty
  = do { fd_eqns <- improve_wanted_top_fun_eqs fam_tc args rhs_ty
       ; traceTcS "improveTopFunEqs" (vcat [ text "lhs:" <+> ppr fam_tc <+> ppr args
                                           , text "rhs:" <+> ppr rhs_ty
                                           , text "eqns:" <+> ppr fd_eqns ])
       ; solveFunDeps ev fd_eqns }

improve_wanted_top_fun_eqs :: TyCon -> [TcType] -> Xi -> TcS [FunDepEqn]
-- TyCon is definitely a type family
improve_wanted_top_fun_eqs fam_tc lhs_tys rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return [FDEqn { fd_qtvs = []
                  , fd_eqs = map snd $ tryInteractTopFam ops fam_tc lhs_tys rhs_ty }]

  -- ToDo: use ideas in #23162 for closed type families; injectivity only for open

  -- See Note [Type inference for type families with injectivity]
  -- Open, so look for inj
  | Injective inj_args <- tyConInjectivityInfo fam_tc
  = do { fam_envs <- getFamInstEnvs
       ; top_eqns <- improve_injective_wanted_top fam_envs inj_args fam_tc lhs_tys rhs_ty
       ; let local_eqns = improve_injective_wanted_famfam  inj_args fam_tc lhs_tys rhs_ty
       ; traceTcS "improve_wanted_top_fun_eqs" $
         vcat [ ppr fam_tc
              , text "local_eqns" <+> ppr local_eqns
              , text "top_eqns" <+> ppr top_eqns ]
              -- xxx ToDo: this does both local and top => bug?
       ; return (local_eqns ++ top_eqns) }

  | otherwise  -- No injectivity
  = return []

improve_injective_wanted_top :: FamInstEnvs -> [Bool] -> TyCon
                             -> [TcType] -> Xi -> TcS [FunDepEqn]
-- Interact with top-level instance declarations
-- See Section 5.2 in the Injective Type Families paper
-- The injectivity flags [Bool] will not all be False, but nothing goes wrong if they are
improve_injective_wanted_top fam_envs inj_args fam_tc lhs_tys rhs_ty
  = concatMapM do_one branches
  where
    branches :: [CoAxBranch]
    branches | isOpenTypeFamilyTyCon fam_tc
             , let fam_insts = lookupFamInstEnvByTyCon fam_envs fam_tc
             = concatMap (fromBranches . coAxiomBranches . fi_axiom) fam_insts

             | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
             = fromBranches (coAxiomBranches ax)

             | otherwise
             = []

    do_one :: CoAxBranch -> TcS [FunDepEqn]
    do_one branch@(CoAxBranch { cab_tvs = branch_tvs, cab_lhs = branch_lhs_tys, cab_rhs = branch_rhs })
      | let in_scope1 = in_scope `extendInScopeSetList` branch_tvs
      , Just subst <- tcUnifyTyForInjectivity False in_scope1 branch_rhs rhs_ty
                      -- False: matching, not unifying
      = do { let inSubst tv = tv `elemVarEnv` getTvSubstEnv subst
                 unsubstTvs = filterOut inSubst branch_tvs
                 -- The order of unsubstTvs is important; it must be
                 -- in telescope order e.g. (k:*) (a:k)

           ; (_subst_tvs, subst1) <- instFlexiX subst unsubstTvs
                -- If the current substitution bind [k -> *], and
                -- one of the un-substituted tyvars is (a::k), we'd better
                -- be sure to apply the current substitution to a's kind.
                -- Hence instFlexiX.   #13135 was an example.

           ; traceTcS "improve_inj_top" $
             vcat [ text "branch_rhs" <+> ppr branch_rhs
                  , text "rhs_ty" <+> ppr rhs_ty
                  , text "subst" <+> ppr subst
                  , text "subst1" <+> ppr subst1 ]
           ; let branch_lhs_tys' = substTys subst1 branch_lhs_tys
           ; if apartnessCheck branch_lhs_tys' branch
             then do { traceTcS "improv_inj_top1" (ppr branch_lhs_tys')
                     ; return [mkInjectivityFDEqn inj_args branch_lhs_tys' lhs_tys] }
                  -- NB: The fresh unification variables (from unsubstTvs) are on the left
                  --     See Note [Improvement orientation]
             else do { traceTcS "improve_inj_top2" empty; return []  } }
      | otherwise
      = do { traceTcS "improve_inj_top:fail" (ppr branch_rhs $$ ppr rhs_ty $$ ppr in_scope $$ ppr branch_tvs)
           ; return [] }

    in_scope = mkInScopeSet (tyCoVarsOfType rhs_ty)


improve_injective_wanted_famfam :: [Bool] -> TyCon -> [TcType] -> Xi -> [FunDepEqn]
-- Interact with itself, specifically  F s1 s2 ~ F t1 t2
-- The injectivity flags [Bool] will not all be False, but nothing goes wrong if they are
improve_injective_wanted_famfam inj_args fam_tc lhs_tys rhs_ty
  | Just (tc, rhs_tys) <- tcSplitTyConApp_maybe rhs_ty
  , tc == fam_tc
  = [mkInjectivityFDEqn inj_args lhs_tys rhs_tys]
  | otherwise
  = []

mkInjectivityFDEqn :: [Bool] -> [TcType] -> [TcType] -> FunDepEqn
-- When F s1 s2 s3 ~ F t1 t2 t3, and F has injectivity info [True,False,True]
--   return the FDEqn { fd_eqs = [Pair s1 t1, Pair s3 t3] }
-- The injectivity flags [Bool] will not all be False, but nothing goes wrong if they are
mkInjectivityFDEqn inj_args lhs_args rhs_args
  = FDEqn { fd_qtvs = [], fd_eqs = eqs }
  where
    eqs = [ Pair lhs_arg rhs_arg
          | (True, lhs_arg, rhs_arg) <- zip3 inj_args lhs_args rhs_args ]

---------------------------------------------
improveLocalFunEqs :: TyCon -> [TcType] -> EqCt   -- F args ~ rhs
                   -> SolverStage ()
-- Emit equalities from interaction between two equalities
improveLocalFunEqs fam_tc args eq_ct@(EqCt { eq_ev = work_ev, eq_rhs = rhs })
  = Stage $
    do { inerts <- getInertCans
       ; let my_funeqs = get_my_funeqs inerts
       ; imp <- if isGiven work_ev
                then improveGivenLocalFunEqs  my_funeqs fam_tc args work_ev rhs
                else improveWantedLocalFunEqs my_funeqs fam_tc args work_ev rhs
       ; if imp then startAgainWith (CEqCan eq_ct)
                else continueWith () }
  where
    get_my_funeqs :: InertCans -> [EqCt]   -- Mixture of Given and Wanted
    get_my_funeqs (IC { inert_funeqs = funeqs })
      = [ funeq_ct | equal_ct_list <- findFunEqsByTyCon funeqs fam_tc
                   , funeq_ct <- equal_ct_list
                   , NomEq == eq_eq_rel funeq_ct ]
                      -- Representational equalities don't interact
                      -- with type family dependencies

improveGivenLocalFunEqs :: [EqCt]    -- Inert items, mixture of Given and Wanted
                        -> TyCon -> [TcType] -> CtEvidence -> Xi  -- Work item (Given)
                        -> TcS Bool  -- Always False (no unifications)
-- Emit equalities from interaction between two Given type-family equalities
--    e.g.    (x+y1~z, x+y2~z) => (y1 ~ y2)
improveGivenLocalFunEqs funeqs_for_tc fam_tc work_args work_ev work_rhs
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = do { mapM_ (do_one ops) funeqs_for_tc
       ; return False }     -- False: no unifications
  | otherwise
  = return False
  where
    given_co :: Coercion = ctEvCoercion work_ev

    do_one :: BuiltInSynFamily -> EqCt -> TcS ()
    -- Used only work-item is Given
    do_one ops EqCt { eq_ev  = inert_ev, eq_lhs = inert_lhs, eq_rhs = inert_rhs }
      | isGiven inert_ev                    -- Given/Given interaction
      , TyFamLHS _ inert_args <- inert_lhs  -- Inert item is F inert_args ~ inert_rhs
      , work_rhs `tcEqType` inert_rhs       -- Both RHSs are the same
      , -- So we have work_ev  : F work_args  ~ rhs
        --            inert_ev : F inert_args ~ rhs
        let pairs :: [(CoAxiomRule, TypeEqn)]
            pairs = tryInteractInertFam ops fam_tc work_args inert_args
      , not (null pairs)
      = do { traceTcS "improveGivenLocalFunEqs" (vcat[ ppr fam_tc <+> ppr work_args
                                                     , text "work_ev" <+>  ppr work_ev
                                                     , text "inert_ev" <+> ppr inert_ev
                                                     , ppr work_rhs
                                                     , ppr pairs ])
           ; emitNewGivens (ctEvLoc inert_ev) (map mk_ax_co pairs) }
             -- This CtLoc for the new Givens doesn't reflect the
             -- fact that it's a combination of Givens, but I don't
             -- this that matters.
      where
        inert_co = ctEvCoercion inert_ev
        mk_ax_co (ax,_) = (Nominal, mkAxiomCo ax [combined_co])
          where
            combined_co = given_co `mkTransCo` mkSymCo inert_co
            -- given_co :: F work_args  ~ rhs
            -- inert_co :: F inert_args ~ rhs
            -- the_co :: F work_args ~ F inert_args

    do_one _  _ = return ()

improveWantedLocalFunEqs
    :: [EqCt]     -- Inert items (Given and Wanted)
    -> TyCon -> [TcType] -> CtEvidence -> Xi  -- Work item (Wanted)
    -> TcS Bool   -- True <=> some unifications
-- Emit improvement equalities for a Wanted constraint, by comparing
-- the current work item with inert CFunEqs (both Given and Wanted)
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [W] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
improveWantedLocalFunEqs funeqs_for_tc fam_tc args work_ev rhs
  = do { traceTcS "interactFunEq improvements: " $
                   vcat [ text "Eqns:" <+> ppr improvement_eqns
                        , text "Candidates:" <+> ppr funeqs_for_tc ]
       ; solveFunDeps work_ev improvement_eqns }
  where
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: [FunDepEqn]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmetic
        concatMap (do_one_built_in ops rhs) funeqs_for_tc

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        concatMap (do_one_injective injective_args rhs) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops rhs (EqCt { eq_lhs = TyFamLHS _ iargs, eq_rhs = irhs })
      | irhs `tcEqType` rhs
      = [FDEqn { fd_qtvs = [], fd_eqs = map snd $ tryInteractInertFam ops fam_tc args iargs }]
      | otherwise
      = []
    do_one_built_in _ _ _ = pprPanic "interactFunEq 1" (ppr fam_tc) -- TyVarLHS

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args rhs (EqCt { eq_lhs = TyFamLHS _ inert_args, eq_rhs = irhs })
      | rhs `tcEqType` irhs
      = [mkInjectivityFDEqn inj_args args inert_args]
      | otherwise
      = []

    do_one_injective _ _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)  -- TyVarLHS


{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have an equality like F s1 t1 ~ F s2 t2,
we can use the injectivity to get a new Wanted constraint on
the injective argument
  [W] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and inferring values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are Wanted constraints, but we will not use the evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

We generate these Wanteds in three places, depending on how we notice the
injectivity.

1. When we have a [W] F tys1 ~ F tys2. This is handled in canEqCanLHS2, and
described in Note [Decomposing type family applications] in GHC.Tc.Solver.Equality

2. When we have [W] F tys1 ~ T and [W] F tys2 ~ T. Note that neither of these
constraints rewrites the other, as they have different LHSs. This is done
in improveLocalFunEqs, called during the interactWithInertsStage.

3. When we have [W] F tys ~ T and an equation for F that looks like F tys' = T.
This is done in improve_top_fun_eqs, called from the top-level reactions stage.

See also Note [Injective type families] in GHC.Core.TyCon

Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.
-}

{- *********************************************************************
*                                                                      *
              Emitting equalities arising from fundeps
*                                                                      *
********************************************************************* -}

solveFunDeps :: CtEvidence  -- The work item
             -> [FunDepEqn]
             -> TcS Bool
-- Solve a bunch of type-equality equations, generated by functional dependencies
-- By "solve" we mean: (only) do unifications.  We do not generate evidence, and
-- other than unifications there should be no effects whatsoever
--
-- The returned Bool is True if some unifications happened
--
-- See Note [Overview of fundeps]
solveFunDeps work_ev fd_eqns
  | null fd_eqns
  = return False -- Common case no-op

  | otherwise
  = do { (unif_happened, _res)
             <- reportUnifications $
                nestFunDepsTcS     $
                do { (_, eqs) <- wrapUnifier work_ev Nominal do_fundeps
                   ; solveSimpleWanteds eqs }
    -- ToDo: why solveSimpleWanteds?  Answer
    --     (a) don't rely on eager unifier
    --     (b) F Int alpha ~ Maybe Int   where  type instance F Int x = Maybe x

       ; return unif_happened }
  where
    do_fundeps :: UnifyEnv -> TcM ()
    do_fundeps env = mapM_ (do_one env) fd_eqns

    do_one :: UnifyEnv -> FunDepEqn -> TcM ()
    do_one uenv eqn = do { eqs <- instantiateFunDepEqn eqn
                         ; uPairsTcM uenv eqs }

instantiateFunDepEqn :: FunDepEqn -> TcM [TypeEqn]
instantiateFunDepEqn (FDEqn { fd_qtvs = tvs, fd_eqs = eqs })
  | null tvs
  = return rev_eqs
  | otherwise
  = do { TcM.traceTc "emitFunDepWanteds 2" (ppr tvs $$ ppr eqs)
       ; (_, subst) <- instFlexiXTcM emptySubst tvs  -- Takes account of kind substitution
       ; return (map (subst_pair subst) rev_eqs) }
  where
    rev_eqs = reverse eqs
       -- (reverse eqs): See Note [Reverse order of fundep equations]
       -- ToDo: is this still a problem?

    subst_pair subst (Pair ty1 ty2)
       = Pair (substTyUnchecked subst' ty1) ty2
              -- ty2 does not mention fd_qtvs, so no need to subst it.
              -- See GHC.Tc.Instance.Fundeps Note [Improving against instances]
              --     Wrinkle (1)
       where
         subst' = extendSubstInScopeSet subst (tyCoVarsOfType ty1)
                  -- The free vars of ty1 aren't just fd_qtvs: ty1 is the result
                  -- of matching with the [W] constraint. So we add its free
                  -- vars to InScopeSet, to satisfy substTy's invariants, even
                  -- though ty1 will never (currently) be a poytype, so this
                  -- InScopeSet will never be looked at.


{- Note [Reverse order of fundep equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this scenario (from dependent/should_fail/T13135_simple):

  type Sig :: Type -> Type
  data Sig a = SigFun a (Sig a)

  type SmartFun :: forall (t :: Type). Sig t -> Type
  type family SmartFun sig = r | r -> sig where
    SmartFun @Type (SigFun @Type a sig) = a -> SmartFun @Type sig

  [W] SmartFun @kappa sigma ~ (Int -> Bool)

The injectivity of SmartFun allows us to produce two new equalities:

  [W] w1 :: Type ~ kappa
  [W] w2 :: SigFun @Type Int beta ~ sigma

for some fresh (beta :: SigType). The second Wanted here is actually
heterogeneous: the LHS has type Sig Type while the RHS has type Sig kappa.
Of course, if we solve the first wanted first, the second becomes homogeneous.

When looking for injectivity-inspired equalities, we work left-to-right,
producing the two equalities in the order written above. However, these
equalities are then passed into wrapUnifierAndEmit, which will fail, adding these
to the work list. However, the work list operates like a *stack*.
So, because we add w1 and then w2, we process w2 first. This is silly: solving
w1 would unlock w2. So we make sure to add equalities to the work
list in left-to-right order, which requires a few key calls to 'reverse'.

When this was originally conceived, it was necessary to avoid a loop in T13135.
That loop is now avoided by continuing with the kind equality (not the type
equality) in canEqCanLHSHetero (see Note [Equalities with heterogeneous kinds]).
However, the idea of working left-to-right still seems worthwhile, and so the calls
to 'reverse' remain.

This treatment is also used for class-based functional dependencies, although
we do not have a program yet known to exhibit a loop there. It just seems
like the right thing to do.

In general, I believe this is (now, anyway) just an optimisation, not required
to avoid loops.
-}

{- *********************************************************************
*                                                                      *
                 Historical notes

     Here are a bunch of Notes that are rendered obselete by
          Note [Partial functional dependencies]

*                                                                      *
********************************************************************* -}

{-
Historical Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Fundeps with instances, and equality orientation], which describes
the Exact Same Problem, with the same solution, but for functional dependencies.

A very delicate point is the orientation of equalities
arising from injectivity improvement (#12522).  Suppose we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] F (alpha, beta) ~ (Int, <some type>)

The injectivity will give rise to constraints

  [W] gamma1 ~ alpha
  [W] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [W] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we substitute it back in, it'd look like
   [W] F (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

--->  ToDo: all this fragility has gone away!   Fix the Note! <---

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new
equality with the template on the left.  Delicate, but it works.

Historical Note [Fundeps with instances, and equality orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes a delicate interaction that constrains the orientation of
equalities. This one is about fundeps, but the /exact/ same thing arises for
type-family injectivity constraints: see Note [Improvement orientation].

doTopFunDepImprovement compares the constraint with all the instance
declarations, to see if we can produce any equalities. E.g
   class C2 a b | a -> b
   instance C Int Bool
Then the constraint (C Int ty) generates the equality [W] ty ~ Bool.

There is a nasty corner in #19415 which led to the typechecker looping:
   class C s t b | s -> t
   instance ... => C (T kx x) (T ky y) Int
   T :: forall k. k -> Type

   work_item: dwrk :: C (T @ka (a::ka)) (T @kb0 (b0::kb0)) Char
      where kb0, b0 are unification vars

   ==> {doTopFunDepImprovement: compare work_item with instance,
        generate /fresh/ unification variables kfresh0, yfresh0,
        emit a new Wanted, and add dwrk to inert set}

   Suppose we emit this new Wanted from the fundep:
       [W] T kb0 (b0::kb0) ~ T kfresh0 (yfresh0::kfresh0)

   ==> {solve that equality kb0 := kfresh0, b0 := yfresh0}
   Now kick out dwrk, since it mentions kb0
   But now we are back to the start!  Loop!

NB1: This example relies on an instance that does not satisfy the
     coverage condition (although it may satisfy the weak coverage
     condition), and hence whose fundeps generate fresh unification
     variables.  Not satisfying the coverage condition is known to
     lead to termination trouble, but in this case it's plain silly.

NB2: In this example, the third parameter to C ensures that the
     instance doesn't actually match the Wanted, so we can't use it to
     solve the Wanted

We solve the problem by (#21703):

    carefully orienting the new Wanted so that all the
    freshly-generated unification variables are on the LHS.

    Thus we call unifyWanteds on
       T kfresh0 (yfresh0::kfresh0) ~ T kb0 (b0::kb0)
    and /NOT/
       T kb0 (b0::kb0) ~ T kfresh0 (yfresh0::kfresh0)

Now we'll unify kfresh0:=kb0, yfresh0:=b0, and all is well.  The general idea
is that we want to preferentially eliminate those freshly-generated
unification variables, rather than unifying older variables, which causes
kick-out etc.

Keeping younger variables on the left also gives very minor improvement in
the compiler performance by having less kick-outs and allocations (-0.1% on
average).  Indeed Historical Note [Eliminate younger unification variables]
in GHC.Tc.Utils.Unify describes an earlier attempt to do so systematically,
apparently now in abeyance.

But this is is a delicate solution. We must take care to /preserve/
orientation during solving. Wrinkles:

(W1) We start with
       [W] T kfresh0 (yfresh0::kfresh0) ~ T kb0 (b0::kb0)
     Decompose to
       [W] kfresh0 ~ kb0
       [W] (yfresh0::kfresh0) ~ (b0::kb0)
     Preserve orientation when decomposing!!

(W2) Suppose we happen to tackle the second Wanted from (W1)
     first. Then in canEqCanLHSHetero we emit a /kind/ equality, as
     well as a now-homogeneous type equality
       [W] kco : kfresh0 ~ kb0
       [W] (yfresh0::kfresh0) ~ (b0::kb0) |> (sym kco)
     Preserve orientation in canEqCanLHSHetero!!  (Failing to
     preserve orientation here was the immediate cause of #21703.)

(W3) There is a potential interaction with the swapping done by
     GHC.Tc.Utils.Unify.swapOverTyVars.  We think it's fine, but it's
     a slight worry.  See especially Note [TyVar/TyVar orientation] in
     that module.

The trouble is that "preserving orientation" is a rather global invariant,
and sometimes we definitely do want to swap (e.g. Int ~ alpha), so we don't
even have a precise statement of what the invariant is.  The advantage
of the preserve-orientation plan is that it is extremely cheap to implement,
and apparently works beautifully.

--- Alternative plan (1) ---
Rather than have an ill-defined invariant, another possiblity is to
elminate those fresh unification variables at birth, when generating
the new fundep-inspired equalities.

The key idea is to call `instFlexiX` in `emitFunDepWanteds` on only those
type variables that are guaranteed to give us some progress. This means we
have to locally (without calling emitWanteds) identify the type variables
that do not give us any progress.  In the above example, we _know_ that
emitting the two wanteds `kco` and `co` is fruitless.

  Q: How do we identify such no-ops?

  1. Generate a matching substitution from LHS to RHS
        ɸ = [kb0 :-> k0, b0 :->  y0]
  2. Call `instFlexiX` on only those type variables that do not appear in the domain of ɸ
        ɸ' = instFlexiX ɸ (tvs - domain ɸ)
  3. Apply ɸ' on LHS and then call emitWanteds
        unifyWanteds ... (subst ɸ' LHS) RHS

Why will this work?  The matching substitution ɸ will be a best effort
substitution that gives us all the easy solutions. It can be generated with
modified version of `Core/Unify.unify_tys` where we run it in a matching mode
and never generate `SurelyApart` and always return a `MaybeApart Subst`
instead.

The same alternative plan would work for type-family injectivity constraints:
see Note [Improvement orientation] in GHC.Tc.Solver.Equality.
--- End of Alternative plan (1) ---

--- Alternative plan (2) ---
We could have a new flavour of TcTyVar (like `TauTv`, `TyVarTv` etc; see GHC.Tc.Utils.TcType.MetaInfo)
for the fresh unification variables introduced by functional dependencies.  Say `FunDepTv`.  Then in
GHC.Tc.Utils.Unify.swapOverTyVars we could arrange to keep a `FunDepTv` on the left if possible.
Looks possible, but it's one more complication.
--- End of Alternative plan (2) ---


--- Historical note: Failed Alternative Plan (3) ---
Previously we used a flag `cc_fundeps` in `CDictCan`. It would flip to False
once we used a fun dep to hint the solver to break and to stop emitting more
wanteds.  This solution was not complete, and caused a failures while trying
to solve for transitive functional dependencies (test case: T21703)
-- End of Historical note: Failed Alternative Plan (3) --


Historical Note
~~~~~~~~~~~~~~~
This Note (anonymous, but related to dict-solving) is rendered obselete by
 - Danger 1: solved by Note [Instance and Given overlap]
 - Danger 2: solved by fundeps being idempotent

When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
           situations (when a Given overlaps with an instance) that
           may produce new insoluble goals: see #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

-}
