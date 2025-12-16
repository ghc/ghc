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
import GHC.Tc.Solver.Types
import GHC.Tc.Solver.Monad   as TcS
import GHC.Tc.Utils.Monad    as TcM
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify( UnifyEnv(..) )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint

import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.TyCo.Rep( Type(..) )
import GHC.Core.Predicate( EqRel(..) )
import GHC.Core.TyCon
import GHC.Core.Unify( tcUnifyTysForInjectivity, typeListsAreApart )
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCo.Subst( elemSubst )

import GHC.Builtin.Types.Literals( tryInteractTopFam, tryInteractInertFam )

import GHC.Types.Var.Set

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad( unless )
import GHC.Data.Pair
import Data.Maybe( isNothing, isJust, mapMaybe )


{- Note [Overview of functional dependencies in type inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is our plan for dealing with functional dependencies

* When we have failed to solve a Wanted constraint, do this
  - (GEN-FD) Generate any fundep-equalities [FunDepEqns] from that constraint.
  - (SOLVE-FD) Try to solve that [FunDepEqns]
  - (KICK-FD) If any unifications happened,
       * kick out any inert constraints that mention the unified variables
       * send the current constraint back to the start of the pipeline;
         it might now be soluble, and it probably isn't inert


* Once a unification is done, it can't be un-done.  So we must be very careful
  not to threaten completeness by doing a unification when there might be
  another way to solve the constraint.

  Hence Principle (FUNDEP-COMPLETENESS):
    We generate a fundep-equality from a Wanted constraint only if the /sole/
    way to solve that constraint is for that equality to hold.

* (GEN-FD) How we generate those [FunDepEqns] varies:
       - tryDictFunDeps: for class constraints (C t1 .. tn)
         we look at top-level instances and inert Givens
       - tryEqFunDeps: for type-family equalities (F t1 .. tn ~ ty)
         we look at top-level family instances
                    and inert Given family equalities

* (SOLVE-FD) We use `solveFunDeps` to solve the [FunDepEqns] in a nested
  solver.  Key property:

      The ONLY effect of `solveFunDeps` is possibly to perform unifications:
      - It entirely discards any unsolved fundep equalities.
      - It entirely discards any evidence arising from solving fundep equalities

* (KICK-FD) if we did any unifications in (SOLVE-FD), we start again with the
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

Wrinkles

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

(FD2) We discard all evidence in Step 2.  We could go further and offer evidence
  from fundeps, but that would require new evidence forms, and an extension to
  FC, so we don't do that right now (Dec 14).


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

Note [Deeper TcLevel for partial improvement unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Why?  Because if we do (NO) we'll think we have made some progress
(some unification has happened), and hence go round again; but actually all we
have done is to replace `alpha` with `gamma1`.

These "fresh unification variables" in fundep-equalities are ubiquitous.
For example
      class C a b | a -> b
      instance .. => C Int [x]
If we see
      [W] C Int alpha
we'll generate a fundep-equality   [W] alpha ~ [beta1]
where `beta1` is one of those "fresh unification variables"

This problem shows up in several guises; see (at the bottom)
  * Historical Note [Improvement orientation]
  * Historical Note [Fundeps with instances, and equality orientation]

The solution is super-simple:

  * A fundep-equality is described by `FunDepEqns`, whose `fd_qtvs` field
    explicitly lists the "fresh variables"

  * Function `instantiateFunDepEqn` instantiates a `FunDepEqns`, and CRUCIALLY
    (via `nestFunDepsTcS` gives the new unification variables a level one
    deeper than the current level.

  * Now, given `alpha ~ beta`, all the unification machinery guarantees, to
    unify the variable with the deeper level.  See GHC.Tc.Utils.Unify
    Note [Deeper level on the left].  That ensures that the fresh `gamma1`
    will be eliminated in favour of `alpha`. Hooray.

  * Better still, we solve the [FunDepEqns] with
      solveFunDeps :: CtEvidence -> [FunDepEqns] -> TcS Bool
    It uses `reportFineGrainUnifications` to see if any unification happened at this
    level or outside -- that is, it does NOT report unifications to the fresh
    unification variables.  So `solveFunDeps` returns True only if it
    unifies a variable /other than/ the fresh ones.  Bingo.

Another victory for levels numbers!

Note [Do local fundeps before top-level instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider #14745
   class C a b c | a -> b c

   instance C Int Bool Char

   f :: (C Int b c) => a -> c
   f = undefined

From the ambiguity check for `f` we get
  [G] d1: C Int b c
  [W] d2: C Int beta c

This is easily solved by beta:=b, from the fundep between Given and Wanted.
But if we tried the top-level instance first, we'd get
   beta~Int, c~Char
which we can't solve. We'll ignore the insoluble c~Char, but we will still
have unified beta:=Int, leaving [W] C Int c, which we can't solve.

Conclusion: for fundeps, interact with local dictionaries before top-level instances.

Something very similar happens for type families.  See #13651, and test T13651.

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

  #14745 is another example. And #13651.

(DFL2) Try solving from top-level instances before fundeps.
  From the definition `foo = op` we get
    [G] FD Int b
    [W] FD Int Bool
  We solve this from the top level instance before even trying fundeps.
  If we did try fundeps, we'd generate [W] b ~ Bool, which fails.
  That doesn't matter -- failing fundep equalities are discarded -- but it's
  a waste of effort.

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

Note [Insoluble fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~
The pattern-match overlap checker uses the constraint solver
to find definitely-insoluble (aka inconsistent) constraints;
see `GHC.Tc.Solver.tcCheckGivens.`

But that insolubility could show up via a fundep (#22652).  Consider
    type family F a where
      F Int = Bool  -- (F1)
      F Char = Int  -- (F2)
and
   [G] F Bool ~ Bool

The type-family is closed, so the only way to make a Bool is via (F1),
so we know that the original constraint is insoluble.

Knowing this is good because
  * Marking the constraint as insoluble means we we'll put it in the Irreds,
    and won't use it to (confusingly) "help" solve other constraints.
  * Detecting insolubilitly is crucial for patterm-match overlap checking.


The moving parts are:

  * `solveFunDeps` checks to see if the residual unsolved fundep
    equalities are insoluble, and returns a boolean to say

  * All the callers of `solveFunDeps` check this insolubility flag
      - when doing fundeps on dictionaries
      - when doing fundeps on type-family equalities

  * When we detect insolubility, `insolubleFunDep`
      - Adds the constraint to the inert set as a CIrredCan,
        with a CtIrredReason of InsolubleFunDepReason.
      - Returns Stop from the Stage

Wrinkles:

(IFD0) In `mkTopClosedFamEqFDs, if there are no relevant equations, the equality
    can't be solved, so we can call `insolubleFunDep` there too.

(IFD1) Usually we don't generate fundeps for Givens type-family equalities
  (except for built-in type families, see (INJFAM:Given)), because fundeps
  don't generate evidence.  BUT when doing /pattern-match overlap checking/
  we DO want to generate fundeps so that we can see if they are insoluble.

  So we have a rather ad-hoc check in `tryFamEqFunDeps` for this.

(IFD2) During error reporting, don't want to say "Could not deduce X from Y"
  if the constraint X is outright insoluble becuase of /top-level/ equations.
  Then the Y part is just distracting.  But we /do/ want to report the Y part
  if insolublity comes from /local/ constraints.  Consider
     [G] IP "x" Int
     [W] IP "x" String
  This generates the insoluble Int~String, but we don't want to say that
  ?x::String is outright insoluble, only that we can't solve it from ?x::Int.

  Hence the Bool parameter to InsolubleFunDepReason:

    True <=>  Insolubility from top-level equations only
              e.g. type family F a where
                      F Int = Char
                   [W] F Bool ~ Char   -- Definitely insoluble

    False <=> Insolubility from /local/ constraints
              e.g. [G] ?x::Int
                   [W] ?x::String
              We get an insoluble Int~String

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

-- doLocalFunDeps does StartAgain if there
-- are any fundeps: see (DFL1) in Note [Do fundeps last]

tryDictFunDeps dict_ct
  = do { -- Note [Do local fundeps before top-level instances]
         tryDictFunDepsLocal dict_ct
       ; tryDictFunDepsTop dict_ct }

tryDictFunDepsLocal :: DictCt -> SolverStage ()
-- Using functional dependencies, interact the DictCt with the
-- inert Givens and Wanteds, to produce new equalities
-- Returns True if the fundeps are insoluble
tryDictFunDepsLocal dict_ct@(DictCt { di_cls = cls, di_ev = work_ev })
  | isGiven work_ev
  = -- If work_ev is Given, there could in principle be some inert Wanteds
    -- but in practice there never are because we solve Givens first
    nopStage ()

  | otherwise
  = Stage $
    do { inerts <- getInertCans

       ; traceTcS "tryDictFunDepsLocal {" (ppr dict_ct)

       ; let eqns :: [FunDepEqns]
             eqns = foldr ((++) . do_interaction) [] $
                    findDictsByClass (inert_dicts inerts) cls
       ; (insoluble, unif_happened) <- solveFunDeps work_ev eqns

       ; traceTcS "tryDictFunDepsLocal }" $
         text "unif =" <+> ppr unif_happened $$ text "eqns = " <+> ppr eqns

       -- See (DFL1) of Note [Do fundeps last]
       ; if | unif_happened -> startAgainWith (CDictCan dict_ct)
            | insoluble     -> insolubleFunDep False work_ev
            | otherwise     -> continueWith () }
  where
    work_pred     = ctEvPred work_ev
    work_is_given = isGiven work_ev

    do_interaction :: DictCt -> [FunDepEqns]
    do_interaction (DictCt { di_ev = inert_ev }) -- This can be Given or Wanted
      | work_is_given && isGiven inert_ev
        -- Do not create FDs from Given/Given interactions
        -- See Note [No Given/Given fundeps]
        -- It is possible for work_ev to be Given when inert_ev is Wanted:
        -- this can happen if a Given is kicked out by a unification
      = []

      | otherwise
      = improveFromAnother (ctEvPred inert_ev) work_pred

insolubleFunDep :: Bool -> CtEvidence -> TcS (StopOrContinue a)
-- The fundeps generated an insoluble constraint.
-- Stop solving with an inert (insoluble) CIrredCan
-- It's valuable to flag such constraints as insoluble because that improves
-- pattern-match overlap checking; see Note [Insoluble fundeps]
--
-- For the `is_top` parameter see (IFD2) in Note [Insoluble fundeps]
insolubleFunDep is_top ev
  = do { updInertIrreds irred_ct
       ; stopWith ev "Insoluble fundep" }
  where
    irred_ct = IrredCt { ir_ev = ev, ir_reason = InsolubleFunDepReason is_top }

tryDictFunDepsTop :: DictCt -> SolverStage ()
tryDictFunDepsTop dict_ct@(DictCt { di_ev = ev, di_cls = cls, di_tys = xis })
  = Stage $
    do { inst_envs <- getInstEnvs

       ; traceTcS "tryDictFunDepsTop {" (ppr dict_ct)
       ; let eqns :: [FunDepEqns]
             eqns = improveFromInstEnv inst_envs cls xis
       ; (insoluble, unif_happened) <- solveFunDeps ev eqns
       ; traceTcS "tryDictFunDepsTop }" (text "unif =" <+> ppr unif_happened)

       ; if | unif_happened -> startAgainWith (CDictCan dict_ct)
            | insoluble     -> insolubleFunDep True ev
            | otherwise     -> continueWith () }

{- Note [No Given/Given fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For /non-built-in/ type families we do not create constraints from:
* Given/Given interactions via functional dependencies or type-family
  injectivity annotations.
* Given/instance fundep interactions via functional dependencies or
  type family injectivity annotations.

NB: for /built-in type families/ we DO create constraints,
    because we can make evidence for them.
    See Note [Given/Given fundeps for built-in type families].

In this Note, all these interactions are called just "fundeps".

We ignore such fundeps for several reasons:

1. These fundeps will never serve a purpose in accepting more
   programs: Given constraints do not contain metavariables that could
   be unified via exploring fundeps. They *could* be useful in
   discovering inaccessible code. However, the constraints will be
   Wanteds, and as such will cause errors (not just warnings) if they
   go unsolved. Maybe there is a clever way to get the right
   inaccessible code warnings, but the path forward is far from
   clear. #12466 has further commentary.

   NB: for built-in type families we can do a lot better.
   See Note [Given/Given fundeps for built-in type families]

2. Furthermore, here is a case where a Given/instance interaction is actively
   harmful (from dependent/should_compile/RaeJobTalk):

       type family a == b :: Bool
       type family Not a = r | r -> a where
         Not False = True
         Not True  = False

       [G] Not (a == b) ~ True

   Reacting this Given with the equations for Not could produce
      [W] a == b ~ False
   This is indeed a true consequence, and would make sense as a fresh Given.
   But we don't have a way to produce evidence for fundeps, as a Wanted it
   is /useless/.

   (Historical aside: we used to keep fundep-generated Wanteds around, so
   this insoluble constraint would generate a (misleading) error message.
   Nowadays we discard unsolved fundeps. End of historial aside.)

3. #20922 showed a subtle different problem with Given/instance fundeps.
      type family ZipCons (as :: [k]) (bssx :: [[k]]) = (r :: [[k]]) | r -> as bssx where
        ZipCons (a ': as) (bs ': bss) = (a ': bs) ': ZipCons as bss
        ...

      tclevel = 4
      [G] ZipCons is1 iss ~ (i : is2) : jss

   (The tclevel=4 means that this Given is at level 4.)  The fundep tells us that
   'iss' must be of form (is2 : beta[4]) where beta[4] is a fresh unification
   variable; we don't know what type it stands for. So we would emit
      [W] iss ~ is2 : beta[4]

   Again we can't prove that equality. (Historical aside: in the past
   we used to keep fundep Wanteds around, and then it'll rewrite `iss` to
   (is2:beta) in deeply nested constraints inside this implication,
   where beta is untouchable (under other equality constraints), leading
   to other insoluble constraints.  End of historical aside.)

The bottom line:
   since we have no evidence for them, we should ignore
   Given/Given and Given/instance fundeps entirely, for
     * Type-class fundeps
     * Fundeps for user-defined type families

Note [Given/Given fundeps for built-in type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For built-in type families we /can/ generate real evidence from Given/Given
or Given/instance interactions.  For example if we have
   [G] x+3 ~ 7
then we can deduce
   [G] x ~ 4
Or
   [G] x++[3] ~ [5,3]
we can deduce
   [] x ~ [5]

This new Given evidence is generated by `tryGivenBuiltinFamEqFDs` supported
by the extensive code in GHC.Builtin.Types.Literals.
-}



{-
**********************************************************************
*                                                                    *
    Functional dependencies for type families
*                                                                    *
**********************************************************************
-}

tryEqFunDeps :: EqCt -> SolverStage ()
tryEqFunDeps work_item@(EqCt { eq_lhs = work_lhs
                             , eq_rhs = work_rhs
                             , eq_eq_rel = eq_rel })
  | NomEq <- eq_rel   -- Functional dependencies only work for nominal equalities
  , TyFamLHS fam_tc work_args <- work_lhs     -- We have F args ~N# rhs
  = do { eqs_for_me <- simpleStage $ getInertFamEqsFor fam_tc work_args work_rhs
       ; simpleStage $ traceTcS "tryEqFunDeps" (ppr work_item $$ ppr eqs_for_me)
       ; mode <- simpleStage getTcSMode
       ; tryFamEqFunDeps mode eqs_for_me fam_tc work_args work_item }
  | otherwise
  = nopStage ()


tryFamEqFunDeps :: TcSMode -> [EqCt] -> TyCon -> [TcType] -> EqCt -> SolverStage ()
tryFamEqFunDeps mode eqs_for_me fam_tc work_args
                work_item@(EqCt { eq_ev = ev, eq_rhs = work_rhs })
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = if isGiven ev
    then tryGivenBuiltinFamEqFDs eqs_for_me fam_tc ops work_args work_item
    else do { -- Note [Do local fundeps before top-level instances]
              eqns <- mkLocalBuiltinFamEqFDs eqs_for_me fam_tc ops work_args work_rhs
            ; tryFDEqns False fam_tc work_args work_item eqns

            ; unless (hasRelevantGiven eqs_for_me work_args work_item) $
              do { eqns <- mkTopBuiltinFamEqFDs fam_tc ops work_args work_rhs
                 ; tryFDEqns True fam_tc work_args work_item eqns } }

  | isGiven ev  -- See (INJFAM:Given)
  , not (tcsmResumable mode)   -- In the pattern-match checker, continue even for
  = nopStage ()                -- Givens in the hope of discovering insolubility
                               -- See (IFD1) in Note [Insoluble fundeps]

  -- Only Wanted constraints below here

  | otherwise   -- Wanted, user-defined type families
  = do { -- Note [Do local fundeps before top-level instances]
         case tyConInjectivityInfo fam_tc of
           NotInjective  -> nopStage ()
           Injective inj -> do { eqns <- mkLocalFamEqFDs eqs_for_me fam_tc inj work_args work_rhs
                               ; tryFDEqns False fam_tc work_args work_item eqns }

       ; unless (hasRelevantGiven eqs_for_me work_args work_item) $
         do { eqns <- mkTopFamEqFDs fam_tc work_args work_item
            ; tryFDEqns True fam_tc work_args work_item eqns } }

mkTopFamEqFDs :: TyCon -> [TcType] -> EqCt -> SolverStage [FunDepEqns]
mkTopFamEqFDs fam_tc work_args work_item
  | isOpenTypeFamilyTyCon fam_tc
  , Injective inj_flags <- tyConInjectivityInfo fam_tc
  = -- Open, injective type families
    simpleStage (mkTopOpenFamEqFDs fam_tc inj_flags work_args work_item)

  | Just ax <- isClosedFamilyTyCon_maybe fam_tc
  = -- Closed type families
    mkTopClosedFamEqFDs ax work_args work_item

  | otherwise
  = -- Data families, abstract families,
    -- open families that are not injective,
    -- closed type families with no equations (isClosedFamilyTyCon_maybe returns Nothing)
    return []

tryFDEqns :: Bool -> TyCon -> [TcType] -> EqCt -> [FunDepEqns] -> SolverStage ()
tryFDEqns is_top fam_tc work_args work_item@(EqCt { eq_ev = ev, eq_rhs= rhs }) fd_eqns
  = Stage $
    do { traceTcS "tryFDEqns" (vcat [ text "lhs:" <+> ppr fam_tc <+> ppr work_args
                                    , text "rhs:" <+> ppr rhs
                                    , text "eqns:" <+> ppr fd_eqns ])

       ; (insoluble, unif_happened) <- solveFunDeps ev fd_eqns

       ; if | unif_happened -> startAgainWith (CEqCan work_item)
            | insoluble     -> insolubleFunDep is_top ev
            | otherwise     -> continueWith () }

-----------------------------------------
--  User-defined type families
-----------------------------------------
mkTopClosedFamEqFDs :: CoAxiom Branched -> [TcType] -> EqCt -> SolverStage [FunDepEqns]
-- Look at the top-level axioms; we effectively infer injectivity,
-- so we don't need tyConInjectivtyInfo.  This works fine for closed
-- type families without injectivity info
-- See Note [Exploiting closed type families]
mkTopClosedFamEqFDs ax work_args (EqCt { eq_ev = ev, eq_rhs = work_rhs })
  | isGenerativeType work_rhs -- See (CF5) in Note [Exploiting closed type families]
  = Stage $
    do { let branches = fromBranches (coAxiomBranches ax)
       ; traceTcS "mkTopClosed" (ppr branches $$ ppr work_args $$ ppr work_rhs)
       ; case getRelevantBranches ax work_args work_rhs of
           []    -> insolubleFunDep True ev  -- See (IFD0) in Note [Insoluble fundeps]
           [eqn] -> continueWith [eqn]       -- If there is just one relevant equation, use it
           _     -> continueWith [] }
  | otherwise
  = Stage $ continueWith []

isGenerativeType :: Type -> Bool
-- True <=> This type cannot rewrite to, or be substituted to,
--          a saturated type-family application
-- See (CF5) in Note [Exploiting closed type families]
isGenerativeType ty | Just ty' <- coreView ty = isGenerativeType ty'
isGenerativeType (FunTy {})      = True
isGenerativeType (CastTy ty _)   = isGenerativeType ty
isGenerativeType (ForAllTy {})   = True
isGenerativeType (TyConApp tc _) = isGenerativeTyCon tc Nominal
isGenerativeType (AppTy {})      = True
isGenerativeType (LitTy {})      = True
isGenerativeType (TyVarTy {})    = False
isGenerativeType (CoercionTy {}) = False

hasRelevantGiven :: [EqCt] -> [TcType] -> EqCt -> Bool
-- See (CF1) in Note [Exploiting closed type families]
-- A Given is relevant if it is not apart from the Wanted
hasRelevantGiven eqs_for_me work_args (EqCt { eq_rhs = work_rhs })
  = any relevant eqs_for_me
  where
    work_tys = work_rhs : work_args

    relevant (EqCt { eq_ev = ev, eq_lhs = lhs, eq_rhs = rhs_ty })
       | isGiven ev
       , TyFamLHS _ lhs_tys <- lhs
       = isJust (tcUnifyTysForInjectivity True work_tys (rhs_ty:lhs_tys))
       | otherwise
       = False

getRelevantBranches :: CoAxiom Branched -> [TcType] -> Xi -> [FunDepEqns]
-- Return the FunDepEqns that arise from each relevant branch
getRelevantBranches ax work_args work_rhs
  = go [] (fromBranches (coAxiomBranches ax))
  where
    work_tys = work_rhs : work_args

    go _ [] = []
    go preceding (branch:branches)
      = case is_relevant branch of
          Just eqn -> eqn : go (branch:preceding) branches
          Nothing  ->       go (branch:preceding) branches
      where
         is_relevant (CoAxBranch { cab_tvs = qtvs, cab_lhs = lhs_tys, cab_rhs = rhs_ty })
            | Just subst <- tcUnifyTysForInjectivity True (rhs_ty:lhs_tys) work_tys
            , let (subst', qtvs') = trim_qtvs subst qtvs
                  lhs_tys' = substTys subst' lhs_tys
                  rhs_ty'  = substTy  subst' rhs_ty
            , all (no_match lhs_tys') preceding
            = Just (FDEqns { fd_qtvs = qtvs'
                           , fd_eqs = zipWith Pair (rhs_ty':lhs_tys') work_tys })
            | otherwise
            = Nothing

         no_match lhs_tys (CoAxBranch { cab_lhs = lhs_tys1 })
            = isNothing (tcUnifyTysForInjectivity False lhs_tys1 lhs_tys)

mkTopOpenFamEqFDs :: TyCon -> [Bool] -> [TcType] -> EqCt -> TcS [FunDepEqns]
-- Implements (INJFAM:Wanted/top)
mkTopOpenFamEqFDs fam_tc inj_flags work_args (EqCt { eq_rhs = work_rhs })
  = do { fam_envs <- getFamInstEnvs
       ; let branches :: [CoAxBranch]
             branches = concatMap (fromBranches . coAxiomBranches . fi_axiom) $
                        lookupFamInstEnvByTyCon fam_envs fam_tc
       ; return (mapMaybe do_one branches) }
  where
    do_one :: CoAxBranch -> Maybe FunDepEqns
    do_one branch@(CoAxBranch { cab_tvs = branch_tvs
                              , cab_lhs = branch_lhs_tys
                              , cab_rhs = branch_rhs })
      | Just subst <- tcUnifyTysForInjectivity False [branch_rhs] [work_rhs]
                      -- False: matching, not unifying
      , let (subst', qtvs) = trim_qtvs subst branch_tvs
            branch_lhs_tys' = substTys subst' branch_lhs_tys
      , apartnessCheck branch_lhs_tys' branch  -- See (TIF3)
      = Just (mkInjectivityFDEqn inj_flags qtvs branch_lhs_tys' work_args)

      | otherwise
      = Nothing

mkLocalFamEqFDs :: [EqCt] -> TyCon -> [Bool] -> [TcType] -> Xi -> SolverStage [FunDepEqns]
-- Wanted constraints only
-- Both open and closed type families, but only ones with declared injectivity
-- See Note [Type inference for type families with injectivity] esp (TIF2)
mkLocalFamEqFDs eqs_for_me fam_tc inj_flags work_args work_rhs
  = do { let -- eqns_from_inerts: see (INJFAM:Wanted/other)
             eqns_from_inerts = mapMaybe do_one eqs_for_me
             -- eqns_from_self: see (INJFAM:Wanted/Self)
             eqns_from_self   = case tcSplitTyConApp_maybe work_rhs of
                                  Just (tc,rhs_tys) | tc==fam_tc -> [mk_eqn rhs_tys]
                                  _                              -> []

       ; return (eqns_from_inerts ++ eqns_from_self) }
  where
    do_one :: EqCt -> Maybe FunDepEqns
    do_one (EqCt { eq_lhs = TyFamLHS _ inert_args, eq_rhs = inert_rhs })
      | work_rhs `tcEqType` inert_rhs = Just (mk_eqn inert_args)
      | otherwise                     = Nothing
    do_one _ = pprPanic "interactFunEq 2" (ppr fam_tc)  -- TyVarLHS

    mk_eqn iargs = mkInjectivityFDEqn inj_flags [] work_args iargs

trim_qtvs :: Subst -> [TcTyVar] -> (Subst,[TcTyVar])
-- Tricky stuff: see (TIF1) in
-- Note [Type inference for type families with injectivity]
trim_qtvs subst []       = (subst, [])
trim_qtvs subst (tv:tvs)
  | tv `elemSubst` subst = trim_qtvs subst tvs
  | otherwise            = let !(subst1, tv')  = substTyVarBndr subst tv
                               !(subst', tvs') = trim_qtvs subst1 tvs
                           in (subst', tv':tvs')

-----------------------------------------
--  Built-in type families
-----------------------------------------

tryGivenBuiltinFamEqFDs :: [EqCt] -> TyCon -> BuiltInSynFamily
                        -> [TcType] -> EqCt -> SolverStage ()
-- TyCon is definitely a built-in type family
-- Built-in type families are special becase we can generate
-- evidence from /Givens/. For example:
--    from [G] x+4~7 we can deduce [G] x~7
-- That's important!
-- See Note [Given/Given fundeps for built-in type families]
tryGivenBuiltinFamEqFDs eqs_for_me fam_tc ops work_args
                        (EqCt { eq_ev = work_ev, eq_rhs = work_rhs })
  = Stage $
    do { traceTcS "tryBuiltinFamEqFDs" $
         vcat [ text "lhs:" <+> ppr fam_tc <+> ppr work_args
              , text "rhs:" <+> ppr work_rhs
              , text "work_ev:" <+> ppr work_ev ]

       -- interact with inert Givens, emitting new Givens
       ; mapM_ do_one eqs_for_me

       -- Interact with top-level instancs, emitting new Givens
       ; emitNewGivens (ctEvLoc work_ev) $
           [ (Nominal, new_co)
           | (ax, _) <- tryInteractTopFam ops fam_tc work_args work_rhs
           , let new_co = mkAxiomCo ax [work_co] ]

       -- All done
       ; continueWith () }
  where
    work_co :: Coercion = ctEvCoercion work_ev

    do_one :: EqCt -> TcS ()
    -- Used only when work-item is Given
    do_one (EqCt { eq_ev  = inert_ev, eq_lhs = inert_lhs, eq_rhs = inert_rhs })
      | isGiven inert_ev                    -- Given/Given interaction
      , TyFamLHS _ inert_args <- inert_lhs  -- Inert item is F inert_args ~ inert_rhs
      , work_rhs `tcEqType` inert_rhs       -- Both RHSs are the same
      , -- So we have work_ev  : F work_args  ~ rhs
        --            inert_ev : F inert_args ~ rhs
        let pairs :: [(CoAxiomRule, TypeEqn)]
            pairs = tryInteractInertFam ops fam_tc work_args inert_args
      , not (null pairs)
      = do { traceTcS "tryGivenLocalFamEqFDs" (vcat[ ppr fam_tc <+> ppr work_args
                                                     , text "work_ev" <+>  ppr work_ev
                                                     , text "inert_ev" <+> ppr inert_ev
                                                     , ppr work_rhs
                                                     , ppr pairs ])
           ; emitNewGivens (ctEvLoc inert_ev) (map mk_ax_co pairs) }
             -- This CtLoc for the new Givens doesn't reflect the
             -- fact that it's a combination of Givens, but I don't
             -- think that matters.
      where
        inert_co = ctEvCoercion inert_ev
        mk_ax_co (ax,_) = (Nominal, mkAxiomCo ax [combined_co])
          where
            combined_co = work_co `mkTransCo` mkSymCo inert_co
            -- work_co     :: F work_args  ~ rhs
            -- inert_co    :: F inert_args ~ rhs
            -- combined_co :: F work_args ~ F inert_args

    do_one _ = return ()

mkTopBuiltinFamEqFDs :: TyCon -> BuiltInSynFamily -> [TcType] -> Xi -> SolverStage [FunDepEqns]
mkTopBuiltinFamEqFDs fam_tc ops work_args work_rhs
  = return [FDEqns { fd_qtvs = []
                   , fd_eqs = map snd $ tryInteractTopFam ops fam_tc work_args work_rhs }]

mkLocalBuiltinFamEqFDs :: [EqCt] -> TyCon -> BuiltInSynFamily
                       -> [TcType] -> Xi -> SolverStage [FunDepEqns]
mkLocalBuiltinFamEqFDs eqs_for_me fam_tc ops work_args work_rhs
  = do { let do_one :: EqCt -> [FunDepEqns]
             do_one (EqCt { eq_lhs = TyFamLHS _ inert_args, eq_rhs = inert_rhs })
               | inert_rhs `tcEqType` work_rhs = [mk_eqn inert_args]
               | otherwise                     = []
             do_one _ = pprPanic "interactFunEq 1" (ppr fam_tc) -- TyVarLHS

             mk_eqn :: [TcType] -> FunDepEqns
             mk_eqn iargs = FDEqns { fd_qtvs = []
                                   , fd_eqs = map snd $ tryInteractInertFam ops fam_tc
                                                                     work_args iargs }

       ; let eqns_from_inerts = concatMap do_one eqs_for_me
             eqns_from_self   = case tcSplitTyConApp_maybe work_rhs of
                                  Just (tc,rhs_tys) | tc==fam_tc -> [mk_eqn rhs_tys]
                                  _                              -> []
       ; return (eqns_from_inerts ++ eqns_from_self) }

--------------------
mkInjectivityFDEqn :: [Bool]       -- Injectivity flags
                   -> [TcTyVar]    -- Quantify these
                   -> [TcType] -> [TcType]  -- Make these equal
                   -> FunDepEqns
-- When F s1 s2 s3 ~ F t1 t2 t3, and F has injectivity info [True,False,True]
--   return the FDEqns { fd_eqs = [Pair s1 t1, Pair s3 t3] }
-- The injectivity flags [Bool] will not all be False, but nothing goes wrong if they are
mkInjectivityFDEqn inj_args qtvs lhs_args rhs_args
  = FDEqns { fd_qtvs = qtvs, fd_eqs = eqs }
  where
    eqs = [ Pair lhs_arg rhs_arg
          | (True, lhs_arg, rhs_arg) <- zip3 inj_args lhs_args rhs_args ]

getInertFamEqsFor :: TyCon -> [TcType] -> Xi -> TcS [EqCt]
-- Look in the InertSet, and return all inert equalities
--    F tys ~N# rhs
--    where F is the specified TyCon
-- But filter out ones that can't possibly help;
--    that is, ones that are "apart" from the Wanted
-- Returns a mixture of Given and Wanted
-- Nominal only, becaues Representational equalities don't interact
--    with type family dependencies
getInertFamEqsFor fam_tc work_args work_rhs
  = do { IC {inert_funeqs = funeqs } <- getInertCans
       ; return [ funeq_ct | equal_ct_list <- findFunEqsByTyCon funeqs fam_tc
                           , funeq_ct@(EqCt { eq_eq_rel = eq_rel
                                            , eq_lhs = TyFamLHS _ inert_args
                                            , eq_rhs = inert_rhs })
                                 <- equal_ct_list
                           , NomEq == eq_rel
                           , eqnIsRelevant inert_args inert_rhs work_args work_rhs ] }

eqnIsRelevant :: [TcType] -> TcType
              -> [TcType] -> TcType
              -> Bool
eqnIsRelevant lhs_tys1 rhs_ty1 lhs_tys2 rhs_ty2
  = not ((rhs_ty1:lhs_tys1) `typeListsAreApart` (rhs_ty2:lhs_tys2))


{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Here is the plan; see also Note [Injective type families]
in GHC.Core.TyCon):

Suppose we have a constraint
   [G/W]  F t1 t2 ~ rhs
Then if F is
    - user-defined, not built-in
    - declared injective
    - either open or closed
we attempt to exploit injectivity, via `mkLocalFamEqFDs`:

* (INJFAM:Given) For Given constraints
  - When F is user-defined, do nothing at all
    See Note [No Given/Given fundeps]
  - When F is a built-in type family, we can do better;
    See Note [Given/Given fundeps for built-in type families]

* (INJFAM:Wanted/Self) see `mkLocalFamEqFDs`
    work item: [W] F s1 s2 ~ F t1 t2
  We can generate FunDepEqns: (s2 ~ t2)

* (INJFAM:Wanted/other) see `mkLocalFamEqFDs`
    work item: [W]   F s1 s2 ~ rhs   -- Wanted
    inert:     [G/W] F t2 t2 ~ rhs   -- Same `rhs`, Given or Wanted
  We can generate FunDepEqns: (s2 ~ t2)

* (INJFAM:Wanted/top) see `mkTopUserFamEqFDs`
    work item: [W] F s1 s2 ~ rhs
    type instance forall a b c. F t1 t2 ~ top_rhs
  and we can /match/ the LHS, so that
     S(top_rhs) = rhs
  then we can generate the FunDepEqns:  forall a c. s2 ~ S(t2)
  But see wrinkle (TIF1), (TIF3)

For /built-in/ type families, it's pretty similar, except that

* We generate new Given constraints, /with evidence/, from Givens.
  e.g. from             [G] x+2 ~ 7
       we can generate  [G] x ~ 5
  See `tryGivenBuiltinFamEqFDs`

* For Wanteds things go much as for user-defined families.
   * mkLocalBuiltinFamEqFDs
   * mkTopBuiltinFamEqFDs

(TIF1) Generating fundeps from a top-level type instance is covered in
  Section 5.2 in the Injective Type Families paper. It's a bit tricky.
  Consider
     type family F @k (a::k) (b::k) = r | r -> k b
     type instance forall k (a::k) (b::k). F @k (Proxy @k a) (Proxy @k b) = Proxy @k a
     [W] F @kappa alpha beta ~ Maybe (Proxy @kappa (delta::kappa))

  we match (Proxy @kappa delta) against the template (Proxy k a), succeeding
  with substitution [k:->kappa, a:->delta].  We want to generate this FunDepEqns
    FDEqn { fd_qtvs = [b:kappa], fd_eqs = [ beta ~ Proxy @kappa b ] }
  Notice that
    * we must quantify the FunDepEqns over `b`, which is not matched; for this
      we will generate a fresh unification variable in `instantiateFunDepEqn`.
    * we must substitute `k:->kappa` in the kind of `b`.
  This fancy footwork for `fd_qtvs` is done by `trim_qtvs` in
  `mkInjWantedFamEqTopEqns`.

(TIF2) All this applies equally to /closed/ type families; it is /not/
  subsumed by Note [Exploiting closed type families].  Consider:
      type family F a | r -> a where { .... }

      [W] F t1 ~ a
      [W] F t2 ~ a

  Then since F is declared injective, we can generate t1~t2.

  As for open type families, we insist on /user-declared/ injectivity only; we
  don't try to /infer/ injectivity even for a closed family.  See Section 3.4
  of the "Injective type families" paper.  (Arguably, we could /infer/
  injectivity for closed type families, and that would be more in the spirit of
  Note [Exploiting closed type families].)

(TIF3) Further to (TIF1), if we are considering a /closed/ type family, we
  must ensure (see Section 5.2 of the paper) that after matching that
  equation would indeed be the one to fire.  So we call `apartnessCheck`
  on the branch to ensure this, in `mkTopUserFamEqFDs`.

Definition [Relevance]
~~~~~~~~~~~~~~~~~~~~~~
We say that a closed-type-family equation `F lhs = rhs` is
   /relevant/ for a Wanted [W] F wlhs ~ wrhs
iff
  (R1) (lhs,rhs) pre-unifies with (wlhs,wrhs) yielding substitution S.
       See (RW1),(RW2), (RW3)

  (R2) There is no earlier equation that matches S(lhs).  See (RW4) below.

(RW1) Pre-unification treats type-family applications as binding to anything,
    rather like type variables.  If two types don't even pre-unify, we say that they
    are /apart/.  It is done by `tcUnifyTysForInjectivity`.

(RW2) lhs and wlhs are of course each a list of types. We don't really form a
    tuple (lhs,rhs); we just pre-unify the list (rhs_ty : lhs_tys).

(RW3) Why "pre-unifies with" rather than "unifies with"?  Answer: see Section 5.2
    in "Injective Type Families for Haskell".  A concrete example is test T12522a:

        newtype I a = I a

        type family Curry (as :: [Type]) b = f | f -> as b where
            Curry '[]    b = I b
            Curry (a:as) b = a -> Curry as b

        [W] Curry alpha beta ~ (gamma -> String -> I String)


    Clearly the RHS is apart from the first equation and we want to fire injectivity
    on the second equation.

(RW4) Why "no earlier equation matches" in conditoin (R2)?  Consider the family

          type family Bak a = r where
             Bak Int  = Char   -- B1
             Bak Char = Int    -- B2
             Bak a    = a      -- B3

    and [W] Bak alpha ~ Char. In fact, only (B2) is relevant for this Wanted.
    You might think that (B3) could be instantiated to Bak Char ~ Char; but
    actually that instantiation will never fire because (B2) Bak Char ~ Int would
    fire first.  So the only way to return a Char is if the argment is Int; so we
    can emit [W] alpha ~ Int.  Hence (B3) is not relevant; only (B2) is relevant.

    That is the reason for condition (R2) in the definition of Relevance above.
    A watertight proof that this is the Right Thing is not very easy.  See more
    discussion in #23162.

Note [Exploiting closed type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
    type family F a b where
       F Int Bool = Bool  -- (F1)
       F Int Char = Char  -- (F2)
       F Bool a   = Char  -- (F3)

    [W]  F Int alpha ~ Char

The /only/ way to solve this Wanted is using (F2), so we can safely unify
alpha:=Char without risking losing any solutions.  That is what
`mkTopClosedFamEqFDs` does.  Ticket #23162 has lots of background detail

More precisely, here is the Closed Family Fundep Algorithm (CFFA)

    IF * F a is a closed type family.
       * We are trying to solve [W] F wlhs ~ wrhs.
       * There are no "relevant" Givens [G] F lhs ~ rhs.  See (CF1) below.
       * F  has exactly one equation, F lhs = rhs that is "relevant" for that Wanted
    THEN
      we can emit and solve the fundep equalities:
          [W] wlhs1 ~ lhs1
          ...
          [W] wlhsn ~ lhsn
          [W] wrhs ~ rhs     See (CF2) below.
    with fresh unification vars in lhs and rhs for the quantified variables of the
    equation.

See Definition [Relevance] for what "relevant" means.
We need to take care about non-termination; see (CF3).

Key point: equations that are not relevant do not need to be considered for fundeps at all.

(CF1) Why "no relevant Givens", implemented by `hasRelevantGivens`?
      Consider test `CEqCanOccursCheck`:

        type family F a where
          F Bool = Bool
        type family G a b where
          G a a = a

        foo :: (F a ~ a, F a ~ b) => G a b -> ()

    In the ambiguity check for foo we get
      [G] F a ~ a
      [G] F a ~ b
      [W] F alpha ~ alpha
      [W] F alpha ~ beta
      [W] G a b ~ G alpha beta

    Now use algoritm (CFFA) on [W] F alpha ~ alpha.  There is only one
    equation for F, and it is relevant, so we gaily emit the fundep equality
    [W] alpha ~ Bool, and we are immediately dead.  We end up with
        • Could not deduce ‘b ~ Bool’
          from the context: (F a ~ a, F a ~ b)

    It is true that the only way a caller can satisfy F a ~ a is by instantiating
    a to Bool; but we don't have /evidence/ for that which we can use to satisfy
    b ~ Bool.

    The trouble is that (CFFA) relies on knowing /all/ the equations for F;
    but in this case we have some Given constraints that locally extend F.

    This relates closely to
        Note [Do local fundeps before top-level instances] and
        Note [Do fundeps last] (which are saying much the same thing)

    These Notes are extremely delicate.  Suppose a local Given doesn't give rise
    to a fundep equation and we move on to the top-level fundeps; but then after
    some other constraints are solved the local Given would fire.  Indeed this is
    exactly what happens above!

    Solution: Only run (CFFA) if there are no relevant Givens.  This is much more
    robust than "only run (CFFA) if attempting local fundeps gives rise to
    equations" because if a Given is irrelevant is is forever irrelevant.  It's a
    bit like `noMatchableGivenDicts` and `mightEqualLater` for dictionaries.
    Indeed we should probably apply a similar check when doing fundeps on
    dictionaries.

(CF2) Fundeps from RHS as well as LHS.  Consider this from test T6018:

       type family Bak a = r where
            Bak Int  = Char
            Bak Char = Int
            Bak a    = a

   and [W] Bak alpha ~ ().  Only the last equation is relevant, but we clearly
   don't want to just produce a new fundep Wanted for the LHS: beta ~ alpha,
   where beta is freshly instantiated from a.  We must /also/ produce an equality
   [W] beta ~ () from the RHS.  Hence the [W] wrhs ~ rhs in (CFFA).

(CF3) Algorithm (CFFA) can diverge, just as ordinary fundeps can, as discussed
  extensively in the paper "Understanding functional dependencies via constraint
  handling rules".  Example (test T16512a):

       type family LV as b where
           LV (a : as) b = a -> LV as b

        [W] LV as bsk ~ LV as (ask->bsk)

    Here `as` is a unification variable, while `ask` and `bsk` are skolems.
    There is one relevant equation, because there is only one equation in the
    family!  Hence algorithm (CFFA) generates new equalities
          x:asx ~ as
          bx ~ bsk
          (ax -> LV asx bx) ~ LV as (ask->bsk)

    where ax, asx and bx are fresh unification variables. We can solve:
          as := ax:asx
          bx := bsk

    Leaving us with
      (ax -> LV asx bsk)  ~   LV (ax:asx) (ask->bsk)
      -->{reduce RHS with the equation for LV}
         (ax -> LV asx bsk)  ~   (ax -> LV asx (ask->bsk))
      -->{decompose ->)
         LV asx bsk ~ LV asx (ask->bsk)

     And now we are back where we started -- loop.

  We solve this by bumping the `ctLocDepth` in `solveFunDeps`, and imposing
  a depth bound.  See the call to `bumpReductionDepth`.  If the depth limit
  is exceeded we add an error message and fail in the monad.

  Take care: when we are solving-for-unsatisfiability, in the pattern match
  checker, we must carefully catch this failure: see the use of `tryM` in
  `tcCheckGivens`.

(CF4) If one of the fundeps generated by interacting with the local equalities is
  definitely insoluble (e.g. Int~Bool) then there is no point in continuing to
  look at the global type-family definitions.  That can happen.  It came up when
  I was looking at non-termination for closed type families, but it's a small
  improvement in general.

(CF5) Consider (see "Yikes5" in #23162):
      type family F a where
         F (Just x) = Int

      [W] F alpha ~ F (G beta)
      [W] alpha ~ G beta
  We can solve both Wanteds by `alpha := G beta`.  But if we use fundeps on the
  first Wanted, we see that there is just one relevant equation, so we'll emit
      [W] alpha ~ Just gamma
      [W] F (G beta) ~ Int
  and try to solve them.  We'll do `alpha := Just gamma`, and then it's game over;
  we end up with these constraints that we can't solve
      [W] Just gamma ~ F (G beta)
      [W] Just gamma ~ G beta
  This actually happens when compiling the libarary `type-rows`, in Data.Row.Variants.

  Even if there is only one relevant equation, we can only use it /if/ we are
  sure that we cannot solve the current Wanted by reflexivity; that is, if we
  must do a type-family reduction to solve it.  So if the Wanted is [W] F tys ~
  rhs we must be sure that `rhs` can't turn into `F tys`.  The only way to be
  sure of that is if `rhs` is headed by a generative type constructor. See
  `isGenerativeType`.

  Otherwise prinicple (FUNDEP-COMPLETENESS) is threatened.

  It's important for `isGenerativeType` to look through casts.  Consider T13822
      type I :: Ty k -> IK k
      type family I t = res | res -> t where
        I TInt = Int |> g    -- where g :: Type ~# IK k
  and [W] I alpha ~ Int |> g2
  Here we definiteily want to take advantage of injectivity.

Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in
our solved cache (which is the default behaviour or xCtEvidence), because the
interaction may not be contributing towards a solution. Here is an example:

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
             -> [FunDepEqns]
             -> TcS ( Bool   -- True <=> some insoluble fundeps
                             --    See Note [Insoluble fundeps]
                    , Bool ) -- True <=> unifications happened
-- Solve a bunch of type-equality equations, generated by functional dependencies
-- By "solve" we mean: (only) do unifications.  We do not generate evidence, and
-- other than unifications there should be no effects whatsoever
--
-- See (SOLVE-FD) in Note [Overview of functional dependencies in type inference]
solveFunDeps work_ev fd_eqns
  | null fd_eqns
  = return (False, False) -- Common case no-op

  | otherwise
  = do { traceTcS "solveFunDeps {" (ppr work_ev)
       ; loc' <- bumpReductionDepth (ctEvLoc work_ev) (ctEvPred work_ev)
                 -- See (CF3) in Note [Exploiting closed type families]

       ; (unifs, residual)
             <- reportFineGrainUnifications $
                nestFunDepsTcS              $
                TcS.pushTcLevelM_           $
                   -- pushTcLevelTcM: increase the level so that unification variables
                   -- allocated by the fundep-creation itself don't count as useful unifications
                   -- See Note [Deeper TcLevel for partial improvement unification variables]
                do { (_, eqs) <- wrapUnifier (ctEvRewriters work_ev) loc' Nominal $
                                 do_fundeps
                   ; solveSimpleWanteds eqs }
    -- Why solveSimpleWanteds?  Answer
    --     (a) We don't want to rely on the eager unifier being clever
    --     (b) F Int alpha ~ Maybe Int   where  type instance F Int x = Maybe x

       -- Kick out any inert constraints that mention variables
       -- that were unified by the fundep
       ; kickOutAfterUnification unifs

       ; let insoluble_fundeps = any insolubleCt (wc_simple residual)
             -- Don't use insolubleWC, because that ignores Given constraints
             -- and Given constraints are super-important when doing
             -- tcCheckGivens in the pattern match overlap checker
             -- See Note [Insoluble fundeps]

       ; traceTcS "solveFunDeps }" (ppr insoluble_fundeps <+>  ppr unifs $$ ppr residual)
       ; return (insoluble_fundeps, not (isEmptyVarSet unifs)) }
  where
    do_fundeps :: UnifyEnv -> TcM ()
    do_fundeps env = mapM_ (do_one env) fd_eqns

    do_one :: UnifyEnv -> FunDepEqns -> TcM ()
    do_one uenv eqn = do { eqs <- instantiateFunDepEqns eqn
                         ; uPairsTcM uenv eqs }

instantiateFunDepEqns :: FunDepEqns -> TcM [TypeEqn]
instantiateFunDepEqns (FDEqns { fd_qtvs = tvs, fd_eqs = eqs })
  | null tvs
  = return rev_eqs
  | otherwise
  = do { TcM.traceTc "instantiateFunDepEqns" (ppr tvs $$ ppr eqs)
       ; (_, subst) <- instFlexiXTcM emptySubst tvs  -- Takes account of kind substitution
       ; return (map (subst_pair subst) rev_eqs) }
  where
    rev_eqs = reverse eqs
       -- (reverse eqs): See Note [Reverse order of fundep equations]

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
However, the idea of working left-to-right still seems worthwhile (less
kick-out), and so the calls to 'reverse' remain.

This treatment is also used for class-based functional dependencies, although
we do not have a program yet known to exhibit a loop there. It just seems
like the right thing to do.

In general, I believe this is (now, anyway) just an optimisation, not required
to avoid loops.
-}

{- *********************************************************************
*                                                                      *
                 Historical notes
     Here are a bunch of Notes that are rendered obsolete by
  Note [Deeper TcLevel for partial improvement unification variables]
*                                                                      *
********************************************************************* -}

{-
Historical Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Historical Note [Fundeps with instances, and equality orientation],
which describes the Exact Same Problem, with the same solution, but for
functional dependencies.

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
This Note (anonymous, but related to dict-solving) is rendered obsolete by
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
