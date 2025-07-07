{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

-- | Solving Class constraints CDictCan
module GHC.Tc.Solver.FunDeps (
  unifyAndEmitFunDepWanteds,
  doDictFunDepImprovement,
  ImprovementResult, noImprovement
  ) where

import GHC.Prelude

import GHC.Tc.Instance.FunDeps
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify( UnifyEnv(..) )
import GHC.Tc.Utils.Monad    as TcM

import GHC.Core.Type
import GHC.Core.InstEnv     ( InstEnvs, ClsInst(..) )
import GHC.Core.Coercion.Axiom( TypeEqn )

import GHC.Types.Name
import GHC.Types.Var.Set

import GHC.Utils.Outputable

import GHC.Data.Bag
import GHC.Data.Pair

import qualified Data.Semigroup as S

import Control.Monad

{- *********************************************************************
*                                                                      *
*          Functional dependencies, instantiation of equations
*                                                                      *
************************************************************************

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

To achieve this required some refactoring of GHC.Tc.Instance.FunDeps (nicer
now!).

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

Note [Fundeps with instances, and equality orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  If we ineract that Wanted with /both/ the t0p-level instance, /and/ the
  local Given, we'll get
      beta ~ Int   and     beta ~ b
  respectively.  That would generate (b~Bool), which would fai.  I think
  it doesn't matter which of the two we pick, but historically we have
  picked teh local-fundeps firs.

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

doDictFunDepImprovement :: Cts -> TcS ImprovementResult
-- (doDictFunDepImprovement inst_envs cts)
--   * Generate the fundeps from interacting the
--     top-level `inst_envs` with the constraints `cts`
--   * Do the unifications and return any unsolved constraints
-- See Note [Fundeps with instances, and equality orientation]
-- foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
doDictFunDepImprovement unsolved_wanteds
  = do { inerts    <- getInertCans  -- The inert_dicts are all Givens
       ; inst_envs <- getInstEnvs
       ; (_, imp_res) <- foldM (do_one_dict inst_envs)
                               (inert_dicts inerts, noopImprovement)
                               unsolved_wanteds
       ; return imp_res }

do_one_dict :: InstEnvs
            -> (DictMap DictCt, ImprovementResult)
            -> Ct
            -> TcS (DictMap DictCt, ImprovementResult)
-- The `local_dicts` accumulator starts life as just the Givens, but
--   as we encounter each Wanted we augment it. Result: each Wanted
--   is interacted with all the Givens, and all prededing Wanteds.
--   This is worst-case quadratic because we have to compare each
--   constraint with all the others, to find all the pairwise interactions
do_one_dict inst_envs (local_dicts, imp_res) (CDictCan dict_ct)
  = do { (local_dicts1, imp_res1) <- do_one_local local_dicts dict_ct
       ; if noImprovement imp_res1
         then do { imp_res2 <- do_one_top inst_envs dict_ct
                 ; return (local_dicts1, imp_res `plusImprovements` imp_res2) }
         else      return (local_dicts1, imp_res `plusImprovements` imp_res1) }

do_one_dict _ acc _  -- Non-DictCt constraints
  = return acc

do_one_top :: InstEnvs -> DictCt -> TcS ImprovementResult
do_one_top inst_envs (DictCt { di_ev = ev, di_cls = cls, di_tys = xis })
  = unifyFunDepWanteds ev eqns
  where
    eqns :: [FunDepEqn (CtLoc, RewriterSet)]
    eqns = improveFromInstEnv inst_envs mk_ct_loc cls xis

    dict_pred      = mkClassPred cls xis
    dict_loc       = ctEvLoc ev
    dict_origin    = ctLocOrigin dict_loc
    dict_rewriters = ctEvRewriters ev

    mk_ct_loc :: ClsInst  -- The instance decl
              -> (CtLoc, RewriterSet)
    mk_ct_loc ispec
      = (dict_loc { ctl_origin = new_orig }, dict_rewriters)
      where
        inst_pred = mkClassPred cls (is_tys ispec)
        inst_loc  = getSrcSpan (is_dfun ispec)
        new_orig  = FunDepOrigin2 dict_pred dict_origin
                                  inst_pred inst_loc

do_one_local :: DictMap DictCt -> DictCt -> TcS (DictMap DictCt, ImprovementResult)
-- Using functional dependencies, interact the unsolved Wanteds
-- against each other and the inert Givens, to produce new equalities
do_one_local locals dict_ct@(DictCt { di_cls = cls, di_ev = wanted_ev })
    -- locals contains all the Givens and earlier Wanteds
  = do { imp_res <- foldM do_interaction noopImprovement $
                    findDictsByClass locals cls
       ; return (addDict dict_ct locals, imp_res) }
  where
    wanted_pred = ctEvPred wanted_ev
    wanted_loc  = ctEvLoc  wanted_ev

    do_interaction :: (Cts,Bool) -> DictCt -> TcS (Cts,Bool)
    do_interaction (new_eqs, unifs) (DictCt { di_ev = all_ev }) -- This can be Given or Wanted
      = do { traceTcS "doLocalFunDepImprovement" $
             vcat [ ppr wanted_ev
                  , pprCtLoc wanted_loc, ppr (isGivenLoc wanted_loc)
                  , pprCtLoc all_loc, ppr (isGivenLoc all_loc)
                  , pprCtLoc deriv_loc, ppr (isGivenLoc deriv_loc) ]

           ; (new_eqs1, unifs1) <- unifyFunDepWanteds wanted_ev $
                                   improveFromAnother (deriv_loc, all_rewriters)
                                                      all_pred wanted_pred
           ; return (new_eqs1 `unionBags` new_eqs, unifs1 || unifs) }
      where
        all_pred  = ctEvPred all_ev
        all_loc   = ctEvLoc all_ev
        all_rewriters = ctEvRewriters all_ev
        deriv_loc = wanted_loc { ctl_depth  = deriv_depth
                               , ctl_origin = deriv_origin }
        deriv_depth = ctl_depth wanted_loc `maxSubGoalDepth`
                      ctl_depth all_loc
        deriv_origin = FunDepOrigin1 wanted_pred
                                     (ctLocOrigin wanted_loc)
                                     (ctLocSpan wanted_loc)
                                     all_pred
                                     (ctLocOrigin all_loc)
                                     (ctLocSpan all_loc)


{-
************************************************************************
*                                                                      *
              Emitting equalities arising from fundeps
*                                                                      *
************************************************************************
-}

type ImprovementResult = (Cts, Bool)
  -- The Cts are the new equality constraints
  -- The Bool is True if we unified any meta-ty-vars on when
  --  generating those new equality constraints

noopImprovement :: ImprovementResult
noopImprovement = (emptyBag, False)

noImprovement :: ImprovementResult -> Bool
noImprovement (cts,unifs) = not unifs && isEmptyBag cts

plusImprovements :: ImprovementResult -> ImprovementResult -> ImprovementResult
plusImprovements (cts1,unif1) (cts2,unif2)
  = (cts1 `unionBags` cts2, unif1 || unif2)


unifyAndEmitFunDepWanteds :: CtEvidence  -- The work item
                          -> [FunDepEqn (CtLoc, RewriterSet)]
                          -> TcS Bool   -- True <=> some unification happened
unifyAndEmitFunDepWanteds ev fd_eqns
  = do { (new_eqs, unifs)  <- unifyFunDepWanteds ev fd_eqns

       ;   -- Emit the deferred constraints
           -- See Note [Work-list ordering] in GHC.Tc.Solved.Equality
           --
           -- All the constraints in `cts` share the same rewriter set so,
           -- rather than looking at it one by one, we pass it to
           -- extendWorkListChildEqs; just a small optimisation.
       ; unless (isEmptyBag new_eqs) $
         updWorkListTcS (extendWorkListChildEqs ev new_eqs)

       ; return unifs }

unifyFunDepWanteds :: CtEvidence  -- The work item
                  -> [FunDepEqn (CtLoc, RewriterSet)]
                  -> TcS ImprovementResult

unifyFunDepWanteds _ [] = return noopImprovement -- common case noop
-- See Note [FunDep and implicit parameter reactions]

unifyFunDepWanteds ev fd_eqns
  = do { (fresh_tvs_s, cts, unified_tvs) <- wrapUnifierX ev Nominal do_fundeps

       -- Figure out if a "real" unification happened: See Note [unifyFunDeps]
       ; let unif_happened = any is_old_tv unified_tvs
             fresh_tvs     = mkVarSet (concat fresh_tvs_s)
             is_old_tv tv  = not (tv `elemVarSet` fresh_tvs)

       ; return (cts, unif_happened) }
  where
    do_fundeps :: UnifyEnv -> TcM [[TcTyVar]]
    do_fundeps env = mapM (do_one env) fd_eqns

    do_one :: UnifyEnv -> FunDepEqn (CtLoc, RewriterSet) -> TcM [TcTyVar]
    do_one uenv (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = (loc, rewriters) })
      = do { (fresh_tvs, eqs') <- instantiateFunDepEqn tvs (reverse eqs)
                     -- (reverse eqs): See Note [Reverse order of fundep equations]
           ; uPairsTcM env_one eqs'
           ; return fresh_tvs }
      where
        env_one = uenv { u_rewriters = u_rewriters uenv S.<> rewriters
                       , u_loc       = loc }

instantiateFunDepEqn :: [TyVar] -> [TypeEqn] -> TcM ([TcTyVar], [TypeEqn])
instantiateFunDepEqn tvs eqs
  | null tvs
  = return ([], eqs)
  | otherwise
  = do { TcM.traceTc "emitFunDepWanteds 2" (ppr tvs $$ ppr eqs)
       ; (tvs', subst) <- instFlexiXTcM emptySubst tvs  -- Takes account of kind substitution
       ; return (tvs', map (subst_pair subst) eqs) }
  where
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

