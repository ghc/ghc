-- | Solving Class constraints CDictCan
module GHC.Tc.Solver.Dict (
  doTopReactDict,
  checkInstanceOK,
  matchLocalInst, chooseInstance

  ) where

import GHC.Prelude

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad

import GHC.Builtin.Names ( coercibleTyConKey, heqTyConKey, eqTyConKey )

import GHC.Core.Type as Type
import GHC.Core.InstEnv     ( DFunInstType, Coherence(..) )
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.Unify ( ruleMatchTyKiX )

import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.SrcLoc
import GHC.Types.Var.Env
import GHC.Types.Unique( hasKey )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Driver.Session

import qualified GHC.LanguageExtensions as LangExt

import Data.Maybe ( listToMaybe, mapMaybe )


{- *******************************************************************
*                                                                    *
         Top-level reaction for class constraints (CDictCan)
*                                                                    *
**********************************************************************-}

doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = ev, cc_class = cls
                                          , cc_tyargs = xis })
  | isGiven ev   -- Never use instances for Given constraints
  = continueWith work_item
     -- See Note [No Given/Given fundeps]

  | Just solved_ev <- lookupSolvedDict inerts dict_loc cls xis   -- Cached
  = do { setEvBindIfWanted ev IsCoherent (ctEvTerm solved_ev)
       ; stopWith ev "Dict/Top (cached)" }

  | otherwise  -- Wanted, but not cached
   = do { dflags <- getDynFlags
        ; lkup_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_res of
               OneInst { cir_what = what }
                  -> do { insertSafeOverlapFailureTcS what work_item
                        ; addSolvedDict what ev cls xis
                        ; chooseInstance work_item lkup_res }
               _  -> -- NoInstance or NotSure
                     -- We didn't solve it; so try functional dependencies with
                     -- the instance environment
                     do { doTopFundepImprovement work_item
                        ; tryLastResortProhibitedSuperclass inerts work_item } }
   where
     dict_loc = ctEvLoc ev


doTopReactDict _ w = pprPanic "doTopReactDict" (ppr w)

-- | As a last resort, we TEMPORARILY allow a prohibited superclass solve,
-- emitting a loud warning when doing so: we might be creating non-terminating
-- evidence (as we are in T22912 for example).
--
-- See Note [Migrating away from loopy superclass solving] in GHC.Tc.TyCl.Instance.
tryLastResortProhibitedSuperclass :: InertSet -> Ct -> TcS (StopOrContinue Ct)
tryLastResortProhibitedSuperclass inerts
    work_item@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = xis })
  | let loc_w  = ctEvLoc ev_w
        orig_w = ctLocOrigin loc_w
  , ScOrigin _ NakedSc <- orig_w   -- work_item is definitely Wanted
  , Just ct_i <- lookupInertDict (inert_cans inerts) loc_w cls xis
  , let ev_i = ctEvidence ct_i
  , isGiven ev_i
  = do { setEvBindIfWanted ev_w IsCoherent (ctEvTerm ev_i)
       ; ctLocWarnTcS loc_w $
         TcRnLoopySuperclassSolve loc_w (ctPred work_item)
       ; return $ Stop ev_w (text "Loopy superclass") }
tryLastResortProhibitedSuperclass _ work_item
  = continueWith work_item

chooseInstance :: Ct -> ClsInstResult -> TcS (StopOrContinue Ct)
chooseInstance work_item
               (OneInst { cir_new_theta   = theta
                        , cir_what        = what
                        , cir_mk_ev       = mk_ev
                        , cir_coherence   = coherence })
  = do { traceTcS "doTopReact/found instance for" $ ppr ev
       ; deeper_loc <- checkInstanceOK loc what pred
       ; checkReductionDepth deeper_loc pred
       ; evb <- getTcEvBindsVar
       ; if isCoEvBindsVar evb
         then continueWith work_item
                  -- See Note [Instances in no-evidence implications]
         else
           do { evc_vars <- mapM (newWanted deeper_loc (ctRewriters work_item)) theta
              ; setEvBindIfWanted ev coherence (mk_ev (map getEvExpr evc_vars))
              ; emitWorkNC (freshGoals evc_vars)
              ; stopWith ev "Dict/Top (solved wanted)" }}
  where
     ev         = ctEvidence work_item
     pred       = ctEvPred ev
     loc        = ctEvLoc ev

chooseInstance work_item lookup_res
  = pprPanic "chooseInstance" (ppr work_item $$ ppr lookup_res)

checkInstanceOK :: CtLoc -> InstanceWhat -> TcPredType -> TcS CtLoc
-- Check that it's OK to use this instance:
--    (a) the use is well staged in the Template Haskell sense
-- Returns the CtLoc to used for sub-goals
-- Probably also want to call checkReductionDepth
checkInstanceOK loc what pred
  = do { checkWellStagedDFun loc what pred
       ; return deeper_loc }
  where
     deeper_loc = zap_origin (bumpCtLocDepth loc)
     origin     = ctLocOrigin loc

     zap_origin loc  -- After applying an instance we can set ScOrigin to
                     -- NotNakedSc, so that prohibitedSuperClassSolve never fires
                     -- See Note [Solving superclass constraints] in
                     -- GHC.Tc.TyCl.Instance, (sc1).
       | ScOrigin what _ <- origin
       = setCtLocOrigin loc (ScOrigin what NotNakedSc)
       | otherwise
       = loc

{- Note [Instances in no-evidence implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #15290 we had
  [G] forall p q. Coercible p q => Coercible (m p) (m q))
  [W] forall <no-ev> a. m (Int, IntStateT m a)
                          ~R#
                        m (Int, StateT Int m a)

The Given is an ordinary quantified constraint; the Wanted is an implication
equality that arises from
  [W] (forall a. t1) ~R# (forall a. t2)

But because the (t1 ~R# t2) is solved "inside a type" (under that forall a)
we can't generate any term evidence.  So we can't actually use that
lovely quantified constraint.  Alas!

This test arranges to ignore the instance-based solution under these
(rare) circumstances.   It's sad, but I  really don't see what else we can do.
-}


matchClassInst :: DynFlags -> InertSet
               -> Class -> [Type]
               -> CtLoc -> TcS ClsInstResult
matchClassInst dflags inerts clas tys loc
-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use any instance
-- whether top level, or local quantified constraints.
-- See Note [Instance and Given overlap]
  | not (xopt LangExt.IncoherentInstances dflags)
  , not (naturallyCoherentClass clas)
  , not (noMatchableGivenDicts inerts loc clas tys)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item=" <+> pprClassPred clas tys ]
       ; return NotSure }

  | otherwise
  = do { traceTcS "matchClassInst" $ text "pred =" <+> ppr pred <+> char '{'
       ; local_res <- matchLocalInst pred loc
       ; case local_res of
           OneInst {} ->  -- See Note [Local instances and incoherence]
                do { traceTcS "} matchClassInst local match" $ ppr local_res
                   ; return local_res }

           NotSure -> -- In the NotSure case for local instances
                      -- we don't want to try global instances
                do { traceTcS "} matchClassInst local not sure" empty
                   ; return local_res }

           NoInstance  -- No local instances, so try global ones
              -> do { global_res <- matchGlobalInst dflags False clas tys
                    ; traceTcS "} matchClassInst global result" $ ppr global_res
                    ; return global_res } }
  where
    pred = mkClassPred clas tys

-- | If a class is "naturally coherent", then we needn't worry at all, in any
-- way, about overlapping/incoherent instances. Just solve the thing!
-- See Note [Naturally coherent classes]
-- See also Note [The equality types story] in GHC.Builtin.Types.Prim.
naturallyCoherentClass :: Class -> Bool
naturallyCoherentClass cls
  = isCTupleClass cls
    || cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey


{- Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

From 'g' we get the implication constraint:
            forall a. Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The partial solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones.  But not skolems like 'a' in
  the implication constraint above.

The end effect is that, much as we do for overlapping instances, we
delay choosing a class instance if there is a possibility of another
instance OR a given to match our constraint later on. This fixes
tickets #4981 and #5002.

Other notes:

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* See also Note [What might equal later?] in GHC.Tc.Solver.InertSet.

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is #10195; another is #10177.

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see #6002 for a worked-out example where this makes a
  difference.

* Moreover notice that our goals here are different than the goals of
  the top-level overlapping checks. There we are interested in
  validating the following principle:

      If we inline a function f at a site where the same global
      instance environment is available as the instance environment at
      the definition site of f then we should get the same behaviour.

  But for the Given Overlap check our goal is just related to completeness of
  constraint solving.

* The solution is only a partial one.  Consider the above example with
       g :: forall a. Q [a] => [a] -> Int
       g x = let v = wob x
             in v
  and suppose we have -XNoMonoLocalBinds, so that we attempt to find the most
  general type for 'v'.  When generalising v's type we'll simplify its
  Q [alpha] constraint, but we don't have Q [a] in the 'givens', so we
  will use the instance declaration after all. #11948 was a case
  in point.

All of this is disgustingly delicate, so to discourage people from writing
simplifiable class givens, we warn about signatures that contain them;
see GHC.Tc.Validity Note [Simplifiable given constraints].

Note [Naturally coherent classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A few built-in classes are "naturally coherent".  This term means that
the "instance" for the class is bidirectional with its superclass(es).
For example, consider (~~), which behaves as if it was defined like
this:
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
(See Note [The equality types story] in GHC.Builtin.Types.Prim.)

Faced with [W] t1 ~~ t2, it's always OK to reduce it to [W] t1 ~# t2,
without worrying about Note [Instance and Given overlap].  Why?  Because
if we had [G] s1 ~~ s2, then we'd get the superclass [G] s1 ~# s2, and
so the reduction of the [W] constraint does not risk losing any solutions.

On the other hand, it can be fatal to /fail/ to reduce such
equalities, on the grounds of Note [Instance and Given overlap],
because many good things flow from [W] t1 ~# t2.

The same reasoning applies to

* (~~)        heqTyCon
* (~)         eqTyCon
* Coercible   coercibleTyCon

And less obviously to:

* Tuple classes.  For reasons described in GHC.Tc.Solver.Types
  Note [Tuples hiding implicit parameters], we may have a constraint
     [W] (?x::Int, C a)
  with an exactly-matching Given constraint.  We must decompose this
  tuple and solve the components separately, otherwise we won't solve
  it at all!  It is perfectly safe to decompose it, because again the
  superclasses invert the instance;  e.g.
      class (c1, c2) => (% c1, c2 %)
      instance (c1, c2) => (% c1, c2 %)
  Example in #14218

Examples: T5853, T10432, T5315, T9222, T2627b, T3028b

PS: the term "naturally coherent" doesn't really seem helpful.
Perhaps "invertible" or something?  I left it for now though.

Note [Local instances and incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall b c. (Eq b, forall a. Eq a => Eq (c a))
                 => c b -> Bool
   f x = x==x

We get [W] Eq (c b), and we must use the local instance to solve it.

BUT that wanted also unifies with the top-level Eq [a] instance,
and Eq (Maybe a) etc.  We want the local instance to "win", otherwise
we can't solve the wanted at all.  So we mark it as Incohherent.
According to Note [Rules for instance lookup] in GHC.Core.InstEnv, that'll
make it win even if there are other instances that unify.

Moreover this is not a hack!  The evidence for this local instance
will be constructed by GHC at a call site... from the very instances
that unify with it here.  It is not like an incoherent user-written
instance which might have utterly different behaviour.

Consider  f :: Eq a => blah.  If we have [W] Eq a, we certainly
get it from the Eq a context, without worrying that there are
lots of top-level instances that unify with [W] Eq a!  We'll use
those instances to build evidence to pass to f. That's just the
nullary case of what's happening here.
-}

matchLocalInst :: TcPredType -> CtLoc -> TcS ClsInstResult
-- Look up the predicate in Given quantified constraints,
-- which are effectively just local instance declarations.
matchLocalInst pred loc
  = do { inerts@(IS { inert_cans = ics }) <- getTcSInerts
       ; case match_local_inst inerts (inert_insts ics) of
          { ([], []) -> do { traceTcS "No local instance for" (ppr pred)
                           ; return NoInstance }
          ; (matches, unifs) ->
    do { matches <- mapM mk_instDFun matches
       ; unifs   <- mapM mk_instDFun unifs
         -- See Note [Use only the best matching quantified constraint]
       ; case dominatingMatch matches of
          { Just (dfun_id, tys, theta)
            | all ((theta `impliedBySCs`) . thdOf3) unifs
            ->
            do { let result = OneInst { cir_new_theta   = theta
                                      , cir_mk_ev       = evDFunApp dfun_id tys
                                      , cir_coherence   = IsCoherent
                                      , cir_what        = LocalInstance }
               ; traceTcS "Best local instance found:" $
                  vcat [ text "pred:" <+> ppr pred
                       , text "result:" <+> ppr result
                       , text "matches:" <+> ppr matches
                       , text "unifs:" <+> ppr unifs ]
               ; return result }

          ; mb_best ->
            do { traceTcS "Multiple local instances; not committing to any"
                  $ vcat [ text "pred:" <+> ppr pred
                         , text "matches:" <+> ppr matches
                         , text "unifs:" <+> ppr unifs
                         , text "best_match:" <+> ppr mb_best ]
               ; return NotSure }}}}}
  where
    pred_tv_set = tyCoVarsOfType pred

    mk_instDFun :: (CtEvidence, [DFunInstType]) -> TcS InstDFun
    mk_instDFun (ev, tys) =
      let dfun_id = ctEvEvId ev
      in do { (tys, theta) <- instDFunType (ctEvEvId ev) tys
            ; return (dfun_id, tys, theta) }

    -- Compute matching and unifying local instances
    match_local_inst :: InertSet
                     -> [QCInst]
                     -> ( [(CtEvidence, [DFunInstType])]
                        , [(CtEvidence, [DFunInstType])] )
    match_local_inst _inerts []
      = ([], [])
    match_local_inst inerts (qci@(QCI { qci_tvs  = qtvs
                                      , qci_pred = qpred
                                      , qci_ev   = qev })
                            :qcis)
      | let in_scope = mkInScopeSet (qtv_set `unionVarSet` pred_tv_set)
      , Just tv_subst <- ruleMatchTyKiX qtv_set (mkRnEnv2 in_scope)
                                        emptyTvSubstEnv qpred pred
      , let match = (qev, map (lookupVarEnv tv_subst) qtvs)
      = (match:matches, unifs)

      | otherwise
      = assertPpr (disjointVarSet qtv_set (tyCoVarsOfType pred))
                  (ppr qci $$ ppr pred)
            -- ASSERT: unification relies on the
            -- quantified variables being fresh
        (matches, this_unif `combine` unifs)
      where
        qloc = ctEvLoc qev
        qtv_set = mkVarSet qtvs
        (matches, unifs) = match_local_inst inerts qcis
        this_unif
          | Just subst <- mightEqualLater inerts qpred qloc pred loc
          = Just (qev, map  (lookupTyVar subst) qtvs)
          | otherwise
          = Nothing

        combine Nothing  us = us
        combine (Just u) us = u : us

-- | Instance dictionary function and type.
type InstDFun = (DFunId, [TcType], TcThetaType)

-- | Try to find a local quantified instance that dominates all others,
-- i.e. which has a weaker instance context than all the others.
--
-- See Note [Use only the best matching quantified constraint].
dominatingMatch :: [InstDFun] -> Maybe InstDFun
dominatingMatch matches =
  listToMaybe $ mapMaybe (uncurry go) (holes matches)
  -- listToMaybe: arbitrarily pick any one context that is weaker than
  -- all others, e.g. so that we can handle [Eq a, Num a] vs [Num a, Eq a]
  -- (see test case T22223).

  where
    go :: InstDFun -> [InstDFun] -> Maybe InstDFun
    go this [] = Just this
    go this@(_,_,this_theta) ((_,_,other_theta):others)
      | this_theta `impliedBySCs` other_theta
      = go this others
      | otherwise
      = Nothing

-- | Whether a collection of constraints is implied by another collection,
-- according to a simple superclass check.
--
-- See Note [When does a quantified instance dominate another?].
impliedBySCs :: TcThetaType -> TcThetaType -> Bool
impliedBySCs c1 c2 = all in_c2 c1
  where
    in_c2 :: TcPredType -> Bool
    in_c2 pred = any (pred `tcEqType`) c2_expanded

    c2_expanded :: [TcPredType]  -- Includes all superclasses
    c2_expanded = [ q | p <- c2, q <- p : transSuperClasses p ]


{- Note [When does a quantified instance dominate another?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When matching local quantified instances, it's useful to be able to pick
the one with the weakest precondition, e.g. if one has both

  [G] d1: forall a b. ( Eq a, Num b, C a b  ) => D a b
  [G] d2: forall a  .                C a Int  => D a Int
  [W] {w}: D a Int

Then it makes sense to use d2 to solve w, as doing so we end up with a strictly
weaker proof obligation of `C a Int`, compared to `(Eq a, Num Int, C a Int)`
were we to use d1.

In theory, to compute whether one context implies another, we would need to
recursively invoke the constraint solver. This is expensive, so we instead do
a simple check using superclasses, implemented in impliedBySCs.

Examples:

 - [Eq a] is implied by [Ord a]
 - [Ord a] is not implied by [Eq a],
 - any context is implied by itself,
 - the empty context is implied by any context.

Note [Use only the best matching quantified constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#20582) the ambiguity check for
  (forall a. Ord (m a), forall a. Semigroup a => Eq (m a)) => m Int

Because of eager expansion of given superclasses, we get
  [G] d1: forall a. Ord (m a)
  [G] d2: forall a. Eq (m a)
  [G] d3: forall a. Semigroup a => Eq (m a)

  [W] {w1}: forall a. Ord (m a)
  [W] {w2}: forall a. Semigroup a => Eq (m a)

The first wanted is solved straightforwardly. But the second wanted
matches *two* local instances: d2 and d3. Our general rule around multiple local
instances is that we refuse to commit to any of them. However, that
means that our type fails the ambiguity check. That's bad: the type
is perfectly fine. (This actually came up in the wild, in the streamly
library.)

The solution is to prefer local instances which are easier to prove, meaning
that they have a weaker precondition. In this case, the empty context
of d2 is a weaker constraint than the "Semigroup a" context of d3, so we prefer
using it when proving w2. This allows us to pass the ambiguity check here.

Our criterion for solving a Wanted by matching local quantified instances is
thus as follows:

  - There is a matching local quantified instance that dominates all others
    matches, in the sense of [When does a quantified instance dominate another?].
    Any such match do, we pick it arbitrarily (the T22223 example below says why).
  - This local quantified instance also dominates all the unifiers, as we
    wouldn't want to commit to a single match when we might have multiple,
    genuinely different matches after further unification takes place.

Some other examples:


  #15244:

    f :: (C g, D g) => ....
    class S g => C g where ...
    class S g => D g where ...
    class (forall a. Eq a => Eq (g a)) => S g where ...

  Here, in f's RHS, there are two identical quantified constraints
  available, one via the superclasses of C and one via the superclasses
  of D. Given that each implies the other, we pick one arbitrarily.


  #22216:

    class Eq a
    class Eq a => Ord a
    class (forall b. Eq b => Eq (f b)) => Eq1 f
    class (Eq1 f, forall b. Ord b => Ord (f b)) => Ord1 f

  Suppose we have

    [G] d1: Ord1 f
    [G] d2: Eq a
    [W] {w}: Eq (f a)

  Superclass expansion of d1 gives us:

    [G] d3 : Eq1 f
    [G] d4 : forall b. Ord b => Ord (f b)

  expanding d4 and d5 gives us, respectively:

    [G] d5 : forall b. Eq  b => Eq (f b)
    [G] d6 : forall b. Ord b => Eq (f b)

  Now we have two matching local instances that we could use when solving the
  Wanted. However, it's obviously silly to use d6, given that d5 provides us with
  as much information, with a strictly weaker precondition. So we pick d5 to solve
  w. If we chose d6, we would get [W] Ord a, which in this case we can't solve.


  #22223:

    [G] forall a b. (Eq a, Ord b) => C a b
    [G] forall a b. (Ord b, Eq a) => C a b
    [W] C x y

  Here we should be free to pick either quantified constraint, as they are
  equivalent up to re-ordering of the constraints in the context.
  See also Note [Do not add duplicate quantified instances]
  in GHC.Tc.Solver.Monad.

Test cases:
  typecheck/should_compile/T20582
  quantified-constraints/T15244
  quantified-constraints/T22216{a,b,c,d,e}
  quantified-constraints/T22223

Historical note: a previous solution was to instead pick the local instance
with the least superclass depth (see Note [Replacement vs keeping]),
but that doesn't work for the example from #22216.


************************************************************************
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
and top-level instances) for functional dependencies, and implicit
parameters, is that we simply produce new Wanted equalities.  So for example

        class D a b | a -> b where ...
    Inert:
        d1 :g D Int Bool
    WorkItem:
        d2 :w D Int alpha

    We generate the extra work item
        cv :w alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
        d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary
using those extra equalities.

If that were the case with the same inert set and work item we might discard
d2 directly:

        cv :w alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b
And:
     [Given]  d1 : C T Int Char
     [Wanted] d2 : C T beta Int
In this case, it's *not even possible* to solve the wanted immediately.
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse:
     [Given] d1 : C T Int beta
     [Wanted] d2: C T beta Int
Then it is solvable, but its very hard to detect this on the spot.

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

Note [Fundeps with instances, and equality orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes a delicate interaction that constrains the orientation of
equalities. This one is about fundeps, but the /exact/ same thing arises for
type-family injectivity constraints: see Note [Improvement orientation].

doTopFundepImprovement compares the constraint with all the instance
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

   ==> {doTopFundepImprovement: compare work_item with instance,
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

    Thus we emit
       [W] T kfresh0 (yfresh0::kfresh0) ~ T kb0 (b0::kb0)
    and /NOT/
       [W] T kb0 (b0::kb0) ~ T kfresh0 (yfresh0::kfresh0)

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
     Preserve orientiation when decomposing!!

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
see Note [Improvement orientation].
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

doTopFundepImprovement :: Ct -> TcS ()
-- Try to functional-dependency improvement between the constraint
-- and the top-level instance declarations
-- See Note [Fundeps with instances, and equality orientation]
-- See also Note [Weird fundeps]
doTopFundepImprovement work_item@(CDictCan { cc_ev = ev, cc_class = cls
                                           , cc_tyargs = xis })
  = do { traceTcS "try_fundeps" (ppr work_item)
       ; instEnvs <- getInstEnvs
       ; let fundep_eqns = improveFromInstEnv instEnvs mk_ct_loc cls xis
       ; emitFunDepWanteds (ctEvRewriters ev) fundep_eqns }
  where
     dict_pred   = mkClassPred cls xis
     dict_loc    = ctEvLoc ev
     dict_origin = ctLocOrigin dict_loc

     mk_ct_loc :: PredType   -- From instance decl
               -> SrcSpan    -- also from instance deol
               -> (CtLoc, RewriterSet)
     mk_ct_loc inst_pred inst_loc
       = ( dict_loc { ctl_origin = FunDepOrigin2 dict_pred dict_origin
                                                 inst_pred inst_loc }
         , emptyRewriterSet )

doTopFundepImprovement work_item = pprPanic "doTopFundepImprovement" (ppr work_item)

