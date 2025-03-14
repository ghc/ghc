
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

-- | Coverage checking step of the
-- [Lower Your Guards paper](https://dl.acm.org/doi/abs/10.1145/3408989).
--
-- Coverage check guard trees (like @'PmMatch' 'Pre'@) to get a
-- 'CheckResult', containing
--
--   1. The set of uncovered values, 'cr_uncov'
--   2. And an annotated tree variant (like @'PmMatch' 'Post'@) that captures
--      redundancy and inaccessibility information as 'RedSets' annotations
--
-- Basically the UA function from Section 5.1, which is an optimised
-- interleaving of U and A from Section 3.2 (Figure 5).
-- The Normalised Refinement Types 'Nablas' are maintained in
-- "GHC.HsToCore.Pmc.Solver".
module GHC.HsToCore.Pmc.Check (
        CheckAction(..),
        checkMatchGroup, checkGRHSs, checkPatBind, checkEmptyCase, checkRecSel
    ) where

import GHC.Prelude

import GHC.Builtin.Names ( hasKey, considerAccessibleIdKey, trueDataConKey )
import GHC.HsToCore.Monad ( DsM )
import GHC.HsToCore.Pmc.Types
import GHC.HsToCore.Pmc.Utils
import GHC.HsToCore.Pmc.Solver
import GHC.Driver.DynFlags
import GHC.Utils.Outputable
import GHC.Tc.Utils.TcType (evVarPred)
import GHC.Data.OrdList
import GHC.Data.Bag

import qualified Data.Semigroup as Semi
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Coerce
import GHC.Types.Var
import GHC.Core
import GHC.Core.Utils

-- | Coverage checking action. Can be composed 'leftToRight' or 'topToBottom'.
newtype CheckAction a = CA { unCA :: Nablas -> DsM (CheckResult a) }
  deriving Functor

-- | A 'CheckAction' representing a successful pattern-match.
matchSucceeded :: CheckAction RedSets
matchSucceeded = CA $ \inc -> -- succeed
  pure CheckResult { cr_ret = emptyRedSets { rs_cov = inc }
                   , cr_uncov = mempty
                   , cr_approx = Precise }

-- | Composes 'CheckAction's top-to-bottom:
-- If a value falls through the resulting action, then it must fall through the
-- first action and then through the second action.
-- If a value matches the resulting action, then it either matches the
-- first action or matches the second action.
-- Basically the semantics of the LYG branching construct.
topToBottom :: ((Nablas -> (Precision, Nablas)) -> top -> bot -> (Precision, ret))
            -> CheckAction top
            -> CheckAction bot
            -> CheckAction ret
topToBottom f (CA top) (CA bot) = CA $ \inc -> do
  t <- top inc
  b <- bot (cr_uncov t)
  limit <- maxPmCheckModels <$> getDynFlags
  -- See Note [Countering exponential blowup]
  let throttler cov = throttle limit inc cov
  let (prec', ret) = f throttler (cr_ret t) (cr_ret b)
  pure CheckResult { cr_ret = ret
                   , cr_uncov = cr_uncov b
                   , cr_approx = prec' Semi.<> cr_approx t Semi.<> cr_approx b }


-- | Composes 'CheckAction's left-to-right:
-- If a value falls through the resulting action, then it either falls through the
-- first action or through the second action.
-- If a value matches the resulting action, then it must match the first action
-- and then match the second action.
-- Basically the semantics of the LYG guard construct.
leftToRight :: (RedSets -> right -> ret)
            -> CheckAction RedSets
            -> CheckAction right
            -> CheckAction ret
leftToRight f (CA left) (CA right) = CA $ \inc -> do
  l <- left inc
  r <- right (rs_cov (cr_ret l))
  limit <- maxPmCheckModels <$> getDynFlags
  let uncov = cr_uncov l Semi.<> cr_uncov r
  -- See Note [Countering exponential blowup]
  let (prec', uncov') = throttle limit inc uncov
  pure CheckResult { cr_ret = f (cr_ret l) (cr_ret r)
                   , cr_uncov = uncov'
                   , cr_approx = prec' Semi.<> cr_approx l Semi.<> cr_approx r }

-- | @throttle limit old new@ returns @old@ if the number of 'Nabla's in @new@
-- is exceeding the given @limit@ and the @old@ number of 'Nabla's.
-- See Note [Countering exponential blowup].
throttle :: Int -> Nablas -> Nablas -> (Precision, Nablas)
throttle limit old@(MkNablas old_ds) new@(MkNablas new_ds)
  --- | pprTrace "PmCheck:throttle" (ppr (length old_ds) <+> ppr (length new_ds) <+> ppr limit) False = undefined
  | length new_ds > max limit (length old_ds) = (Approximate, old)
  | otherwise                                 = (Precise,     new)

checkAlternatives :: (grdtree -> CheckAction anntree) -> NonEmpty grdtree -> CheckAction (NonEmpty anntree)
-- The implementation is pretty similar to
-- @traverse1 :: Apply f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)@
checkAlternatives act (t :| [])       = (:| []) <$> act t
checkAlternatives act (t1 :| (t2:ts)) =
  topToBottom (no_throttling (NE.<|)) (act t1) (checkAlternatives act (t2:|ts))
  where
    no_throttling f _throttler t b = (Precise, f t b)

emptyRedSets :: RedSets
-- Semigroup instance would be misleading!
emptyRedSets = RedSets mempty mempty mempty

checkGrd :: PmGrd -> CheckAction RedSets
checkGrd grd = CA $ \inc -> case grd of
  -- let x = e: Refine with x ~ e
  PmLet x e -> do
    matched <- addPhiCtNablas inc (PhiCoreCt x e)
    tracePm "check:Let" (ppr x <+> char '=' <+> ppr e)
    pure CheckResult { cr_ret = emptyRedSets { rs_cov = matched }
                     , cr_uncov = mempty
                     , cr_approx = Precise }
  -- Bang x _: Diverge on x ~ ⊥, refine with x ≁ ⊥
  PmBang x mb_info -> do
    div <- addPhiCtNablas inc (PhiBotCt x)
    matched <- addPhiCtNablas inc (PhiNotBotCt x)
    -- See Note [Dead bang patterns]
    -- mb_info = Just info <==> PmBang originates from bang pattern in source
    let bangs | Just info <- mb_info = unitOL (div, info)
              | otherwise            = NilOL
    tracePm "check:Bang" (ppr x <+> ppr div)
    pure CheckResult { cr_ret = RedSets { rs_cov = matched, rs_div = div, rs_bangs = bangs }
                     , cr_uncov = mempty
                     , cr_approx = Precise }
  -- See point (3) of Note [considerAccessible]
  PmCon x (PmAltConLike con) _ _ _
    | x `hasKey` considerAccessibleIdKey
    , con `hasKey` trueDataConKey
    -> pure CheckResult { cr_ret = emptyRedSets { rs_cov = initNablas }
                        , cr_uncov = mempty
                        , cr_approx = Precise }
  -- Con: Fall through on x ≁ K and refine with x ~ K ys and type info
  PmCon x con tvs dicts args -> do
    !div <- if isPmAltConMatchStrict con
      then addPhiCtNablas inc (PhiBotCt x)
      else pure mempty
    !matched <- addPhiCtNablas inc (PhiConCt x con tvs (map evVarPred dicts) args)
    !uncov   <- addPhiCtNablas inc (PhiNotConCt x con)
    tracePm "check:Con" $ vcat
      [ ppr grd
      , ppr inc
      , hang (text "div") 2 (ppr div)
      , hang (text "matched") 2 (ppr matched)
      , hang (text "uncov") 2 (ppr uncov)
      ]
    pure CheckResult { cr_ret = emptyRedSets { rs_cov = matched, rs_div = div }
                     , cr_uncov = uncov
                     , cr_approx = Precise }


checkGrdDag :: GrdDag -> CheckAction RedSets
checkGrdDag (GdOne g)     = checkGrd g
checkGrdDag GdEnd         = matchSucceeded
checkGrdDag (GdSeq dl dr) = leftToRight merge (checkGrdDag dl) (checkGrdDag dr)
  where
    -- Note that
    --   * the incoming set of dr is the covered set of dl
    --   * the covered set of dr is a subset of the incoming set of dr
    --   * this is so that the covered set of dr is the covered set of the
    --     entire sequence
    -- Hence we merge by returning @rs_cov ri_r@ as the covered set.
    merge ri_l ri_r =
      RedSets { rs_cov   = rs_cov   ri_r
              , rs_div   = rs_div   ri_l Semi.<> rs_div   ri_r
              , rs_bangs = rs_bangs ri_l Semi.<> rs_bangs ri_r }
checkGrdDag (GdAlt dt db) = topToBottom merge (checkGrdDag dt) (checkGrdDag db)
  where
    -- The intuition here: ri_b is disjoint with ri_t, because db only gets
    -- fed the "leftover" uncovered set of dt. But for the GrdDag that follows
    -- to the right of the GdAlt (say), we have to reunite the RedSets. Hence
    -- component-wise merge.
    -- After the GdAlt, we unite the covered sets. If they become too large, we
    -- throttle, continuing with the incoming set.
    merge throttler ri_t ri_b =
      let (prec, cov) = throttler (rs_cov ri_t Semi.<> rs_cov ri_b) in
      (prec, RedSets { rs_cov   = cov
                     , rs_div   = rs_div   ri_t Semi.<> rs_div   ri_b
                     , rs_bangs = rs_bangs ri_t Semi.<> rs_bangs ri_b })

checkMatchGroup :: PmMatchGroup Pre -> CheckAction (PmMatchGroup Post)
checkMatchGroup (PmMatchGroup matches) =
  PmMatchGroup <$> checkAlternatives checkMatch matches

checkMatch :: PmMatch Pre -> CheckAction (PmMatch Post)
checkMatch (PmMatch { pm_pats = grds, pm_grhss = grhss }) =
  leftToRight PmMatch (checkGrdDag grds) (checkGRHSs grhss)

checkGRHSs :: PmGRHSs Pre -> CheckAction (PmGRHSs Post)
checkGRHSs (PmGRHSs { pgs_lcls = lcls, pgs_grhss = grhss }) =
  leftToRight PmGRHSs (checkGrdDag lcls) (checkAlternatives checkGRHS grhss)

checkGRHS :: PmGRHS Pre -> CheckAction (PmGRHS Post)
checkGRHS (PmGRHS { pg_grds = grds, pg_rhs = rhs_info }) =
  flip PmGRHS rhs_info <$> checkGrdDag grds

checkEmptyCase :: PmEmptyCase -> CheckAction PmEmptyCase
-- See Note [Checking EmptyCase]
checkEmptyCase pe@(PmEmptyCase { pe_var = var }) = CA $ \inc -> do
  unc <- addPhiCtNablas inc (PhiNotBotCt var)
  pure CheckResult { cr_ret = pe, cr_uncov = unc, cr_approx = mempty }

checkPatBind :: (PmPatBind Pre) -> CheckAction (PmPatBind Post)
checkPatBind = coerce checkGRHS

checkRecSel :: PmRecSel () -> CheckAction (PmRecSel Id)
-- See Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
checkRecSel pr@(PmRecSel { pr_arg = arg, pr_cons = cons }) = CA $ \inc -> do
  arg_id <- case arg of
           Var arg_id -> return arg_id
           _ -> mkPmId $ exprType arg

  let con_cts = map (PhiNotConCt arg_id . PmAltConLike) cons
      arg_ct  = PhiCoreCt arg_id arg
      phi_cts = listToBag (arg_ct : con_cts)
  unc <- addPhiCtsNablas inc phi_cts
  pure CheckResult { cr_ret = pr{ pr_arg_var = arg_id }, cr_uncov = unc, cr_approx = mempty }


{- Note [Checking EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-XEmptyCase is useful for matching on empty data types like 'Void'. For example,
the following is a complete match:

    f :: Void -> ()
    f x = case x of {}

Really, -XEmptyCase is the only way to write a program that at the same time is
safe (@f _ = error "boom"@ is not because of ⊥), doesn't trigger a warning
(@f !_ = error "inaccessible" has inaccessible RHS) and doesn't turn an
exception into divergence (@f x = f x@).

Semantically, unlike every other case expression, -XEmptyCase is strict in its
match var x, which rules out ⊥ as an inhabitant. So we add x ≁ ⊥ to the
initial Nabla and check if there are any values left to match on.

Note [Dead bang patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f :: Bool -> Int
  f True = 1
  f !x   = 2

Whenever we fall through to the second equation, we will already have evaluated
the argument. Thus, the bang pattern serves no purpose and should be warned
about. We call this kind of bang patterns "dead". Dead bangs are the ones
that under no circumstances can force a thunk that wasn't already forced.
Dead bangs are a form of redundant bangs; see below.

We can detect dead bang patterns by checking whether @x ~ ⊥@ is satisfiable
where the PmBang appears in 'checkGrd'. If not, then clearly the bang is
dead. So for a source bang, we add the refined Nabla and the source info to
the 'RedSet's 'rs_bangs'. When collecting stuff to warn, we test that Nabla for
inhabitants. If it's empty, we'll warn that it's redundant.

Note that we don't want to warn for a dead bang that appears on a redundant
clause. That is because in that case, we recommend to delete the clause wholly,
including its leading pattern match.

Dead bang patterns are redundant. But there are bang patterns which are
redundant that aren't dead, for example

  f !() = 0

the bang still forces the match variable, before we attempt to match on (). But
it is redundant with the forcing done by the () match. We currently don't
detect redundant bangs that aren't dead.

Note [Countering exponential blowup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Precise pattern match exhaustiveness checking is necessarily exponential in
the size of some input programs. We implement a counter-measure in the form of
the -fmax-pmcheck-models flag, limiting the number of Nablas we check against
each pattern by a constant.

How do we do that? Consider

  f True True = ()
  f True True = ()

And imagine we set our limit to 1 for the sake of the example. The first clause
will be checked against the initial Nabla, {}. Doing so will produce an
Uncovered set of size 2, containing the models {x≁True} and {x~True,y≁True}.
Also we find the first clause to cover the model {x~True,y~True}.

But the Uncovered set we get out of the match is too large! We somehow have to
ensure not to make things worse than they are already, so we continue checking
with a singleton Uncovered set of the initial Nabla {}. Why is this
sound (wrt. the notion in GADTs Meet Their Match)? Well, it basically amounts
to forgetting that we matched against the first clause. The values represented
by {} are a superset of those represented by its two refinements {x≁True} and
{x~True,y≁True}.

This forgetfulness becomes very apparent in the example above: By continuing
with {} we don't detect the second clause as redundant, as it again covers the
same non-empty subset of {}. So we don't flag everything as redundant anymore,
but still will never flag something as redundant that isn't.

For exhaustivity, the converse applies: We will report @f@ as non-exhaustive
and report @f _ _@ as missing, which is a superset of the actual missing
matches. But soundness means we will never fail to report a missing match.

This mechanism is implemented in 'throttle'.

Guards are an extreme example in this regard, with #11195 being a particularly
dreadful example: Since their RHS are often pretty much unique, we split on a
variable (the one representing the RHS) that doesn't occur anywhere else in the
program, so we don't actually get useful information out of that split!
We counter this by throttling *Uncovered* sets in `leftToRight`.

Another challenge is posed by or-patterns (see also Note [Implementation of OrPatterns]):
Large matches such as `f (LT; GT) (LT; GT) .... True = 1` will desugar into
a long sequence of `GdAlt LT GT`. The careless desugaring of `GdAlt` via
`topToBottom` would cause ever enlarging *Covered* sets.
So we throttle when merging Covered sets from LT and GT, by using the original
incoming covered set. The effect is very like replacing (LT; GT) with a wildcard
pattern _.

Note [considerAccessible]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (T18610)

  f :: Bool -> Int
  f x = case (x, x) of
    (True,  True)  -> 1
    (False, False) -> 2
    (True,  False) -> 3 -- Warning: Redundant

The third case is detected as redundant. But it may be the intent of the
programmer to keep the dead code, in order for it not to bitrot or to support
debugging scenarios. But there is no way to communicate that to the
pattern-match checker! The only way is to deactivate pattern-match checking
whole-sale, which is quite annoying. Hence, we define in "GHC.Exts":

  considerAccessible = True

'considerAccessible' is treated specially by the pattern-match checker in that a
guard with it as the scrutinee expression will keep its parent clause alive:

  g :: Bool -> Int
  g x = case (x, x) of
    (True,  True)  -> 1
    (False, False) -> 2
    (True,  False) | GHC.Exts.considerAccessible -> 3 -- No warning

The key bits of the implementation are:

  1. Its definition is recognised as known-key (see "GHC.Builtin.Names").
  2. After "GHC.HsToCore.Pmc.Desugar", the guard will end up as a 'PmCon', where
     the match var is the known-key 'considerAccessible' and the constructor
     against which it matches is 'True'.
  3. We recognise the 'PmCon' in 'GHC.HsToCore.Check.checkGrd' and inflate the
     incoming set of values for all guards downstream to the unconstrained
     'initNablas' set, e.g. /all/ values.
     (The set of values that falls through that particular guard is empty, as
     matching 'considerAccessible' against 'True' can't fail.)

Note that 'considerAccessible' breaks the invariant that incoming sets of values
reaching syntactic children are subsets of that of the syntactic ancestor:
A whole match, like that of the third clause of the example, might have no
incoming value, but its single RHS has incoming values because of (3).

That means the 'is_covered' flag computed in 'GHC.HsToCore.Pmc.cirbsMatch'
is irrelevant and should not be used to flag all children as redundant (which is
what we used to do).

We achieve great benefits with a very simple implementation.
There are caveats, though:

  (A) Putting potentially failing guards /after/ the
      'considerAccessible' guard might lead to weird check results, e.g.,

        h :: Bool -> Int
        h x = case (x, x) of
          (True,  True)  -> 1
          (False, False) -> 2
          (True,  False) | GHC.Exts.considerAccessible, False <- x -> 3
          -- Warning: Not matched: (_, _)

      That *is* fixable, although we would pay with a much more complicated
      implementation.
  (B) If the programmer puts a 'considerAccessible' marker on an accessible
      clause, the checker doesn't warn about it. E.g.,

        f :: Bool -> Int
        f True | considerAccessible = 0
        f False = 1

      will not emit any warning whatsoever. We could implement code that warns
      here, but it wouldn't be as simple as it is now.
-}
