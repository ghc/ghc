{-# LANGUAGE ScopedTypeVariables #-}
-- | Reordering or pruning the tree in order to prefer or make certain choices.
module Distribution.Solver.Modular.Preference
    ( avoidReinstalls
    , deferSetupChoices
    , deferWeakFlagChoices
    , enforceManualFlags
    , enforcePackageConstraints
    , enforceSingleInstanceRestriction
    , firstGoal
    , preferBaseGoalChoice
    , preferLinked
    , preferPackagePreferences
    , preferReallyEasyGoalChoices
    , requireInstalled
    , sortGoals
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.Reader hiding (sequence)
import Data.Traversable (sequence)

import Distribution.PackageDescription (lookupFlagAssignment, unFlagAssignment) -- from Cabal

import Distribution.Solver.Types.Flag
import Distribution.Solver.Types.InstalledPreference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.Variable

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W

-- | Update the weights of children under 'PChoice' nodes. 'addWeights' takes a
-- list of weight-calculating functions in order to avoid sorting the package
-- choices multiple times. Each function takes the package name, sorted list of
-- children's versions, and package option. 'addWeights' prepends the new
-- weights to the existing weights, which gives precedence to preferences that
-- are applied later.
addWeights :: [PN -> [Ver] -> POption -> Weight] -> Tree d c -> Tree d c
addWeights fs = trav go
  where
    go :: TreeF d c (Tree d c) -> TreeF d c (Tree d c)
    go (PChoiceF qpn@(Q _ pn) rdm x cs) =
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
          weights k = [f pn sortedVersions k | f <- fs]

          elemsToWhnf :: [a] -> ()
          elemsToWhnf = foldr seq ()
      in  PChoiceF qpn rdm x
          -- Evaluate the children's versions before evaluating any of the
          -- subtrees, so that 'sortedVersions' doesn't hold onto all of the
          -- subtrees (referenced by cs) and cause a space leak.
          (elemsToWhnf sortedVersions `seq`
             W.mapWeightsWithKey (\k w -> weights k ++ w) cs)
    go x                            = x

addWeight :: (PN -> [Ver] -> POption -> Weight) -> Tree d c -> Tree d c
addWeight f = addWeights [f]

version :: POption -> Ver
version (POption (I v _) _) = v

-- | Prefer to link packages whenever possible.
preferLinked :: Tree d c -> Tree d c
preferLinked = addWeight (const (const linked))
  where
    linked (POption _ Nothing)  = 1
    linked (POption _ (Just _)) = 0

-- Works by setting weights on choice nodes. Also applies stanza preferences.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree d c -> Tree d c
preferPackagePreferences pcs =
    preferPackageStanzaPreferences pcs .
    addWeights [
          \pn _  opt -> preferred pn opt

        -- Note that we always rank installed before uninstalled, and later
        -- versions before earlier, but we can change the priority of the
        -- two orderings.
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> installed opt
                          PreferLatest    -> latest vs opt
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> latest vs opt
                          PreferLatest    -> installed opt
        ]
  where
    -- Prefer packages with higher version numbers over packages with
    -- lower version numbers.
    latest :: [Ver] -> POption -> Weight
    latest sortedVersions opt =
      let l = length sortedVersions
          index = fromMaybe l $ L.findIndex (<= version opt) sortedVersions
      in  fromIntegral index / fromIntegral l

    preference :: PN -> InstalledPreference
    preference pn =
      let PackagePreferences _ ipref _ = pcs pn
      in  ipref

    -- | Prefer versions satisfying more preferred version ranges.
    preferred :: PN -> POption -> Weight
    preferred pn opt =
      let PackagePreferences vrs _ _ = pcs pn
      in fromIntegral . negate . L.length $
         L.filter (flip checkVR (version opt)) vrs

    -- Prefer installed packages over non-installed packages.
    installed :: POption -> Weight
    installed (POption (I _ (Inst _)) _) = 0
    installed _                          = 1

-- | Traversal that tries to establish package stanza enable\/disable
-- preferences. Works by reordering the branches of stanza choices.
preferPackageStanzaPreferences :: (PN -> PackagePreferences) -> Tree d c -> Tree d c
preferPackageStanzaPreferences pcs = trav go
  where
    go (SChoiceF qsn@(SN (Q pp pn) s) rdm gr _tr ts)
      | primaryPP pp && enableStanzaPref pn s =
          -- move True case first to try enabling the stanza
          let ts' = W.mapWeightsWithKey (\k w -> weight k : w) ts
              weight k = if k then 0 else 1
          -- defer the choice by setting it to weak
          in  SChoiceF qsn rdm gr (WeakOrTrivial True) ts'
    go x = x

    enableStanzaPref :: PN -> OptionalStanza -> Bool
    enableStanzaPref pn s =
      let PackagePreferences _ _ spref = pcs pn
      in  s `elem` spref

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: forall d c. QPN
                          -> ConflictSet
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintP qpn c i (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go i prop
    else r
  where
    go :: I -> PackageProperty -> Tree d c
    go (I v _) (PackagePropertyVersion vr)
        | checkVR vr v  = r
        | otherwise     = Fail c (GlobalConstraintVersion vr src)
    go _       PackagePropertyInstalled
        | instI i       = r
        | otherwise     = Fail c (GlobalConstraintInstalled src)
    go _       PackagePropertySource
        | not (instI i) = r
        | otherwise     = Fail c (GlobalConstraintSource src)
    go _       _        = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintF :: forall d c. QPN
                          -> Flag
                          -> ConflictSet
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintF qpn f c b' (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go prop
    else r
  where
    go :: PackageProperty -> Tree d c
    go (PackagePropertyFlags fa) =
        case lookupFlagAssignment f fa of
          Nothing            -> r
          Just b | b == b'   -> r
                 | otherwise -> Fail c (GlobalConstraintFlag src)
    go _                             = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintS :: forall d c. QPN
                          -> OptionalStanza
                          -> ConflictSet
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintS qpn s c b' (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go prop
    else r
  where
    go :: PackageProperty -> Tree d c
    go (PackagePropertyStanzas ss) =
        if not b' && s `elem` ss then Fail c (GlobalConstraintFlag src)
                                 else r
    go _                               = r

-- | Traversal that tries to establish various kinds of user constraints. Works
-- by selectively disabling choices that have been ruled out by global user
-- constraints.
enforcePackageConstraints :: M.Map PN [LabeledPackageConstraint]
                          -> Tree d c
                          -> Tree d c
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q _ pn) rdm gr                    ts) =
      let c = varToConflictSet (P qpn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP qpn c i pc)
                                       id
                                       (M.findWithDefault [] pn pcs)
      in PChoiceF qpn rdm gr        (W.mapWithKey g ts)
    go (FChoiceF qfn@(FN qpn@(Q _ pn) f) rdm gr tr m d ts) =
      let c = varToConflictSet (F qfn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF qpn f c b pc)
                           id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn rdm gr tr m d (W.mapWithKey g ts)
    go (SChoiceF qsn@(SN qpn@(Q _ pn) f) rdm gr tr   ts) =
      let c = varToConflictSet (S qsn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintS qpn f c b pc)
                           id
                           (M.findWithDefault [] pn pcs)
      in SChoiceF qsn rdm gr tr     (W.mapWithKey g ts)
    go x = x

-- | Transformation that tries to enforce the rule that manual flags can only be
-- set by the user.
--
-- If there are no constraints on a manual flag, this function prunes all but
-- the default value. If there are constraints, then the flag is allowed to have
-- the values specified by the constraints. Note that the type used for flag
-- values doesn't need to be Bool.
--
-- This function makes an exception for the case where there are multiple goals
-- for a single package (with different qualifiers), and flag constraints for
-- manual flag x only apply to some of those goals. In that case, we allow the
-- unconstrained goals to use the default value for x OR any of the values in
-- the constraints on x (even though the constraints don't apply), in order to
-- allow the unconstrained goals to be linked to the constrained goals. See
-- https://github.com/haskell/cabal/issues/4299. Removing the single instance
-- restriction (SIR) would also fix #4299, so we may want to remove this
-- exception and only let the user toggle manual flags if we remove the SIR.
--
-- This function does not enforce any of the constraints, since that is done by
-- 'enforcePackageConstraints'.
enforceManualFlags :: M.Map PN [LabeledPackageConstraint] -> Tree d c -> Tree d c
enforceManualFlags pcs = trav go
  where
    go (FChoiceF qfn@(FN (Q _ pn) fn) rdm gr tr Manual d ts) =
        FChoiceF qfn rdm gr tr Manual d $
          let -- A list of all values specified by constraints on 'fn'.
              -- We ignore the constraint scope in order to handle issue #4299.
              flagConstraintValues :: [Bool]
              flagConstraintValues =
                  [ flagVal
                  | let lpcs = M.findWithDefault [] pn pcs
                  , (LabeledPackageConstraint (PackageConstraint _ (PackagePropertyFlags fa)) _) <- lpcs
                  , (fn', flagVal) <- unFlagAssignment fa
                  , fn' == fn ]

              -- Prune flag values that are not the default and do not match any
              -- of the constraints.
              restrictToggling :: Eq a => a -> [a] -> a -> Tree d c -> Tree d c
              restrictToggling flagDefault constraintVals flagVal r =
                  if flagVal `elem` constraintVals || flagVal == flagDefault
                  then r
                  else Fail (varToConflictSet (F qfn)) ManualFlag

      in W.mapWithKey (restrictToggling d flagConstraintValues) ts
    go x                                                            = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree d c -> Tree d c
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) rdm gr cs)
      | p pn      = PChoiceF v rdm gr (W.mapWithKey installed cs)
      | otherwise = PChoiceF v rdm gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (varToConflictSet (P v)) CannotInstall
    go x          = x

-- | Avoid reinstalls.
--
-- This is a tricky strategy. If a package version is installed already and the
-- same version is available from a repo, the repo version will never be chosen.
-- This would result in a reinstall (either destructively, or potentially,
-- shadowing). The old instance won't be visible or even present anymore, but
-- other packages might have depended on it.
--
-- TODO: It would be better to actually check the reverse dependencies of installed
-- packages. If they're not depended on, then reinstalling should be fine. Even if
-- they are, perhaps this should just result in trying to reinstall those other
-- packages as well. However, doing this all neatly in one pass would require to
-- change the builder, or at least to change the goal set after building.
avoidReinstalls :: (PN -> Bool) -> Tree d c -> Tree d c
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) rdm gr cs)
      | p pn      = PChoiceF qpn rdm gr disableReinstalls
      | otherwise = PChoiceF qpn rdm gr cs
      where
        disableReinstalls =
          let installed = [ v | (_, POption (I v (Inst _)) _, _) <- W.toList cs ]
          in  W.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (varToConflictSet (P qpn)) CannotReinstall
        notReinstall _ _ x =
          x
    go x          = x

-- | Sort all goals using the provided function.
sortGoals :: (Variable QPN -> Variable QPN -> Ordering) -> Tree d c -> Tree d c
sortGoals variableOrder = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.sortByKeys goalOrder xs)
    go x                    = x

    goalOrder :: Goal QPN -> Goal QPN -> Ordering
    goalOrder = variableOrder `on` (varToVariable . goalToVar)

    varToVariable :: Var QPN -> Variable QPN
    varToVariable (P qpn)                    = PackageVar qpn
    varToVariable (F (FN qpn fn))     = FlagVar qpn fn
    varToVariable (S (SN qpn stanza)) = StanzaVar qpn stanza

-- | Always choose the first goal in the list next, abandoning all
-- other choices.
--
-- This is unnecessary for the default search strategy, because
-- it descends only into the first goal choice anyway,
-- but may still make sense to just reduce the tree size a bit.
firstGoal :: Tree d c -> Tree d c
firstGoal = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.firstOnly xs)
    go x                    = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible by pruning all other goals when base is available. In nearly
-- all cases, there's a single choice for the base package. Also, fixing
-- base early should lead to better error messages.
preferBaseGoalChoice :: Tree d c -> Tree d c
preferBaseGoalChoice = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.filterIfAnyByKeys isBase xs)
    go x                    = x

    isBase :: Goal QPN -> Bool
    isBase (Goal (P (Q _pp pn)) _) = unPN pn == "base"
    isBase _                       = False

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup dependencies against package dependencies when possible
deferSetupChoices :: Tree d c -> Tree d c
deferSetupChoices = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.preferByKeys noSetup xs)
    go x                    = x

    noSetup :: Goal QPN -> Bool
    noSetup (Goal (P (Q (PackagePath _ns (QualSetup _)) _)) _) = False
    noSetup _                                                  = True

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree d c -> Tree d c
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.prefer noWeakFlag (P.prefer noWeakStanza xs))
    go x                    = x

    noWeakStanza :: Tree d c -> Bool
    noWeakStanza (SChoice _ _ _ (WeakOrTrivial True)   _) = False
    noWeakStanza _                                        = True

    noWeakFlag :: Tree d c -> Bool
    noWeakFlag (FChoice _ _ _ (WeakOrTrivial True) _ _ _) = False
    noWeakFlag _                                          = True

-- | Transformation that prefers goals with lower branching degrees.
--
-- When a goal choice node has at least one goal with zero or one children, this
-- function prunes all other goals. This transformation can help the solver find
-- a solution in fewer steps by allowing it to backtrack sooner when it is
-- exploring a subtree with no solutions. However, each step is more expensive.
preferReallyEasyGoalChoices :: Tree d c -> Tree d c
preferReallyEasyGoalChoices = trav go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.filterIfAny zeroOrOneChoices xs)
    go x                    = x

-- | Monad used internally in enforceSingleInstanceRestriction
--
-- For each package instance we record the goal for which we picked a concrete
-- instance. The SIR means that for any package instance there can only be one.
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree d c -> Tree d c
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF d c (EnforceSIR (Tree d c)) -> EnforceSIR (Tree d c)

    -- We just verify package choices.
    go (PChoiceF qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequence (W.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree d c) -> EnforceSIR (Tree d c)
    goP qpn@(Q _ pn) (POption i linkedTo) r = do
      let inst = PI pn i
      env <- ask
      case (linkedTo, M.lookup inst env) of
        (Just _, _) ->
          -- For linked nodes we don't check anything
          r
        (Nothing, Nothing) ->
          -- Not linked, not already used
          local (M.insert inst qpn) r
        (Nothing, Just qpn') -> do
          -- Not linked, already used. This is an error
          return $ Fail (CS.union (varToConflictSet (P qpn)) (varToConflictSet (P qpn'))) MultipleInstances
