{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Solver.Modular.Dependency (
    -- * Variables
    Var(..)
  , showVar
  , varPN
    -- * Conflict sets
  , ConflictSet
  , ConflictMap
  , CS.showConflictSet
    -- * Constrained instances
  , CI(..)
    -- * Flagged dependencies
  , FlaggedDeps
  , FlaggedDep(..)
  , LDep(..)
  , Dep(..)
  , IsExe(..)
  , showDep
  , DependencyReason(..)
  , showDependencyReason
  , flattenFlaggedDeps
  , QualifyOptions(..)
  , qualifyDeps
  , unqualifyDeps
    -- * Reverse dependency map
  , RevDepMap
    -- * Goals
  , Goal(..)
  , GoalReason(..)
  , QGoalReason
  , goalToVar
  , varToConflictSet
  , goalReasonToCS
  , dependencyReasonToCS
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (pi)

import Language.Haskell.Extension (Extension(..), Language(..))

import Distribution.Text

import Distribution.Solver.Modular.ConflictSet (ConflictSet, ConflictMap)
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Var
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS

import Distribution.Solver.Types.ComponentDeps (Component(..))
import Distribution.Solver.Types.PackagePath

{-------------------------------------------------------------------------------
  Constrained instances
-------------------------------------------------------------------------------}

-- | Constrained instance. It represents the allowed instances for a package,
-- which can be either a fixed instance or a version range.
data CI = Fixed I | Constrained VR
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Flagged dependencies
-------------------------------------------------------------------------------}

-- | Flagged dependencies
--
-- 'FlaggedDeps' is the modular solver's view of a packages dependencies:
-- rather than having the dependencies indexed by component, each dependency
-- defines what component it is in.
--
-- Note that each dependency is associated with a Component. We must know what
-- component the dependencies belong to, or else we won't be able to construct
-- fine-grained reverse dependencies.
type FlaggedDeps qpn = [FlaggedDep qpn]

-- | Flagged dependencies can either be plain dependency constraints,
-- or flag-dependent dependency trees.
data FlaggedDep qpn =
    -- | Dependencies which are conditional on a flag choice.
    Flagged (FN qpn) FInfo (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
    -- | Dependencies which are conditional on whether or not a stanza
    -- (e.g., a test suite or benchmark) is enabled.
  | Stanza  (SN qpn)       (TrueFlaggedDeps qpn)
    -- | Dependencies which are always enabled, for the component 'comp'.
  | Simple (LDep qpn) Component

-- | Conversatively flatten out flagged dependencies
--
-- NOTE: We do not filter out duplicates.
flattenFlaggedDeps :: FlaggedDeps qpn -> [(LDep qpn, Component)]
flattenFlaggedDeps = concatMap aux
  where
    aux :: FlaggedDep qpn -> [(LDep qpn, Component)]
    aux (Flagged _ _ t f) = flattenFlaggedDeps t ++ flattenFlaggedDeps f
    aux (Stanza  _   t)   = flattenFlaggedDeps t
    aux (Simple d c)      = [(d, c)]

type TrueFlaggedDeps  qpn = FlaggedDeps qpn
type FalseFlaggedDeps qpn = FlaggedDeps qpn

-- | Is this dependency on an executable
newtype IsExe = IsExe Bool
  deriving (Eq, Show)

-- | A 'Dep' labeled with the reason it was introduced.
--
-- 'LDep' intentionally has no 'Functor' instance because the type variable
-- is used both to record the dependencies as well as who's doing the
-- depending; having a 'Functor' instance makes bugs where we don't distinguish
-- these two far too likely. (By rights 'LDep' ought to have two type variables.)
data LDep qpn = LDep (DependencyReason qpn) (Dep qpn)
  deriving (Eq, Show)

-- | A dependency (constraint) associates a package name with a constrained
-- instance. It can also represent other types of dependencies, such as
-- dependencies on language extensions.
data Dep qpn = Dep  IsExe qpn CI       -- ^ dependency on a package (possibly for executable)
             | Ext  Extension          -- ^ dependency on a language extension
             | Lang Language           -- ^ dependency on a language version
             | Pkg  PkgconfigName VR   -- ^ dependency on a pkg-config package
  deriving (Functor, Eq, Show)

-- | The reason that a dependency is active. It identifies the package and any
-- flag and stanza choices that introduced the dependency. It contains
-- everything needed for creating ConflictSets or describing conflicts in solver
-- log messages.
data DependencyReason qpn = DependencyReason qpn [(Flag, FlagValue)] [Stanza]
  deriving (Functor, Eq, Show)

-- | Print a dependency.
showDep :: LDep QPN -> String
showDep (LDep dr (Dep (IsExe is_exe) qpn (Fixed i)       )) =
  let DependencyReason qpn' _ _ = dr
  in (if qpn /= qpn' then showDependencyReason dr ++ " => " else "") ++
     showQPN qpn ++
     (if is_exe then " (exe) " else "") ++ "==" ++ showI i
showDep (LDep dr (Dep (IsExe is_exe) qpn (Constrained vr))) =
  showDependencyReason dr ++ " => " ++ showQPN qpn ++
  (if is_exe then " (exe) " else "") ++ showVR vr
showDep (LDep _ (Ext ext))   = "requires " ++ display ext
showDep (LDep _ (Lang lang)) = "requires " ++ display lang
showDep (LDep _ (Pkg pn vr)) = "requires pkg-config package "
                      ++ display pn ++ display vr
                      ++ ", not found in the pkg-config database"

-- | Print the reason that a dependency was introduced.
showDependencyReason :: DependencyReason QPN -> String
showDependencyReason (DependencyReason qpn flags stanzas) =
    intercalate " " $
        showQPN qpn
      : map (uncurry showFlagValue) flags ++ map (\s -> showSBool s True) stanzas

-- | Options for goal qualification (used in 'qualifyDeps')
--
-- See also 'defaultQualifyOptions'
data QualifyOptions = QO {
    -- | Do we have a version of base relying on another version of base?
    qoBaseShim :: Bool

    -- Should dependencies of the setup script be treated as independent?
  , qoSetupIndependent :: Bool
  }
  deriving Show

-- | Apply built-in rules for package qualifiers
--
-- Although the behaviour of 'qualifyDeps' depends on the 'QualifyOptions',
-- it is important that these 'QualifyOptions' are _static_. Qualification
-- does NOT depend on flag assignment; in other words, it behaves the same no
-- matter which choices the solver makes (modulo the global 'QualifyOptions');
-- we rely on this in 'linkDeps' (see comment there).
--
-- NOTE: It's the _dependencies_ of a package that may or may not be independent
-- from the package itself. Package flag choices must of course be consistent.
qualifyDeps :: QualifyOptions -> QPN -> FlaggedDeps PN -> FlaggedDeps QPN
qualifyDeps QO{..} (Q pp@(PackagePath ns q) pn) = go
  where
    go :: FlaggedDeps PN -> FlaggedDeps QPN
    go = map go1

    go1 :: FlaggedDep PN -> FlaggedDep QPN
    go1 (Flagged fn nfo t f) = Flagged (fmap (Q pp) fn) nfo (go t) (go f)
    go1 (Stanza  sn     t)   = Stanza  (fmap (Q pp) sn)     (go t)
    go1 (Simple dep comp)    = Simple (goLDep dep comp) comp

    -- Suppose package B has a setup dependency on package A.
    -- This will be recorded as something like
    --
    -- > LDep (DependencyReason "B") (Dep False "A" (Constrained AnyVersion))
    --
    -- Observe that when we qualify this dependency, we need to turn that
    -- @"A"@ into @"B-setup.A"@, but we should not apply that same qualifier
    -- to the DependencyReason.
    goLDep :: LDep PN -> Component -> LDep QPN
    goLDep (LDep dr dep) comp = LDep (fmap (Q pp) dr) (goD dep comp)

    goD :: Dep PN -> Component -> Dep QPN
    goD (Ext  ext)    _    = Ext  ext
    goD (Lang lang)   _    = Lang lang
    goD (Pkg pkn vr)  _    = Pkg pkn vr
    goD (Dep is_exe dep ci) comp
      | isExeToBool is_exe = Dep is_exe (Q (PackagePath ns (QualExe pn dep)) dep) ci
      | qBase  dep         = Dep is_exe (Q (PackagePath ns (QualBase  pn)) dep) ci
      | qSetup comp        = Dep is_exe (Q (PackagePath ns (QualSetup pn)) dep) ci
      | otherwise          = Dep is_exe (Q (PackagePath ns inheritedQ) dep) ci

    isExeToBool (IsExe b) = b

    -- If P has a setup dependency on Q, and Q has a regular dependency on R, then
    -- we say that the 'Setup' qualifier is inherited: P has an (indirect) setup
    -- dependency on R. We do not do this for the base qualifier however.
    --
    -- The inherited qualifier is only used for regular dependencies; for setup
    -- and base deppendencies we override the existing qualifier. See #3160 for
    -- a detailed discussion.
    inheritedQ :: Qualifier
    inheritedQ = case q of
                   QualSetup _  -> q
                   QualExe _ _  -> q
                   QualToplevel -> q
                   QualBase _   -> QualToplevel

    -- Should we qualify this goal with the 'Base' package path?
    qBase :: PN -> Bool
    qBase dep = qoBaseShim && unPackageName dep == "base"

    -- Should we qualify this goal with the 'Setup' package path?
    qSetup :: Component -> Bool
    qSetup comp = qoSetupIndependent && comp == ComponentSetup

-- | Remove qualifiers from set of dependencies
--
-- This is used during link validation: when we link package @Q.A@ to @Q'.A@,
-- then all dependencies @Q.B@ need to be linked to @Q'.B@. In order to compute
-- what to link these dependencies to, we need to requalify @Q.B@ to become
-- @Q'.B@; we do this by first removing all qualifiers and then calling
-- 'qualifyDeps' again.
unqualifyDeps :: FlaggedDeps QPN -> FlaggedDeps PN
unqualifyDeps = go
  where
    go :: FlaggedDeps QPN -> FlaggedDeps PN
    go = map go1

    go1 :: FlaggedDep QPN -> FlaggedDep PN
    go1 (Flagged fn nfo t f) = Flagged (fmap unq fn) nfo (go t) (go f)
    go1 (Stanza  sn     t)   = Stanza  (fmap unq sn)     (go t)
    go1 (Simple dep comp)    = Simple (goLDep dep) comp

    goLDep :: LDep QPN -> LDep PN
    goLDep (LDep dr dep) = LDep (fmap unq dr) (fmap unq dep)

    unq :: QPN -> PN
    unq (Q _ pn) = pn

{-------------------------------------------------------------------------------
  Reverse dependency map
-------------------------------------------------------------------------------}

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [(Component, QPN)]

{-------------------------------------------------------------------------------
  Goals
-------------------------------------------------------------------------------}

-- | A goal is just a solver variable paired with a reason.
-- The reason is only used for tracing.
data Goal qpn = Goal (Var qpn) (GoalReason qpn)
  deriving (Eq, Show, Functor)

-- | Reason why a goal is being added to a goal set.
data GoalReason qpn =
    UserGoal                              -- introduced by a build target
  | DependencyGoal (DependencyReason qpn) -- introduced by a package
  deriving (Eq, Show, Functor)

type QGoalReason = GoalReason QPN

goalToVar :: Goal a -> Var a
goalToVar (Goal v _) = v

-- | Compute a singleton conflict set from a 'Var'
varToConflictSet :: Var QPN -> ConflictSet
varToConflictSet = CS.singleton

goalReasonToCS :: GoalReason QPN -> ConflictSet
goalReasonToCS UserGoal            = CS.empty
goalReasonToCS (DependencyGoal dr) = dependencyReasonToCS dr

-- | This function returns the solver variables responsible for the dependency.
-- It drops the flag and stanza values, which are only needed for log messages.
dependencyReasonToCS :: DependencyReason QPN -> ConflictSet
dependencyReasonToCS (DependencyReason qpn flags stanzas) =
    CS.fromList $ P qpn : flagVars ++ map stanzaToVar stanzas
  where
    -- Filter out any flags that introduced the dependency with both values.
    -- They don't need to be included in the conflict set, because changing the
    -- flag value can't remove the dependency.
    flagVars :: [Var QPN]
    flagVars = [F (FN qpn fn) | (fn, fv) <- flags, fv /= FlagBoth]

    stanzaToVar :: Stanza -> Var QPN
    stanzaToVar = S . SN qpn
