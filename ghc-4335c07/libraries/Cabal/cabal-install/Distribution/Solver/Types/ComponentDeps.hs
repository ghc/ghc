{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Fine-grained package dependencies
--
-- Like many others, this module is meant to be "double-imported":
--
-- > import Distribution.Solver.Types.ComponentDeps (
-- >     Component
-- >   , ComponentDep
-- >   , ComponentDeps
-- >   )
-- > import qualified Distribution.Solver.Types.ComponentDeps as CD
module Distribution.Solver.Types.ComponentDeps (
    -- * Fine-grained package dependencies
    Component(..)
  , componentNameToComponent
  , ComponentDep
  , ComponentDeps -- opaque
    -- ** Constructing ComponentDeps
  , empty
  , fromList
  , singleton
  , insert
  , zip
  , filterDeps
  , fromLibraryDeps
  , fromSetupDeps
  , fromInstalled
    -- ** Deconstructing ComponentDeps
  , toList
  , flatDeps
  , nonSetupDeps
  , libraryDeps
  , setupDeps
  , select
  ) where

import Prelude ()
import Distribution.Types.UnqualComponentName
import Distribution.Solver.Compat.Prelude hiding (empty,zip)

import qualified Data.Map as Map
import Data.Foldable (fold)

import qualified Distribution.Types.ComponentName as CN

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Component of a package.
data Component =
    ComponentLib
  | ComponentSubLib UnqualComponentName
  | ComponentFLib   UnqualComponentName
  | ComponentExe    UnqualComponentName
  | ComponentTest   UnqualComponentName
  | ComponentBench  UnqualComponentName
  | ComponentSetup
  deriving (Show, Eq, Ord, Generic)

instance Binary Component

-- | Dependency for a single component.
type ComponentDep a = (Component, a)

-- | Fine-grained dependencies for a package.
--
-- Typically used as @ComponentDeps [Dependency]@, to represent the list of
-- dependencies for each named component within a package.
--
newtype ComponentDeps a = ComponentDeps { unComponentDeps :: Map Component a }
  deriving (Show, Functor, Eq, Ord, Generic)

instance Semigroup a => Monoid (ComponentDeps a) where
  mempty = ComponentDeps Map.empty
  mappend = (<>)

instance Semigroup a => Semigroup (ComponentDeps a) where
  ComponentDeps d <> ComponentDeps d' =
      ComponentDeps (Map.unionWith (<>) d d')

instance Foldable ComponentDeps where
  foldMap f = foldMap f . unComponentDeps

instance Traversable ComponentDeps where
  traverse f = fmap ComponentDeps . traverse f . unComponentDeps

instance Binary a => Binary (ComponentDeps a)

componentNameToComponent :: CN.ComponentName -> Component
componentNameToComponent (CN.CLibName)      = ComponentLib
componentNameToComponent (CN.CSubLibName s) = ComponentSubLib s
componentNameToComponent (CN.CFLibName s)   = ComponentFLib s
componentNameToComponent (CN.CExeName s)    = ComponentExe s
componentNameToComponent (CN.CTestName s)   = ComponentTest s
componentNameToComponent (CN.CBenchName s)  = ComponentBench s

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: ComponentDeps a
empty = ComponentDeps $ Map.empty

fromList :: Monoid a => [ComponentDep a] -> ComponentDeps a
fromList = ComponentDeps . Map.fromListWith mappend

singleton :: Component -> a -> ComponentDeps a
singleton comp = ComponentDeps . Map.singleton comp

insert :: Monoid a => Component -> a -> ComponentDeps a -> ComponentDeps a
insert comp a = ComponentDeps . Map.alter aux comp . unComponentDeps
  where
    aux Nothing   = Just a
    aux (Just a') = Just $ a `mappend` a'

-- | Zip two 'ComponentDeps' together by 'Component', using 'mempty'
-- as the neutral element when a 'Component' is present only in one.
zip :: (Monoid a, Monoid b) => ComponentDeps a -> ComponentDeps b -> ComponentDeps (a, b)
{- TODO/FIXME: Once we can expect containers>=0.5, switch to the more efficient version below:

zip (ComponentDeps d1) (ComponentDeps d2) =
    ComponentDeps $
      Map.mergeWithKey
        (\_ a b -> Just (a,b))
        (fmap (\a -> (a, mempty)))
        (fmap (\b -> (mempty, b)))
        d1 d2

-}
zip (ComponentDeps d1) (ComponentDeps d2) =
    ComponentDeps $
      Map.unionWith
        mappend
        (Map.map (\a -> (a, mempty)) d1)
        (Map.map (\b -> (mempty, b)) d2)


-- | Keep only selected components (and their associated deps info).
filterDeps :: (Component -> a -> Bool) -> ComponentDeps a -> ComponentDeps a
filterDeps p = ComponentDeps . Map.filterWithKey p . unComponentDeps

-- | ComponentDeps containing library dependencies only
fromLibraryDeps :: a -> ComponentDeps a
fromLibraryDeps = singleton ComponentLib

-- | ComponentDeps containing setup dependencies only.
fromSetupDeps :: a -> ComponentDeps a
fromSetupDeps = singleton ComponentSetup

-- | ComponentDeps for installed packages.
--
-- We assume that installed packages only record their library dependencies.
fromInstalled :: a -> ComponentDeps a
fromInstalled = fromLibraryDeps

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

toList :: ComponentDeps a -> [ComponentDep a]
toList = Map.toList . unComponentDeps

-- | All dependencies of a package.
--
-- This is just a synonym for 'fold', but perhaps a use of 'flatDeps' is more
-- obvious than a use of 'fold', and moreover this avoids introducing lots of
-- @#ifdef@s for 7.10 just for the use of 'fold'.
flatDeps :: Monoid a => ComponentDeps a -> a
flatDeps = fold

-- | All dependencies except the setup dependencies.
--
-- Prior to the introduction of setup dependencies in version 1.24 this
-- would have been _all_ dependencies.
nonSetupDeps :: Monoid a => ComponentDeps a -> a
nonSetupDeps = select (/= ComponentSetup)

-- | Library dependencies proper only.  (Includes dependencies
-- of internal libraries.)
libraryDeps :: Monoid a => ComponentDeps a -> a
libraryDeps = select (\c -> case c of ComponentSubLib _ -> True
                                      ComponentLib -> True
                                      _ -> False)

-- | Setup dependencies.
setupDeps :: Monoid a => ComponentDeps a -> a
setupDeps = select (== ComponentSetup)

-- | Select dependencies satisfying a given predicate.
select :: Monoid a => (Component -> Bool) -> ComponentDeps a -> a
select p = foldMap snd . filter (p . fst) . toList
