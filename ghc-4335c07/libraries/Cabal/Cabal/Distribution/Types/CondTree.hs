{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Types.CondTree (
    CondTree(..),
    CondBranch(..),
    condIfThen,
    condIfThenElse,
    mapCondTree,
    mapTreeConstrs,
    mapTreeConds,
    mapTreeData,
    traverseCondTreeV,
    traverseCondBranchV,
    extractCondition,
    simplifyCondTree,
    ignoreConditions,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Condition

-- | A 'CondTree' is used to represent the conditional structure of
-- a Cabal file, reflecting a syntax element subject to constraints,
-- and then any number of sub-elements which may be enabled subject
-- to some condition.  Both @a@ and @c@ are usually 'Monoid's.
--
-- To be more concrete, consider the following fragment of a @Cabal@
-- file:
--
-- @
-- build-depends: base >= 4.0
-- if flag(extra)
--     build-depends: base >= 4.2
-- @
--
-- One way to represent this is to have @'CondTree' 'ConfVar'
-- ['Dependency'] 'BuildInfo'@.  Here, 'condTreeData' represents
-- the actual fields which are not behind any conditional, while
-- 'condTreeComponents' recursively records any further fields
-- which are behind a conditional.  'condTreeConstraints' records
-- the constraints (in this case, @base >= 4.0@) which would
-- be applied if you use this syntax; in general, this is
-- derived off of 'targetBuildInfo' (perhaps a good refactoring
-- would be to convert this into an opaque type, with a smart
-- constructor that pre-computes the dependencies.)
--
data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [CondBranch v c a]
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance (Binary v, Binary c, Binary a) => Binary (CondTree v c a)

-- | A 'CondBranch' represents a conditional branch, e.g., @if
-- flag(foo)@ on some syntax @a@.  It also has an optional false
-- branch.
--
data CondBranch v c a = CondBranch
    { condBranchCondition :: Condition v
    , condBranchIfTrue    :: CondTree v c a
    , condBranchIfFalse   :: Maybe (CondTree v c a)
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Traversable)

-- This instance is written by hand because GHC 8.0.1/8.0.2 infinite
-- loops when trying to derive it with optimizations.  See
-- https://ghc.haskell.org/trac/ghc/ticket/13056
instance Foldable (CondBranch v c) where
    foldMap f (CondBranch _ c Nothing) = foldMap f c
    foldMap f (CondBranch _ c (Just a)) = foldMap f c `mappend` foldMap f a

instance (Binary v, Binary c, Binary a) => Binary (CondBranch v c a)

condIfThen :: Condition v -> CondTree v c a -> CondBranch v c a
condIfThen c t = CondBranch c t Nothing

condIfThenElse :: Condition v -> CondTree v c a -> CondTree v c a -> CondBranch v c a
condIfThenElse c t e = CondBranch c t (Just e)

mapCondTree :: (a -> b) -> (c -> d) -> (Condition v -> Condition w)
            -> CondTree v c a -> CondTree w d b
mapCondTree fa fc fcnd (CondNode a c ifs) =
    CondNode (fa a) (fc c) (map g ifs)
  where
    g (CondBranch cnd t me)
        = CondBranch (fcnd cnd)
                     (mapCondTree fa fc fcnd t)
                     (fmap (mapCondTree fa fc fcnd) me)

mapTreeConstrs :: (c -> d) -> CondTree v c a -> CondTree v d a
mapTreeConstrs f = mapCondTree id f id

mapTreeConds :: (Condition v -> Condition w) -> CondTree v c a -> CondTree w c a
mapTreeConds f = mapCondTree id id f

mapTreeData :: (a -> b) -> CondTree v c a -> CondTree v c b
mapTreeData f = mapCondTree f id id

-- | @Traversal (CondTree v c a) (CondTree w c a) v w@
traverseCondTreeV :: Applicative f => (v -> f w) -> CondTree v c a -> f (CondTree w c a)
traverseCondTreeV f (CondNode a c ifs) =
    CondNode a c <$> traverse (traverseCondBranchV f) ifs

-- | @Traversal (CondBranch v c a) (CondBranch w c a) v w@
traverseCondBranchV :: Applicative f => (v -> f w) -> CondBranch v c a -> f (CondBranch w c a)
traverseCondBranchV f (CondBranch cnd t me) = CondBranch
    <$> traverse f cnd
    <*> traverseCondTreeV f t
    <*> traverse (traverseCondTreeV f) me

-- | Extract the condition matched by the given predicate from a cond tree.
--
-- We use this mainly for extracting buildable conditions (see the Note above),
-- but the function is in fact more general.
extractCondition :: Eq v => (a -> Bool) -> CondTree v c a -> Condition v
extractCondition p = go
  where
    go (CondNode x _ cs) | not (p x) = Lit False
                         | otherwise = goList cs

    goList []               = Lit True
    goList (CondBranch c t e : cs) =
      let
        ct = go t
        ce = maybe (Lit True) go e
      in
        ((c `cAnd` ct) `cOr` (CNot c `cAnd` ce)) `cAnd` goList cs

-- | Flattens a CondTree using a partial flag assignment.  When a condition
-- cannot be evaluated, both branches are ignored.
simplifyCondTree :: (Monoid a, Monoid d) =>
                    (v -> Either v Bool)
                 -> CondTree v d a
                 -> (d, a)
simplifyCondTree env (CondNode a d ifs) =
    mconcat $ (d, a) : mapMaybe simplifyIf ifs
  where
    simplifyIf (CondBranch cnd t me) =
        case simplifyCondition cnd env of
          (Lit True, _) -> Just $ simplifyCondTree env t
          (Lit False, _) -> fmap (simplifyCondTree env) me
          _ -> Nothing

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
--  possible paths into account.  Note that since branches represent exclusive
--  choices this may not result in a \"sane\" result.
ignoreConditions :: (Monoid a, Monoid c) => CondTree v c a -> (a, c)
ignoreConditions (CondNode a c ifs) = (a, c) `mappend` mconcat (concatMap f ifs)
  where f (CondBranch _ t me) = ignoreConditions t
                       : maybeToList (fmap ignoreConditions me)
