module Distribution.Types.AnnotatedId (
    AnnotatedId(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Types.ComponentName

-- | An 'AnnotatedId' is a 'ComponentId', 'UnitId', etc.
-- which is annotated with some other useful information
-- that is useful for printing to users, etc.
data AnnotatedId id = AnnotatedId {
        ann_pid   :: PackageId,
        ann_cname :: ComponentName,
        ann_id    :: id
    }
    deriving (Show)

instance Package (AnnotatedId id) where
    packageId = ann_pid

instance Functor AnnotatedId where
    fmap f (AnnotatedId pid cn x) = AnnotatedId pid cn (f x)
