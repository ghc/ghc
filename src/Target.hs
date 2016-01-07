{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Target (
      Target (..)
    , PartialTarget (..)
    , unsafeFromPartial
    , fullTarget
    , fullTargetWithWay
) where

import Control.Monad.Trans.Reader

import Base
import Builder
import GHC.Generics (Generic)
import Package
import Stage
import Way

-- | Parameters relevant to the current build target.
data Target = Target
     {
        stage   :: Stage,      -- ^ Stage being built
        package :: Package,    -- ^ Package being built
        builder :: Builder,    -- ^ Builder to be invoked
        way     :: Way,        -- ^ Way to build (set to vanilla for most targets)
        inputs  :: [FilePath], -- ^ Source files passed to the builder
        outputs :: [FilePath]  -- ^ Files to be produced
     }
     deriving (Show, Eq, Generic)

-- | If values of type @a@ form a 'Monoid' then we can also derive a 'Monoid'
-- instance for values of type @'ReaderT' 'Target' 'Action' a@:
--
-- * the empty computation is the identity element of the underlying type
-- * two computations can be combined by combining their results
instance Monoid a => Monoid (ReaderT Target Action a) where
    mempty  = return mempty
    mappend = liftM2 mappend

-- | A partially constructed Target with fields 'Stage' and 'Package' only.
-- 'PartialTarget's are used for generating build rules.
data PartialTarget = PartialTarget Stage Package deriving (Eq, Show)

-- | Convert 'PartialTarget' to a 'Target' assuming that unknown fields won't
-- be used.
unsafeFromPartial :: PartialTarget -> Target
unsafeFromPartial (PartialTarget s p) = Target
    {
        stage   = s,
        package = p,
        builder = error "unsafeFromPartial: builder not set",
        way     = error "unsafeFromPartial: way not set",
        inputs  = error "unsafeFromPartial: inputs not set",
        outputs = error "unsafeFromPartial: outputs not set"
    }

-- | Construct a full 'Target' by augmenting a 'PartialTarget' with missing
-- fields. Most targets are built only one way, 'vanilla', hence it is set by
-- default. Use 'fullTargetWithWay' otherwise.
fullTarget ::
       PartialTarget
    -> Builder
    -> [FilePath] -- ^ Source files
    -> [FilePath] -- ^ Produced files
    -> Target
fullTarget (PartialTarget s p) b srcs fs = Target
    {
        stage   = s,
        package = p,
        builder = b,
        way     = vanilla,
        inputs  = map unifyPath srcs,
        outputs = map unifyPath fs
    }

-- | Like 'fullTarget', but allows an explicit 'Way' parameter.
fullTargetWithWay ::
       PartialTarget
    -> Builder
    -> Way
    -> [FilePath] -- ^ Source files
    -> [FilePath] -- ^ Produced files
    -> Target
fullTargetWithWay pt b w srcs fs = (fullTarget pt b srcs fs) { way = w }

instance Binary Target
instance NFData Target
instance Hashable Target
