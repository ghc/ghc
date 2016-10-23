{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Target (Target (..), dummyTarget) where

import Control.Monad.Trans.Reader
import GHC.Generics

import Base
import Builder
import Context

-- | Parameters relevant to the current build target.
data Target = Target
    { context :: Context    -- ^ Current build context
    , builder :: Builder    -- ^ Builder to be invoked
    , inputs  :: [FilePath] -- ^ Source files passed to the builder
    , outputs :: [FilePath] -- ^ Files to be produced
    } deriving (Show, Eq, Generic)

-- | If values of type @a@ form a 'Monoid' then we can also derive a 'Monoid'
-- instance for values of type @'ReaderT' 'Target' 'Action' a@:
-- * the empty computation is the identity element of the underlying type
-- * two computations can be combined by combining their results
instance Monoid a => Monoid (ReaderT Target Action a) where
    mempty  = return mempty
    mappend = liftM2 mappend

dummyTarget :: Context -> Target
dummyTarget ctx = Target
    { context = ctx
    , builder = error "dummyTarget: builder not set"
    , inputs  = error "dummyTarget: inputs not set"
    , outputs = error "dummyTarget: outputs not set" }

instance Binary Target
instance Hashable Target
instance NFData Target
