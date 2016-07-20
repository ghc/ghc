{-# LANGUAGE RankNTypes #-}

module T12082 where

import Data.Typeable (Typeable)
import Control.Monad.ST (RealWorld)

f :: forall a. (forall b. Typeable b => b -> a) -> a
f = undefined :: (RealWorld -> a) -> a
