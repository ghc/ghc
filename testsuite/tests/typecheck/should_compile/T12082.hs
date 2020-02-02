{-# LANGUAGE RankNTypes #-}

module T12082 where

import Data.Typeable (Typeable)
import Control.Monad.ST (RealWorld)

f :: forall a. (forall b. Typeable b => b -> a) -> a
f x = (undefined :: (RealWorld -> a) -> a) x
  -- Simple subsumption (#17775) requires eta expansion here
