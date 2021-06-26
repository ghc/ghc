{-# LANGUAGE QuantifiedConstraints , UndecidableInstances #-}

module T20033 where

import Data.Typeable

data Some c

extractSome :: (Typeable a, forall x. c x => Typeable x) => Some c -> a
extractSome _ = undefined
