{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module T15431a where

import Data.Coerce
import Data.Functor.Identity

g1 :: Coercible (t a) Int => t a -> Int
g1 = coerce

g2 :: Coercible Int (t a) => t a -> Int
g2 = coerce
