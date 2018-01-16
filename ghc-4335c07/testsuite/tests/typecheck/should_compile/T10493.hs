{-# LANGUAGE FlexibleContexts #-}

module T10493 where

import Data.Coerce
import Data.Ord (Down)  -- no constructor

foo :: Coercible (Down Int) Int => Down Int -> Int
foo = coerce
