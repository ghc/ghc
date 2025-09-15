{-# LANGUAGE FlexibleContexts #-}

module RepArrow where

import Data.Ord ( Down )  -- convenient "Id" newtype, without its constructor
import Data.Coerce

foo :: Coercible (Down (Int -> Int)) (Int -> Int) => Down (Int -> Int) -> Int -> Int
foo = coerce
