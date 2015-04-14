{-# LANGUAGE PatternSignatures #-}
module Test10255 where

import Data.Maybe

fob (f :: (Maybe t -> Int)) =
  undefined
