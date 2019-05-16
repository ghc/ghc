{-# LANGUAGE PatternSynonyms #-}

module Lib where

data Boolean = F | T
  deriving Eq

pattern TooGoodToBeTrue :: Boolean
pattern TooGoodToBeTrue = T
{-# COMPLETE F, TooGoodToBeTrue #-}

catchAll :: Boolean -> Int
catchAll F               = 0
catchAll TooGoodToBeTrue = 1
catchAll _               = error "impossible"
