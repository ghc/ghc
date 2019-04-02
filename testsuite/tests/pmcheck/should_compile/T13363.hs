{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Bug where

data Boolean = F | T
  deriving Eq

pattern TooGoodToBeTrue :: Boolean
pattern TooGoodToBeTrue <- ((== T) -> True)
  where
    TooGoodToBeTrue = T
{-# COMPLETE F, TooGoodToBeTrue #-}

catchAll :: Boolean -> Int
catchAll F               = 0
catchAll TooGoodToBeTrue = 1
catchAll _               = error "impossible"

