{-# OPTIONS_GHC -Wincomplete-patterns #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module T13964 (Boolean(F, TooGoodToBeTrue), catchAll) where

data Boolean = F | T
  deriving Eq

pattern TooGoodToBeTrue :: Boolean
pattern TooGoodToBeTrue <- ((== T) -> True)
  where
    TooGoodToBeTrue = T
{-# COMPLETE F, TooGoodToBeTrue #-}

catchAll :: Boolean -> Int
catchAll F               = 0
-- catchAll TooGoodToBeTrue = 1

