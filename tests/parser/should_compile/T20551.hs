{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module T20551 where

{-# COMPLETE T, Prelude.False #-}
pattern T :: Bool
pattern T <- True where
  T = True

foo :: Bool -> Bool
foo T     = False
foo False = True
