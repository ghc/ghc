{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}

module T18960 where

pattern P :: a -> a
pattern P x = x
{-# COMPLETE P :: () #-}

foo :: ()
foo = case () of
    P _ -> ()

