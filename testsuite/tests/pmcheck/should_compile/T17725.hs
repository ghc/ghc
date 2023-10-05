{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
module Lib where

newtype IInt = IInt Int

f :: IInt -> Bool -> ()
f !(IInt _) True = ()
f (IInt 42) True = ()
f _         _    = ()
