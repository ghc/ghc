{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Type.Equality

data T a where
  TInt :: T Int
  TBool :: T Bool

f :: T a -> a :~: Int -> ()
f TInt Refl = ()
