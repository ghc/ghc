{-# LANGUAGE Modifiers, DataKinds #-}

module ModifiersSuggestLinear where

import GHC.TypeLits (Nat)
import GHC.Types (Multiplicity(..))

%1 %01 %(1 :: Nat) %One %()
data D = D { d %1 %01 %(1 :: Nat) %One %() :: Int }

l :: Int %1 %01 %(1 :: Nat) %One %() -> Int
l = undefined
