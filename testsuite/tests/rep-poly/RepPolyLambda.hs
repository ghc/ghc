{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyLambda where

import Data.Kind
import GHC.Exts

f :: forall r (a :: TYPE r). a -> a
f = \ x -> x
