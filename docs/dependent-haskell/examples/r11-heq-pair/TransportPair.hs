{-# OPTIONS_GHC -fplugin=PairCorePlugin #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Transport where

import Data.Type.Equality

type role Index nominal
data Index (a :: k) = Index Int

transport :: forall k l (a :: k) (b :: l). a ~~ b => Index a -> Index b
transport x = x
