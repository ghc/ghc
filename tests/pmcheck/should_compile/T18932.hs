{-# OPTIONS_GHC -fforce-recomp -Wall -Wno-missing-signatures #-}

module T18932 where

import Data.Void

data T a = MkT1 Int | MkT2 !a

f (MkT1 x) = x
f (MkT2 y) = absurd y

f' (MkT1 x) = x

g (MkT1 x) (MkT1 _) (MkT1 _) = x
