{-# OPTIONS_GHC -Wunused-imports -Wunused-top-binds #-}
module T10347 (N, mkN, mkSum) where

import Data.Coerce
import Data.Monoid (Sum(Sum))

newtype N a = MkN Int

mkN :: Int -> N a
mkN = coerce -- Should mark MkN (a locally defined constructor) as used

mkSum :: Int -> Sum Int
mkSum = coerce -- Should mark Sum (an imported constructor) as used
