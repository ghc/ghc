{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Type.Ord
import GHC.TypeLits

data Prox t = Prox

main :: IO ()
main = do
  print $ cmpSymbol (Prox @"foo") (Prox @"qux")
  print $ cmpSymbol (Prox @"foo") (Prox @"foo")
  print $ cmpSymbol (Prox @"foo") (Prox @"bar")
  print $ cmpNat (Prox @1) (Prox @3)
  print $ cmpNat (Prox @5) (Prox @5)
  print $ cmpNat (Prox @7) (Prox @2)
