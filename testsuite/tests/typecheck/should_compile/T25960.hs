{-# LANGUAGE LambdaCase #-}

module T25960 where

import Data.Void (Void)

f :: (forall a. Void -> a) -> (forall a. Void -> a)
f g = g

absurd :: Void -> a
absurd = f (\case)

