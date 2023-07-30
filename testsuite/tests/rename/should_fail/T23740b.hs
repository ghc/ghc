{-# LANGUAGE RequiredTypeArguments #-}

module T23740b where

import Prelude (id, Int)

f :: id -> Int
f _ = 10