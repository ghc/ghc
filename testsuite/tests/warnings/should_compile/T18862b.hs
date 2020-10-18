{-# OPTIONS -Wcompat -Wno-error=type-equality-out-of-scope #-}

module T18862b where

import Prelude (id)

f :: (a ~ b) => a -> b
f = id
