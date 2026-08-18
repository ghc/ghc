{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables #-}
module Mid where

import {-# SOURCE #-} Callee

class UC a => MC a where
  mcm :: a -> Int

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE keep #-}
keep :: [Dict c] -> Int
keep xs = length xs + 41

{-# NOINLINE f #-}
f :: forall a. MC a => a -> Int
f _ = keep [Dict :: Dict (UC a)]
