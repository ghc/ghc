{-# LANGUAGE GADTs, ScopedTypeVariables, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
module Callee where

import Data.Kind (Constraint)

class Eq a => TC a where
  tcDummy :: a -> Int

class c => UC (c :: Constraint)

instance TC Int where tcDummy _ = 0
instance c => UC c

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE discard #-}
discard :: Dict c -> Int
discard _ = 42

{-# NOINLINE b #-}
b :: forall a. UC (UC (TC a)) => a -> Int
b _ = discard (Dict :: Dict (Eq a))
