{-# LANGUAGE GHC2024, ExperimentalDependentTypes #-}
module Provider where

import Data.Proxy (Proxy)

type Payload (b :: Bool) = case b of { True -> Int; False -> Char }

data Stored a b where
  Stored :: forall a b. (a ~ Bool, b ~ Char) =>
            foreach (flag :: Bool) -> Payload flag -> Stored a b

data Nat = Z | S Nat

data T a where
  Mk :: forall a. (a ~ Int) => foreach (n :: Nat) -> Proxy n -> T a
