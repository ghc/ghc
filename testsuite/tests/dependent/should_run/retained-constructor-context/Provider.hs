{-# LANGUAGE GHC2024, ExperimentalDependentTypes #-}
module Provider where

type Payload (b :: Bool) = case b of { True -> Int; False -> Char }

data Stored a b where
  Stored :: forall a b. (a ~ Bool, b ~ Char) =>
            foreach (flag :: Bool) -> Payload flag -> Stored a b
