{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}

module T14479 where

foo :: Num a => a -> a
foo xxx = g xxx
  where
    g :: forall b. Num b => _ -> b
    g y = xxx + y
