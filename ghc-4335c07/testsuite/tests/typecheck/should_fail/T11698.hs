{-# LANGUAGE MonoLocalBinds #-}
module T11698 where

f x = (k 'v', k True)
  where
    h = const True x
    k z = const h (k z) -- k type should not be generalized because h is closed.
