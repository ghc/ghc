{-# LANGUAGE MagicHash #-}

-- | Asserts that absent bindings of UnliftedRep are properly WWed
module Unlifted where

import GHC.Exts

fac :: Int -> Int
fac n = product [1..n]

data MMutVar s a = MMutVar (MutVar# s a) Int
mutVar :: MMutVar s a -> Int
mutVar (MMutVar _ n) = fac n
{-# NOINLINE mutVar #-}

data AArray a = AArray (Array# a) Int
array :: AArray a -> Int
array (AArray _ n) = fac n
{-# NOINLINE array #-}
