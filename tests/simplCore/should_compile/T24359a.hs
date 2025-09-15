{-# LANGUAGE TypeApplications, ExplicitForAll #-}

module T24359a where

data UA i = UA !i

class IArray a where
  bounds :: a i -> i

showsIArray :: (IArray a, Show i) => a i -> String
showsIArray a = show (bounds a)

{-# SPECIALISE
    showsIArray :: (Show i) => UA i -> String
  #-}

instance IArray UA where
  bounds (UA u) = u
