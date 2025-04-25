{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module T26051_Import (specMe, MaybeShowNum) where

import Data.Kind

type family MaybeShowNum a n :: Constraint where
  MaybeShowNum a n = (Show a, Num n)

{-# INLINABLE specMe #-}
specMe :: (Integral n, MaybeShowNum a n) => a -> n -> (String,n)
specMe s !n = (show s, n+1 `div` 2)
