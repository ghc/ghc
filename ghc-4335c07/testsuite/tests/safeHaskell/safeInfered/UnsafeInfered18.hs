{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module UnsafeInfered18 where

class C a where
  f :: a -> String

instance C a where
  f _ = "a"

