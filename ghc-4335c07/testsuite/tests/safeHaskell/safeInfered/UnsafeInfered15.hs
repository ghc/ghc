{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module UnsafeInfered15 where

class C a where
  f :: a -> String

instance {-# OVERLAPPING #-} C a where
  f _ = "a"

