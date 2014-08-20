{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module UnsafeInfered13 where

class C a where
  f :: a -> String

instance {-# OVERLAPS #-} C a where
  f _ = "a"

