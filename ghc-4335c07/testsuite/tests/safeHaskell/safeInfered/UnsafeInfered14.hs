{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module UnsafeInfered14 where

class C a where
  f :: a -> String

instance {-# OVERLAPPABLE #-} C a where
  f _ = "a"

