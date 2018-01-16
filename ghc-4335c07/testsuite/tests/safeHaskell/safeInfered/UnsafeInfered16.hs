{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module UnsafeInfered16 where

class C a where
  f :: a -> String

instance {-# OVERLAPPING #-} C a where
  f _ = "a"

instance {-# OVERLAPS #-} C Int where
  f _ = "Int"

instance {-# OVERLAPPABLE #-} C Bool where
  f _ = "Bool"

