{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module UnsafeInfered17 where

class C a where
  f :: a -> String

instance {-# INCOHERENT #-} C a where
  f _ = "a"

