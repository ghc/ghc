{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module UnsafeInfered19 where

class C a where
  f :: a -> String

instance C a where
  f _ = "a"

