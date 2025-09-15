-- This module defines an instance with -fspecialise-incoherents in effect,
-- then it will be imported by a module that uses -fno-specialise-incoherents.

{-# LANGUAGE UndecidableInstances #-}
module T25883d_import where

class C a where
  m :: a -> a

instance {-# INCOHERENT #-} Num a => C a where
  m = (* 3)
