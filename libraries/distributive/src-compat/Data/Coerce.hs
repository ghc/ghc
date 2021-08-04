-- This is a shim for GHC before 7.8. Cabal ignores it
-- for GHC 7.8 and later.
module Data.Coerce (coerce) where

import Unsafe.Coerce (unsafeCoerce)

coerce :: a -> b
coerce = unsafeCoerce
