{-# LANGUAGE StandaloneDeriving #-}

module ShouldFail0 where

import GHC.Generics (Representable0)

data X = X

deriving instance Representable0 X

-- Should fail (no XDeriveRepresentable)
