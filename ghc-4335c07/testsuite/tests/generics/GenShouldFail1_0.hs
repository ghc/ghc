{-# LANGUAGE StandaloneDeriving #-}

module ShouldFail1_0 where

import GHC.Generics (Generic1)

data X a = X

deriving instance Generic1 X

-- Should fail (no XDeriveGeneric)
