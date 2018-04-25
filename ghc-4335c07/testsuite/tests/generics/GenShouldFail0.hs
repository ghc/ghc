{-# LANGUAGE StandaloneDeriving #-}

module ShouldFail0 where

import GHC.Generics (Generic)

data X = X

deriving instance Generic X

-- Should fail (no XDeriveGeneric)
