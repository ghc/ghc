{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module UnsatPMWarnings where

import Data.Kind
import Data.Void
import GHC.TypeError

data MyGADT a where
  MyInt :: MyGADT Int

type IsBool :: Type -> Constraint
type family IsBool a where
  IsBool Bool = ()
  IsBool a    = Unsatisfiable (Text "Must be Bool")

foo :: IsBool a => MyGADT a -> Void
foo x = case x of {}
