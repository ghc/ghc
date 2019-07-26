{-# LANGUAGE TypeFamilies #-}
module T16995 where

import Data.Kind

type family Param :: Type -> Type

type family LookupParam (a :: Type) :: Type where
  LookupParam (_ Char)  = Bool
  LookupParam _ = Int

foo :: LookupParam (Param Bool)
foo = 42
