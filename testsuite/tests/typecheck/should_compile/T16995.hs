{-# LANGUAGE TypeFamilies #-}
module T16995 where

import Data.Kind

type family Param1 :: Type -> Type
type family Param2 (a :: Type) :: Type -> Type
type family Param3 (a :: Type) (b :: Type) :: Type -> Type

type family LookupParam (a :: Type) :: Type where
  LookupParam (_ Char)  = Bool
  LookupParam _ = Int

foo :: LookupParam (Param1 Bool)
foo = 42

bar :: LookupParam (Param2 Bool Bool)
bar = 27

baz :: LookupParam (Param3 Bool Bool Bool)
baz = 12
