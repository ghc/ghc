{-# LANGUAGE TypeInType, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The "tricky case" in #11391
module CustomTypeErrors05 where

import Data.Kind
import GHC.TypeLits (TypeError, ErrorMessage(..))

type family Resolve (t :: Type -> Type) :: Type -> Type where
  Resolve _ = TypeError (Text "ERROR")

testNOTOK2 :: Resolve [] Int
testNOTOK2 = 1
