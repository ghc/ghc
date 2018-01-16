{-# LANGUAGE TypeInType, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The "bad case" in #11391
module CustomTypeErrors04 where

import Data.Kind
import GHC.TypeLits (TypeError, ErrorMessage(..))

type family Resolve (t :: Type -> Type) :: Type -> Type where
  Resolve _ = TypeError (Text "ERROR")

testNOTOK1 :: Resolve [] Int
testNOTOK1 = ()
