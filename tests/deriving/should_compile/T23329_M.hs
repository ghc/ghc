{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T23329_M where

import Data.Kind (Type)
import Data.Proxy (Proxy)

class MyClass (f :: k -> Type) where
  type MyTypeFamily f (i :: k) :: Type
  myMethod :: MyTypeFamily f i -> Proxy f -> Proxy i -> ()

instance MyClass Maybe where
  type MyTypeFamily Maybe i = ()
  myMethod = undefined

newtype MyMaybe a = MyMaybe (Maybe a)
  deriving MyClass
