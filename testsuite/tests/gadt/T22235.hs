{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module T22235 (f) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))

f :: ST x -> ST y -> x :~: y
f st1@SMkT st2@SMkT = method st1 st2

type T :: Type -> Type
data T a where
  MkT :: T Int

type ST :: T a -> Type
data ST (t :: T a) where
  SMkT :: ST MkT

class C f where
  method :: f a -> f b -> a :~: b

instance C ST where
  method SMkT SMkT = Refl
