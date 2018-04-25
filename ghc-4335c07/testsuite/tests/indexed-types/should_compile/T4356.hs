{-# LANGUAGE TypeFamilies #-}
module T4356 where

type family T t :: * -> * -> *
type instance T Bool = (->)

f :: T Bool Bool Bool
f = not
