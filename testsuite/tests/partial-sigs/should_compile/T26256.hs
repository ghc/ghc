{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module M (go) where

import Data.Kind

type Apply :: (Type -> Type) -> Type
data Apply m

type (:->) :: Type -> Type -> Type
type family (:->) where (:->) = (->)

f :: forall (k :: Type -> Type -> Type) (m :: Type -> Type).
     k Int (m Char) -> k Bool (Apply m)
f = f

x :: Int :-> Maybe Char
x = x

go :: Bool -> _ _
go = f x
