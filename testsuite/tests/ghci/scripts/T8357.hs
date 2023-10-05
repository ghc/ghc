{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module T8357 where

import Data.Kind (Type)
import GHC.TypeLits 

data (:::) (sy :: Symbol) ty 
data Key (sy :: Symbol) 
data Rec (rs :: [Type])

(*=) :: Key sy -> ty -> Rec '[sy ::: ty]
(*=) = undefined

(.*.) :: (Union xs ys ~ rs) => Rec xs -> Rec ys -> Rec rs
(.*.) = undefined

type family Union (xs :: [Type]) (ys :: [Type]) :: [Type]  where
    Union ((sy ::: t) ': xs) ys = (sy ::: t) ': Union xs ys
    Union '[] ys = ys


fFoo :: Key "foo"
fFoo = undefined

fBar :: Key "bar"
fBar = undefined

foo = fFoo *= "foo"
bar = fBar *= "bar"
both = foo .*. bar
