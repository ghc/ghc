{-# Language TypeInType, TypeFamilies, TypeOperators #-}

module T14520 where

import Data.Kind

type a ~>> b = (a, b) -> Type

data family Sing (a::k)

type family XXX (f::a~>>b) (x::a) :: b

type family Id   :: (kat :: a ~>> (a ~>> *)) `XXX` (b :: a) `XXX` b

sId :: Sing w -> Sing (Id :: bat w w)
sId = sId
