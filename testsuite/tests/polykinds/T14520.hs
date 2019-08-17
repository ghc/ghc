{-# Language DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             AllowAmbiguousTypes, TopLevelKindSignatures #-}

module T14520 where

import Data.Kind

type a ~>> b = (a, b) -> Type

data family Sing (a::k)

type family XXX (f::a~>>b) (x::a) :: b

type Id :: (kat :: a ~>> (a ~>> Type)) `XXX` (b :: a) `XXX` b
type family Id

sId :: Sing w -> Sing (Id :: bat w w)
sId = sId
