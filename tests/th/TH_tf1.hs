{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module TH_tf1 where

import Data.Kind (Type)

$( [d| data family T a |] )
$( [d| data instance T Int = TInt Bool |] )

foo :: Bool -> T Int
foo b = TInt (b && b)

$( [d| type family S a |] )
$( [d| type instance S Int = Bool |] )

bar :: S Int -> Int
bar c = if c then 1 else 2

$( [d| type family R (a :: Type -> Type) :: Type -> Type |] )
$( [d| type instance R Maybe = [] |] )

baz :: R Maybe Int -> Int
baz = head
