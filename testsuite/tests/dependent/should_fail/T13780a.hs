{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
module T13780a where

data family Sing (a :: k)

data Foo a = a ~ Bool => MkFoo
data instance Sing (z :: Foo a) = (z ~ MkFoo) => SMkFoo
