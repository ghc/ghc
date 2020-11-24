{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
module T13780a where

data family Sing (c :: k)

data Foo b = b ~ Bool => MkFoo

data instance Sing (z :: Foo a) = (z ~ MkFoo) => SMkFoo
