{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Bug1103 where

import Data.Kind

data family   Foo1 :: Type -> Type
data instance Foo1 Bool = Foo1Bool
data instance Foo1 (Maybe a)

data family   Foo2 :: k -> Type
data instance Foo2 Bool = Foo2Bool
data instance Foo2 (Maybe a)
data instance Foo2 :: Char -> Type
data instance Foo2 :: (Char -> Char) -> Type where

data family   Foo3 :: k
data instance Foo3
data instance Foo3 Bool = Foo3Bool
data instance Foo3 (Maybe a)
data instance Foo3 :: Char -> Type
data instance Foo3 :: (Char -> Char) -> Type where
