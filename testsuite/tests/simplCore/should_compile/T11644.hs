{-# LANGUAGE TypeFamilies, ScopedTypeVariables#-}

module T11644 where

import Data.Kind (Type)

class Foo m where
    type Bar m :: Type
    action :: m -> Bar m -> m

right x m = action m (Right x)

right' :: (Either a b ~ Bar m, Foo m) => b -> m -> m
right' x m = action m (Right x)

instance Foo Int where
    type Bar Int = Either Int Int
    action m a = either (*) (+) a m

instance Foo Float where
    type Bar Float = Either Float Float
    action m a = either (*) (+) a m

foo = print $ right (1::Int) (3 :: Int)
bar = print $ right (1::Float) (3 :: Float)
