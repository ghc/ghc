{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeInType            #-}

module T16320 where

import Data.Proxy
import Data.Kind

class C a b where
  meth :: forall a. a -> b

foo :: forall a. (a ~ Int) => a -> a
foo x = x

bar :: forall a b. (a ~ Int) => a -> b -> a
bar x y = x

-- Taken from #14238.
data Foo (k :: Type) :: k -> Type where
  MkFoo :: Foo (k1 -> k2) f -> Foo k1 a -> Foo k2 (f a)

f4 :: (forall b. b -> b) -> a -> a
f4 b a = a

f6 :: Int -> Show a => String
f6 i = "abc"

f7 :: (forall a. a) -> (forall a. a)
f7 _ = undefined
