
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module T20466 where

import Data.Kind (Type)

type family F (a :: Type) :: Type

type family G (a :: Type) :: Type

type Syn a b c = Either (G a) (G b)

class Cls (a :: Type)

instance {-# OVERLAPPABLE #-} Cls a

instance Cls (Either Int Bool)

foo :: forall a b c. Cls (Either a (F (Syn a b c))) => a -> b -> c -> Int
foo _ _ _ = 42

bar :: forall a b c. a -> b -> c -> Int
bar = foo

foo' :: Cls (Either Int (F Bool)) => Int
foo' = 42

bar' :: Int
bar' = foo'
