{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds                  #-}

module T9968 where

import GHC.Generics ( Generic(..), Generic1(..), Rep, M1(..) )


data D1 = D11
  deriving (C1, C8)

newtype D2 = D21 Int
  deriving (C1, C8)

newtype D3 a = D31 a
  deriving (Show, Foldable, C1, C2, C3 a, C5 Int, C8)

data D4 a = D41
  deriving (Foldable, C2)

data D5 a b = D51 a | D52 b
  deriving (C9)

data D6 f a = D61 (f a)
  deriving (C1, C8)

data D7 h f = D71 (h f) (f Int)
  deriving (C1, C3 Int, C4)

instance Show (D7 h f) where show = undefined

data Proxy (t :: k) = Proxy
  deriving (Foldable, C1, C2, C8)


class C1 a where
  c11 :: a -> Int
  c11 = undefined

class Foldable f => C2 f where
  c21 :: (Show a) => f a -> String
  c21 = foldMap show

class C3 a b where
  c31 :: Read c => a -> b -> c
  default c31 :: (Show a, Show b, Read c) => a -> b -> c
  c31 a b = read (show a ++ show b)

class C4 h where
  c41 :: (f a -> f a) -> h f -> Int
  c41 = undefined

class C5 a f where
  c51 :: f a -> Int
  c51 = undefined

class C6 a where
  c61 :: a -> Int
  default c61 :: (Generic a, C7 (Rep a)) => a -> Int
  c61 = c71 . from

-- trivial generic function that always returns 0
class C7 f where c71 :: f p -> Int
instance C7 (M1 i c f) where c71 _ = 0

class C8 (a :: k) where
  c81 :: Proxy a -> Int
  c81 _ = 0

class C9 (h :: * -> * -> *) where
  c91 :: h a b -> Int
  c91 _ = 0
