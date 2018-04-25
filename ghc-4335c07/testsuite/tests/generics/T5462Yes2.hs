{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DefaultSignatures             #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}

module Main where

import GHC.Generics hiding (C, C1, D)
import GFunctor

class C1 a where
  c1 :: a -> Int
  c1 _ = 1

class C2 a where
  c21 :: a -> Int
  c21 = c22
  c22 :: a -> Int
  c22 = c21
  {-# MINIMAL c21 | c22 #-}

newtype D = D Int deriving C1

instance C1 Int where c1 _ = 2

newtype F a = F1 [a]
  deriving (Show, Eq, Generic, Generic1, GFunctor)

data G = G1 deriving (C1)
data H = H1 deriving (C2)


main = print (c1 (D 3))
