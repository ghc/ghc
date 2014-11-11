{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DerivingViaGenerics    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}

module T5462No2 where

import GHC.Generics hiding (C, C1, D)
import GFunctor

class C1 a where
  c1 :: a -> Int

class C2 a where
  c21 :: a -> Int
  c21 = c22
  c22 :: a -> Int
  c22 = c21
  {-# MINIMAL c21 | c22 #-}

newtype F a = F1 [a]
  deriving (Show, Eq, Generic, Generic1, GFunctor)

data G = G1 deriving (C1)
data H = H1 deriving (C2)
