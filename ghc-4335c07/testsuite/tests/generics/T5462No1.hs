{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}

-- DeriveAnyClass not enabled

module T5462No1 where

import GHC.Generics hiding (C, C1, D)
import GFunctor

class C1 a where
  c1 :: a -> Int

class C2 a where
  c2 :: a -> Int
  c2 _ = 0

newtype F a = F1 [a]
  deriving (Show, Eq, Generic, Generic1, GFunctor)

data G = G1 deriving (C1)
data H = H1 deriving (C2)
