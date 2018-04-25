{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Main where

import GHC.Generics hiding (C, C1, D)
import GEq1A
import Enum
import GFunctor

data A = A1
  deriving (Show, Generic, GEq, GEnum)

data B a = B1 | B2 a (B a)
  deriving (Show, Generic, Generic1, GEq, GEnum, GFunctor)

data C phantom a = C1 | C2 a (C phantom a)
  deriving (Show, Generic, Generic1, GEq, GEnum, GFunctor)

data D f a = D1 (f a) (f (D f a)) deriving (Generic, Generic1)
deriving instance (Show (f a), Show (f (D f a))) => Show (D f a)
deriving instance (GEq  (f a), GEq  (f (D f a))) => GEq  (D f a)

data E f a = E1 (f a)
  deriving (Show, Eq, Generic, Generic1, GFunctor)


main = print (
               geq A1 A1
             , take 10 (genum :: [A])

             , geq (B2 A1 B1) B1
             , gmap (++ "lo") (B2 "hel" B1)
             , take 3 (genum :: [B A])

             , geq (C2 A1 C1) C1
             , gmap (++ "lo") (C2 "hel" C1)

             , geq (D1 "a" []) (D1 "a" [])

             , gmap (++ "lo") (E1 ["hel"])
             )
