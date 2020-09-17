{-# Language DataKinds           #-}
{-# Language RankNTypes          #-}
{-# Language TypeApplications    #-}
{-# Language PolyKinds           #-}
{-# Language KindSignatures      #-}
{-# Language TypeFamilies        #-}
{-# Language AllowAmbiguousTypes #-}
module T15942 where

import Data.Kind
import Data.Proxy

type G1 = forall (b :: Bool). Type

data Fun1 :: G1

class F1 (bool :: Bool) where
  type Not1 bool :: Bool
  foo1 :: Fun1 @(Not1 bool)

type G2 = Bool -> Type

data Fun2 :: G2

class F2 (bool :: Bool) where
  type Not2 bool :: Bool
  foo2 :: Proxy (x :: Proxy (Not2 bool))

