{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Bug where

import Data.Proxy

class C a where
  m :: Proxy a

f :: (forall {a}. C a) => Proxy Int
f = m
