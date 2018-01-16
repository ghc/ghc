{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics hiding (C, D)
import GFunctor

-- We should be able to generate a generic representation for these types
data D a = D0 | D1 { d11 :: a, d12 :: (D a) }
  deriving (Show, Generic, Generic1)

-- Example values
d0 :: D Char
d0 = D0
d1 = D1 (Just 'p') D0

d2 :: (Fractional a) => D (a,a)
d2 = D1 (3,0.14) D0

-- Generic instances
instance GFunctor D

-- Tests
main = print ( gmap (+1) (Just 2)
             , gmap (+1) [0,1,2]
             , gmap (+1) (0,0)
             , gmap undefined d0 :: D ()
             , gmap (const 'q') d1
             , gmap (\(a,b) -> a + b) d2 :: D Float)
