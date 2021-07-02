{-# OPTIONS_GHC -O1 #-}
module Repro where

data A = A
  { a1 :: (Maybe ())
  , a2 :: (Maybe ())
  , a3 :: (Maybe ())
  , a4 :: (Maybe ())
  , a5 :: (Maybe ())
  , a6 :: (Maybe ())
  , a7 :: (Maybe ())
  , a8 :: (Maybe ())
  , a9 :: (Maybe ())
  , a10 :: (Maybe ())
  , a11 :: (Maybe ())
  , a12 :: (Maybe ())
  }

data B = B
  { b1 :: !Bool
  , b2 :: !Bool
  , b3 :: !Bool
  , b4 :: !Bool
  , b5 :: !Bool
  , b6 :: !Bool
  , b7 :: !Bool
  , b8 :: !Bool
  , b9 :: !Bool
  , b10 :: !Bool
  , b11 :: !Bool
  , b12 :: !Bool
  }

f :: Maybe () -> Bool
f (Just ()) = True
f Nothing = False

g :: A -> B
g (A a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) =
  B {
      b1 = f a1
    , b2 = f a2
    , b3 = f a3
    , b4 = f a4
    , b5 = f a5
    , b6 = f a6
    , b7 = f a7
    , b8 = f a8
    , b9 = f a9
    , b10 = f a10
    , b11 = f a11
    , b12 = f a12
  }
