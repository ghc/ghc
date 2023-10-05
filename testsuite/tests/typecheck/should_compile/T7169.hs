{-#OPTIONS_GHC -Wpartial-fields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}


module T7196 where

data T a = A
  { m1  :: a
  , m2  :: a
  , _m3 :: a
  } | B
  {
    m1 :: a
  }

pattern P{x} = x

data family F a
data instance F a where
  F1 :: { f1 :: Int } -> F Int
  F2 :: { f2 :: Int } -> F Char
