{-# LANGUAGE GADTs, TypeFamilies #-}

module T18809 where

-- Ordinary
data F2 s where
  MkF2 :: {  foo2 :: Int } -> F2 s

bar2 :: F2 s -> Int -> F2 s
bar2 z y = z { foo2 = y }

-- GADT
data F1 s where
  MkF1 :: {  foo1 :: Int } -> F1 Int

bar1 :: F1 s -> Int -> F1 s
bar1 z y = z { foo1 = y }

-- Ordinary data family
data family F3 a
data instance F3 (s,t) where
  MkF2b :: { foo3 :: Int } -> F3 (s,t)

bar3 :: F3 (s,t) -> Int -> F3 (s,t)
bar3 z y = z {foo3 = y}

-- GADT + data family
data family F4 a
data instance F4 (s,t) where
  MkF2a :: { foo4 :: Int } -> F4 (Int,t)

bar4 :: F4 (s,t) -> Int -> F4 (s,t)
bar4 z y = z { foo4 = y}
