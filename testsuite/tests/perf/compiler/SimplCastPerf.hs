{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module SimplCastPerf where

infixr 5 :*
data a :* b
data HNil

type family Hd a where
  Hd Int = Bool

type family Wrap a where
  Wrap (Bool :* r) = Bool :* r

-- A very large type.
type T =
  Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int :* Int
      :* Int :* Int :* Int :* HNil

a0 :: Hd Int :* T
a0 = undefined
b0 :: Bool :* T
b0 = a0
c0 :: Wrap (Bool :* T)
c0 = b0

a1 :: Hd Int :* T
a1 = undefined
b1 :: Bool :* T
b1 = a1
c1 :: Wrap (Bool :* T)
c1 = b1

a2 :: Hd Int :* T
a2 = undefined
b2 :: Bool :* T
b2 = a2
c2 :: Wrap (Bool :* T)
c2 = b2

a3 :: Hd Int :* T
a3 = undefined
b3 :: Bool :* T
b3 = a3
c3 :: Wrap (Bool :* T)
c3 = b3

a4 :: Hd Int :* T
a4 = undefined
b4 :: Bool :* T
b4 = a4
c4 :: Wrap (Bool :* T)
c4 = b4

a5 :: Hd Int :* T
a5 = undefined
b5 :: Bool :* T
b5 = a5
c5 :: Wrap (Bool :* T)
c5 = b5

a6 :: Hd Int :* T
a6 = undefined
b6 :: Bool :* T
b6 = a6
c6 :: Wrap (Bool :* T)
c6 = b6

a7 :: Hd Int :* T
a7 = undefined
b7 :: Bool :* T
b7 = a7
c7 :: Wrap (Bool :* T)
c7 = b7

a8 :: Hd Int :* T
a8 = undefined
b8 :: Bool :* T
b8 = a8
c8 :: Wrap (Bool :* T)
c8 = b8

a9 :: Hd Int :* T
a9 = undefined
b9 :: Bool :* T
b9 = a9
c9 :: Wrap (Bool :* T)
c9 = b9

data Box =
  Box
    { box1, box2, box3, box4, box5, box6, box7, box8, box9, box10 :: Wrap (Bool :* T)
    }

box :: Box
box = Box c0 c1 c2 c3 c4 c5 c6 c7 c8 c9
