{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data T a = MkT a a

foo1 (MkT x @a y) = (x :: a)
foo2 (MkT x @a y @b) = (x :: a)

main = do
  print @Bool (foo1 (MkT True False))
  print @Bool (foo2 (MkT True False))
