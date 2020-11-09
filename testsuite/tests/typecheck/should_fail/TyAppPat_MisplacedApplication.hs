{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data T a = MkT a a

foo (MkT x @a y) = (x :: a)

main = do
  print (foo (MkT True False))
