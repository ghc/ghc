{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Some = forall a. Some a

foo (Some @a x) = (x :: a)

main = do
  print (foo (Some (5 :: Integer)) :: Integer)
