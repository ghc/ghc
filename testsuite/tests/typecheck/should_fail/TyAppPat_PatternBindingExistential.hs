{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Some = forall a. Some a

Some @a x = Some (5 :: Integer)

main = do
  print (x :: a)
