{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

x :: Integer
Just @a x = Just (5 :: Integer)

main = do
  print x
