{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

Just @a x = Just (5 :: Integer)

main = do
  print (x :: a)
