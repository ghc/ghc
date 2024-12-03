{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

foo :: Maybe a -> a
foo (Just @Int x) = x
foo Nothing = 0

main = do
  print (foo (Just 5))
