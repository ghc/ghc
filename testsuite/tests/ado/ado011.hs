{-# LANGUAGE ApplicativeDo, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Functor.Identity

newtype F a = F (Identity a)
  deriving newtype (Functor, Applicative, Show)

x :: F (Int, Int)
x = do
  a <- pure 0
  let b = 1
  pure (a, b)

y :: F (Int, Int, Int)
y = do
   a <- pure 0
   let b = 1
   let c = b + 1
   pure (a, b, c)

main :: IO ()
main = do
  print x
  print y
