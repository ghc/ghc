{-# LANGUAGE ApplicativeDo, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module T20540 where

import Data.Functor.Identity

newtype F a = F (Identity a)
  deriving newtype (Functor, Applicative, Show)

x :: F Int
x = do
  return 3

y :: F Int
y = do
  let a = 3
  let b = 4
  let c = 5
  return $ a + b + c
