{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE DeriveFunctor #-}

module T18824 where

import Control.Applicative

data Box a = Box a
  deriving Functor

instance Applicative Box where
  pure = Box
  Box f <*> Box a = Box (f a)

data X = X
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)

mb :: (String -> Box a) -> String -> Box (Maybe a)
mb _ ""  = Box Nothing
mb _ "-" = Box Nothing
mb p xs  = Just <$> p xs

run :: [String] -> Box X
run
  [ x1
  , x2
  , x3
  , x4
  , x5
  ] = X
  <$> mb pure x1
  <*> mb pure x2
  <*> mb pure x3
  <*> mb pure x4
  <*> mb pure x5
