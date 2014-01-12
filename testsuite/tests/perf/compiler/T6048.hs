module T6048 where

import Control.Applicative

data X = X
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)
  (Maybe String)

mb :: (String -> Maybe a) -> String -> Maybe (Maybe a)
mb _ ""  = Just Nothing
mb _ "-" = Just Nothing
mb p xs  = Just <$> p xs

run :: [String] -> Maybe X
run
  [ x1
  , x2
  , x3
  , x4
  , x5
  , x6
  , x7
  , x8
  , x9
  ] = X
  <$> mb pure x1
  <*> mb pure x2
  <*> mb pure x3
  <*> mb pure x4
  <*> mb pure x5
  <*> mb pure x6
  <*> mb pure x7
  <*> mb pure x8
  <*> mb pure x9

