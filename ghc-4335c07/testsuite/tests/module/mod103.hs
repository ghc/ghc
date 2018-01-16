-- !!! Layout rule extension (restricting empty do's).
module Foo where

g :: Int -> (Int -> IO a) -> IO a
g x cont = cont x

f :: Int -> IO Int
f x = do 
  g x $ \ y -> do
  f y


