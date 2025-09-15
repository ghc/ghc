{-# LANGUAGE BlockArguments #-}

module BlockArguments where

import Control.Monad

foo :: IO ()
foo = when True do
  return ()

foo' :: IO ()
foo' = do
  forM [1 .. 10] \x ->
    print x

  forM [1 .. 10] \x -> do
    print x
    print x

  return ()

foo'' :: IO ()
foo'' = when
  do True
  do return ()
