{-# LANGUAGE BlockArguments, LambdaCase #-}

module BlockArgumentsLambdaCase where

import Control.Monad

foo' :: IO ()
foo' = do
  forM [Just 3, Nothing] \case
    Just 3 -> print 3
    _ -> print 5

  return ()
