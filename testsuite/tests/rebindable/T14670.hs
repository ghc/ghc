{-# LANGUAGE RebindableSyntax #-}

module Lib where

import Prelude (IO)

pure = undefined

foo :: IO ()
foo = do
  pure ()
