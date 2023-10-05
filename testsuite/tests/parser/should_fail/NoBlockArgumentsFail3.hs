{-# LANGUAGE LambdaCase #-}
module NoBlockArgumentsFail3 where

import Control.Monad

foo :: IO ()
foo = forM [1 .. 10] \case
  Just 3 -> print x
