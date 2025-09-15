{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import GHC.Prim.Exception
import GHC.Exception.Type
import Control.Exception
import System.Mem

main ::IO ()
main = do
  -- evaluate the CAF
  evaluate raiseUnderflow
    `catch` \case
      Underflow -> pure ()
      e         -> putStrLn "Unexpected exception"

  -- free it with buggy compiler
  performGC

  -- reuse it
  evaluate raiseUnderflow
    `catch` \case
      Underflow -> pure ()
      e         -> putStrLn "Unexpected exception"
