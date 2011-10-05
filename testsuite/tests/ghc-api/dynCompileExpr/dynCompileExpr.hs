
module Main where

import GHC
import MonadUtils

import System.Environment

main :: IO ()
main = do [libdir] <- getArgs
          runGhc (Just libdir) doit

doit :: Ghc ()
doit = do
  getSessionDynFlags >>= setSessionDynFlags
  dyn <- dynCompileExpr "()"
  liftIO $ print dyn

