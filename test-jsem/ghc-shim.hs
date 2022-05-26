#!/usr/bin/env runhaskell

import System.Environment
import System.Process
import System.Posix.Semaphore
import Control.Exception
import System.Posix.Files (stdFileMode)
import System.Exit


makeSem :: String -> IO (String, Semaphore)
makeSem n = do
  s <- semOpen n
    OpenSemFlags { semCreate = False, semExclusive = False }
    stdFileMode 0
  semWait s
  pure (n,s)

cleanupSem :: (String, Semaphore) -> IO ()
cleanupSem (_, s) = semPost s

main :: IO ()
main = do
  Just sem_name <- lookupEnv "GHC_JSEM_PATH"
  Just ghc <- lookupEnv "GHC_SHIM_PATH"
  r <- bracket (makeSem sem_name) cleanupSem $ \_ -> do
    args <- getArgs
    (_,_,_,h) <- createProcess ((proc ghc args) { delegate_ctlc = True })
    waitForProcess h
  exitWith r
