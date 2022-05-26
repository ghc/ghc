#!/usr/bin/env runhaskell

import System.Environment
import System.Process
import System.Posix.Semaphore
import Control.Exception
import System.Posix.Files (stdFileMode)
import System.Exit


makeSem :: String -> IO (String, Semaphore)
makeSem n = (n,) <$> semOpen n OpenSemFlags { semCreate = True, semExclusive = True } stdFileMode 0

cleanupSem :: (String, Semaphore) -> IO ()
cleanupSem (n, _) = semUnlink n

main :: IO ()
main = do
  Just sem_name <- lookupEnv "GHC_JSEM"
  r <- bracket (makeSem sem_name) cleanupSem $ \_ -> do
    args <- getArgs
    (_,_,_,h) <- createProcess ((proc "cabal" args) { delegate_ctlc = True })
    waitForProcess h
  exitWith r
