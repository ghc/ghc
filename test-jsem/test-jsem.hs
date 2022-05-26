#!/usr/bin/env runhaskell

import System.Environment
import System.Process
import System.Posix.Semaphore
import Control.Exception
import System.Posix.Files (stdFileMode)
import System.Exit


makeSem :: String -> Int -> IO (String, Semaphore)
makeSem n count = (n,) <$> semOpen n
  OpenSemFlags { semCreate = True, semExclusive = True }
  stdFileMode count

cleanupSem :: (String, Semaphore) -> IO ()
cleanupSem (n, _) = semUnlink n

main :: IO ()
main = do
  Just sem_name <- lookupEnv "GHC_JSEM_PATH"
  Just sem_count <- fmap read <$> lookupEnv "GHC_JSEM_COUNT"
  r <- bracket (makeSem sem_name sem_count) cleanupSem $ \_ -> do
    args <- getArgs
    (_,_,_,h) <- createProcess ((proc "cabal" args) { delegate_ctlc = True })
    waitForProcess h
  exitWith r
