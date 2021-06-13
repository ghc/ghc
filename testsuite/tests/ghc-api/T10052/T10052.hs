{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment
import GHC
import GHC.Run

main :: IO ()
main = do
    flags <- getArgs
    runGhc' flags $ do
      dflags <- getSessionDynFlags
      setTargets [Target (TargetFile "T10052-input.hs" Nothing) True (homeUnitId_ dflags) Nothing]
      _success <- load LoadAllTargets
      return ()

runGhc' :: [String] -> Ghc a -> IO a
runGhc' args act = do
    let libdir = head args
        flags  = map noLoc (tail args)
    runGhcWithAbiHashes (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      logger <- getLogger
      (dflags1, _leftover, _warns) <- parseDynamicFlags logger dflags0 flags
      let dflags2 = dflags1 {
              backend   = Interpreter
            , ghcLink   = LinkInMemory
            , verbosity = 1
            }
      _newPkgs <- setSessionDynFlags dflags2
      act
