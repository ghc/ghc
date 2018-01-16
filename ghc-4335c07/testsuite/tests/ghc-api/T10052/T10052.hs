{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment
import GHC

main :: IO ()
main = do
    flags <- getArgs
    runGhc' flags $ do
      setTargets [Target (TargetFile "T10052-input.hs" Nothing) True Nothing]
      _success <- load LoadAllTargets
      return ()

runGhc' :: [String] -> Ghc a -> IO a
runGhc' args act = do
    let libdir = head args
        flags  = map noLoc (tail args)
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      (dflags1, _leftover, _warns) <- parseDynamicFlags dflags0 flags
      let dflags2 = dflags1 {
              hscTarget = HscInterpreted
            , ghcLink   = LinkInMemory
            , verbosity = 1
            }
      _newPkgs <- setSessionDynFlags dflags2
      act
