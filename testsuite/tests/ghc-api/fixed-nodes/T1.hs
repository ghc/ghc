{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Monad
import System.Environment
import Control.Monad (void)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      -- Set up session
      dflags <- getSessionDynFlags

      -- Set targets for our modules
      targets <- mapM (guessTarget Nothing) ["T1A.hs", "T1B.hs", "T1C.hs"]

      -- Add all targets
      setTargets targets

      -- Load modules
      loadResult <- load LoadAllTargets

      case loadResult of
        Failed -> do
          liftIO $ putStrLn "Compilation failed!"
          return ()
        Succeeded -> do
          liftIO $ putStrLn "Compilation succeeded!"

          -- Try to get the modules
          modA <- getModSummary (mkModuleName "T1A")
          modB <- getModSummary (mkModuleName "T1B")
          modC <- getModSummary (mkModuleName "T1C")

          liftIO $ putStrLn $ "Found modules: " ++
                              show (ms_mod_name modA) ++ ", " ++
                              show (ms_mod_name modB) ++ ", " ++
                              show (ms_mod_name modC)