module Main where

import GHC
import GHC.Unit.State
import GHC.Driver.Monad
import GHC.Utils.Outputable
import System.Environment
import GHC.Driver.Session
import GHC.Unit.Module

main =
  do [libdir] <- getArgs
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                dflags <- getSessionDynFlags
                let state = unitState dflags
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state)
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                dflags <- getSessionDynFlags
                let state = unitState dflags
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state)
     return ()
