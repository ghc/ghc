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
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames dflags)
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                dflags <- getSessionDynFlags
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames dflags)
     return ()
