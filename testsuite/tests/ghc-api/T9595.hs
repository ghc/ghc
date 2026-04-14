module Main where

import GHC
import GHC.Unit.State
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Env (hscUnitIndexQuery)
import GHC.Utils.Outputable
import GHC.Unit.Module
import System.Environment

main =
  do [libdir] <- getArgs
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                hsc_env <- getSession
                let state = hsc_units hsc_env
                query <- liftIO $ hscUnitIndexQuery hsc_env
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state query)
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                hsc_env <- getSession
                let state = hsc_units hsc_env
                query <- liftIO $ hscUnitIndexQuery hsc_env
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state query)
     return ()
