module Main where

import GHC
import GHC.Run
import GHC.Unit.State
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Utils.Outputable
import GHC.Unit.Module
import System.Environment

main =
  do [libdir] <- getArgs
     _ <- runGhcWithAbiHashes (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                state <- hsc_units <$> getSession
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state)
     _ <- runGhcWithAbiHashes (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                state <- hsc_units <$> getSession
                liftIO $ print (mkModuleName "GHC.Utils.Outputable" `elem` listVisibleModuleNames state)
     return ()
