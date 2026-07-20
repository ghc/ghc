import GHC
import GHC.Data.OsPath
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Unit.Env
import GHC.Plugins
import GHC.Prelude

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Environment

main :: IO ()
main = do
  libdir : extraArgs <- getArgs
  runGhcT (Just libdir) $ do
    session <- getSession
    df1 <- getSessionDynFlags
    (df, _leftovers, _) <- parseDynamicFlags (hsc_logger session) df1 (map noLoc extraArgs)
    -- The first call simulates having modified the DynFlags once before
    setSessionDynFlags df
    t <- guessTarget "Hello.hs" Nothing Nothing
    setTargets [t]
    r <- load LoadAllTargets
    when (succeeded r) $ do
      liftIO $ throwIO $ ErrorCall "Should not have been able to load Hello as missing package db"

    -- Add the missing package db and dependency by changing the `PackageDBFlags`
    setSessionDynFlags $
      df { packageDBFlags = PackageDB (PkgDbPath $ os "test.package.conf.d") : (packageDBFlags df)
        , packageFlags = [ExposePackage "test" (PackageArg "test") (ModRenaming True [])]
      }

    hsc_env <- getSession
    r <- load LoadAllTargets
    when (failed r) $ do
      liftIO $ throwIO $ ErrorCall "Failed to load the target"

    liftIO $ putStrLn "Successfully changed the PackageDBFlags via setSessionDynFlags"

