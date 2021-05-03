module LinkerUnload (init) where

import GHC
import GHC.Unit.State
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Loader
import System.Environment
import GHC.Utils.Monad ( MonadIO(..) )

foreign export ccall loadPackages :: IO ()

loadPackages :: IO ()
loadPackages = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags { backend = NoBackend
                         , ghcLink = LinkInMemory }
    setSessionDynFlags dflags'
    hsc_env <- getSession
    let interp = hscInterp hsc_env
    liftIO $ Loader.initLoaderState interp hsc_env
    liftIO $ Loader.loadPackages interp hsc_env (preloadUnits (hsc_units hsc_env))
