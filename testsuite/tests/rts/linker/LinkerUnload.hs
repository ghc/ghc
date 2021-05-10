module LinkerUnload (init) where

import GHC
import GHC.Unit.State
import GHC.Unit.Env
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
    let logger     = hsc_logger hsc_env
    let interp     = hscInterp hsc_env
    let unit_env   = hsc_unit_env hsc_env
    let unit_state = ue_units unit_env
    let dflags     = hsc_dflags hsc_env
    let tmpfs      = hsc_tmpfs hsc_env
    liftIO $ Loader.initLoaderState logger tmpfs interp dflags unit_env
    liftIO $ Loader.loadPackages logger interp unit_state dflags (preloadUnits unit_state)
