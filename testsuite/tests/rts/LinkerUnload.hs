module LinkerUnload (init) where

import GHC
import DynFlags
import Linker
import System.Environment
import MonadUtils ( MonadIO(..) )

foreign export ccall loadPackages :: IO ()

loadPackages :: IO ()
loadPackages = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags { hscTarget = HscNothing
                         , ghcLink  = LinkInMemory }
    pkgs <- setSessionDynFlags dflags'
    dflags <- getSessionDynFlags
    liftIO $ Linker.linkPackages dflags pkgs
