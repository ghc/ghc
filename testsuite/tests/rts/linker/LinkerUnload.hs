module LinkerUnload (init) where

import GHC
import GHC.Driver.Session
import GHC.Runtime.Linker as Linker
import System.Environment
import GHC.Utils.Monad ( MonadIO(..) )

foreign export ccall loadPackages :: IO ()

loadPackages :: IO ()
loadPackages = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags { hscTarget = HscNothing
                         , ghcLink  = LinkInMemory }
    pkgs <- setSessionDynFlags dflags'
    hsc_env <- getSession
    liftIO $ Linker.linkPackages hsc_env pkgs
