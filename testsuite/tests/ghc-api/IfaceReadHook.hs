-- | GHC.Driver.Hooks.ifaceReadHook is called with the full interface each
-- time an interface file is read from disk.
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (sort, group)
import System.Environment

import GHC
import GHC.Driver.Env (hsc_hooks)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Hooks (ifaceReadHook)
import GHC.Unit.Module.ModIface (mi_decls)

main :: IO ()
main = do
  [libdir] <- getArgs
  seen <- newIORef []
  let hook path iface = do
        let !n = length (mi_decls iface)
        modifyIORef' seen ((path, moduleNameString (moduleName (mi_module iface)), n) :)
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags { backend = noBackend, ghcLink = NoLink }
    modifySession $ \hsc_env ->
      hsc_env { hsc_hooks = (hsc_hooks hsc_env) { ifaceReadHook = Just hook } }
    target <- guessTarget "IfaceReadHook_A.hs" Nothing Nothing
    setTargets [target]
    ok <- load LoadAllTargets
    liftIO $ print (succeeded ok)
  reads_ <- readIORef seen
  let mods  = [ m | (_, m, _) <- reads_ ]
      paths = [ p | (p, _, _) <- reads_ ]
  putStrLn ("saw Data.Maybe: " ++ show ("Data.Maybe" `elem` mods))
  -- the hook gets full interfaces, including declarations
  putStrLn ("saw declarations: " ++ show (any (\(_, _, n) -> n > 0) reads_))
  putStrLn ("files read twice: " ++ show [ head g | g <- group (sort paths), length g > 1 ])
