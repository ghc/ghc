{-# LANGUAGE TypeApplications #-}
module ExternalWorker ( ExternalWorker(..), initExternalWorker, terminateExternalWorker, ensureExternalWorkerRunning ) where

import Data.IORef
import System.Process
import Base
import Packages
import Rules.Generate
import Target
import Context

import Settings
import Hadrian.Oracles.Cabal
import Hadrian.Haskell.Cabal.Type
import Oracles.ModuleFiles
import Utilities
import Hadrian.Expression

newtype ExternalWorker = ExternalWorker (IORef (Maybe ProcessHandle))

initExternalWorker :: IO ExternalWorker
initExternalWorker = ExternalWorker <$> newIORef Nothing

terminateExternalWorker :: ExternalWorker -> IO ()
terminateExternalWorker (ExternalWorker worker_ref) = do
  mph <- readIORef worker_ref
  case mph of
    Just ph -> terminateProcess ph
    Nothing -> return ()


ensureExternalWorkerRunning :: Stage -> Action ()
ensureExternalWorkerRunning st = do
  ExternalWorker external_worker <- userSetting @ExternalWorker (error "Worker not initialised")
  is_running <- liftIO $ readIORef  external_worker
  case is_running of
    Nothing -> do
      handle <- startExternalWorker st
      liftIO $ writeIORef external_worker (Just handle)

    -- Already running
    Just handle -> do
      mexit <- liftIO $ getProcessExitCode handle
      case mexit of
        Nothing -> return ()
        Just exit -> error ("external server, exited too soon: " ++ show exit)

-- | Create a mapping from files to which component it belongs to.
ghcArgs :: Action [String]
ghcArgs = do
  let c = (Context stage0InTree compiler (if windowsHost then vanilla else dynamic) Final)
      p = compiler
  cd <- readContextData c
  let args = "-fno-unoptimized-core-for-interpreter" : "-j8" : "-fwrite-if-simplified-core" : "-ighc" : modules cd ++ otherModules cd ++ ["ghc/Main.hs", "-fno-break-points"]

  let context = Context stage0InTree p (if windowsHost then vanilla else dynamic) Final
  let fake_target = target context
                      (Ghc ToolArgs stage0InTree) [] ["ignored"]
  -- Generate any source files for this target
  srcs <- hsSources context
  gens <- interpretInContext context generatedDependencies

  -- Build any necessary dependencies
  depPkgIds <- cabalDependencies context
  dep_confs <- mapM (\pkgId -> packageDbPath (PackageDbLoc stage0InTree Final) <&> (-/- pkgId <.> "conf")) depPkgIds

  need (gens ++ srcs ++ dep_confs)

  as <- interpret fake_target getArgs
  return (as ++ args)


startExternalWorker :: Stage -> Action ProcessHandle
startExternalWorker st = do
  putProgressInfo ("Starting external server")
  -- 1. Build the server
  spath <- serverPath st
  need [ spath ]

  -- 2. Get the arguments for the server
  args <- ghcArgs

  ghc_path <- builderPath (Ghc ToolArgs st)
  putProgressInfo (intercalate " " $ spath : ghc_path : args)

  liftIO $ spawnProcess spath (ghc_path : args)







