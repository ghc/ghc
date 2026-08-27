{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Driver.Env
import qualified Data.Map.Strict as Map
import GHC.Data.OsPath (unsafeEncodeUtf)
import GHC.Driver.MakeAction
import GHC.Driver.Downsweep
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Unit.Module.ModNodeKey
import GHC.Types.SourceFile
import System.Environment
import Control.Monad (void, when)
import Data.Maybe (fromJust)
import Control.Concurrent.MVar
import Control.Exception (ExceptionWithContext(..), SomeException)
import Control.Monad.Catch (handle, throwM)
import Control.Exception.Context
import GHC.Utils.Outputable
import Data.List
import GHC.Types.Error
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Tc.Utils.Monad
import GHC.Iface.Env
import GHC.Driver.Ppr
import GHC.Unit.Home


main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ handle (\(ExceptionWithContext c e :: ExceptionWithContext SomeException) ->
      liftIO $ putStrLn (displayExceptionContext c) >> print e >> throwM e) $ do

      -- Set up session
      dflags <- getSessionDynFlags
      setSessionDynFlags (dflags { verbosity = 1 })
      hsc_env <- getSession
      setSession $ hscSetActiveUnitId mainUnitId hsc_env

      -- Get ModSummaries for our test modules
      msA <- getModSummaryFromTarget "T1A.hs"
      msB <- getModSummaryFromTarget "T1B.hs"
      msC <- getModSummaryFromTarget "T1C.hs"

      let targets = [ Target (TargetModule (ms_mod_name msA)) True (moduleUnitId $ ms_mod msA) Nothing
                    , Target (TargetModule (ms_mod_name msB)) True (moduleUnitId $ ms_mod msB) Nothing
                    , Target (TargetModule (ms_mod_name msC)) True (moduleUnitId $ ms_mod msC) Nothing
                    ]

      setTargets targets

      -- Compile interfaces for our modules
      load LoadAllTargets

      hsc_env <- getSession
      setSession $ hsc_env { hsc_dflags = (hsc_dflags hsc_env) { ghcMode = OneShot } }
      hsc_env <- getSession

      -- Create ModNodeKeys with unit IDs
      let keyA = msKey msA
          keyB = msKey msB
          keyC = msKey msC

      let mkGraph roots = do
            let env = DownsweepEnv
                  { ds_hsc_env   = hsc_env
                  , ds_mode      = DownsweepUseFixed
                  , ds_prior     = Map.fromList [ (root_key ms, (ms, SummFresh)) | ms <- roots ]
                  , ds_excl_mods = []
                  }
            ([], nodes, _) <- downsweepFromRootNodes env emptyDownsweepCache
              (mempty { dr_summarise = map root_key roots })
            return $ mkModuleGraph nodes
          root_key ms = (ms_unitid ms, fromJust (ml_hs_file_ospath (ms_location ms)))

      graph <- liftIO $ mkGraph [msC]

      liftIO $ putStrLn "loaded"
      -- 1. Check that the module graph is valid
      let invariantErrors = checkModuleGraph graph

      case invariantErrors of
        [] -> liftIO $ putStrLn "PASS Test passed"
        errors -> do
          liftIO $ putStrLn "FAIL Test failed - invariant violations"
          liftIO $ putStrLn $ showSDoc dflags $ vcat (map ppr errors)

      -- 2. Check that from the root, we can reach the "ghc-internal" package.
      let ghcInternalPackage = NodeKey_ExternalUnit ghcInternalUnitId
      let root = NodeKey_Module keyC
      let reached = mgQuery graph root ghcInternalPackage
      if not reached
        then liftIO $ putStrLn "FAIL Test failed - cannot reach ghc-internal"
        else liftIO $ putStrLn "PASS Test passed"



      where

        -- Helper to get ModSummary from a target file
        getModSummaryFromTarget :: FilePath -> Ghc ModSummary
        getModSummaryFromTarget file = do
          hsc_env <- getSession
          SummariseFound ms <- liftIO $
            summariseSourceFile hsc_env (DefiniteHomeUnit mainUnitId Nothing)
              defaultSourceFileOptions (unsafeEncodeUtf file)
          return ms
