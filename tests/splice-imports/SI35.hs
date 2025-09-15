{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Driver.Make (load', summariseFile)
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Unit.Module.ModIface
import GHC.Unit.Module
import GHC.Unit.Module.ModNodeKey
import GHC.Types.SourceFile
import System.Environment
import Control.Monad (void, when)
import Data.Maybe (fromJust)
import Control.Exception (ExceptionWithContext(..), SomeException)
import Control.Monad.Catch (handle, throwM)
import Control.Exception.Context
import GHC.Utils.Outputable
import GHC.Unit.Home
import GHC.Driver.Env
import Data.List (sort)
import GHC.Driver.MakeFile
import GHC.Data.Maybe
import GHC.Unit.Module.Stage
import GHC.Data.Graph.Directed.Reachability
import GHC.Utils.Trace
import GHC.Unit.Module.Graph

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ handle (\(ExceptionWithContext c e :: ExceptionWithContext SomeException) ->
      liftIO $ putStrLn (displayExceptionContext c) >> print e >> throwM e) $ do

      -- Set up session
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      hsc_env <- getSession
      setSession $ hscSetActiveUnitId mainUnitId hsc_env

      -- Get ModSummary for our test module
      msA <- getModSummaryFromTarget "SI35A.hs"

      -- Define NodeKey
      let keyA = NodeKey_Module (msKey msA)
          edgeA = mkNormalEdge keyA

      -- Define ModuleNodeInfo
      let infoA_compile = ModuleNodeCompile msA

      -- Define the complete node
      let nodeA_compile = ModuleNode [] infoA_compile

      -- This test checks that a module required at compile stage invokes a
      -- depedency on the runstage of itself when using TemplateHaskellQuotes.

      -- This is hard to test with a normal compiler invocation as GHC does not
      -- not distinguish very easily these two stages.
      let (ri, to_node) = mkStageDeps [nodeA_compile]
      let reachable = allReachable ri (expectJust $ to_node (keyA, CompileStage))
      let reachable_nodes = map stageSummaryNodeSummary reachable

      if (keyA, RunStage) `elem` reachable_nodes
        then return ()
        else do
          liftIO $ putStrLn "Test failed -- (keyA, RunStage) not reachable"
          pprTraceM "reachable_nodes" (ppr reachable_nodes)
          pprTraceM "reachable" (ppr (reachabilityIndexMembers ri))

      where
        -- Helper to get ModSummary from a target file
        getModSummaryFromTarget :: FilePath -> Ghc ModSummary
        getModSummaryFromTarget file = do
          hsc_env <- getSession
          Right ms <- liftIO $ summariseFile hsc_env (DefiniteHomeUnit mainUnitId Nothing) mempty file Nothing Nothing
          return ms