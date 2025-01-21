{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Driver.Make (summariseFile)
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Unit.Module.ModNodeKey
import GHC.Types.SourceFile
import System.Environment
import Control.Monad (void, when)
import Data.Maybe (fromJust)
import GHC.Driver.Main
import GHC.Types.Error
import GHC.Unit.Home
import GHC.Driver.Env
import Control.Exception (ExceptionWithContext(..), SomeException)
import Control.Monad.Catch (handle, throwM)
import Control.Exception.Context
import GHC.Utils.Outputable
import Data.List

-- | Convert a ModuleNodeCompile to a ModuleNodeFixed
convertToFixed :: ModuleNodeInfo -> ModuleNodeInfo
convertToFixed (ModuleNodeCompile ms) =
  let modName = ms_mod_name ms
      modLoc = ms_location ms
  in ModuleNodeFixed (msKey ms) (ms_location ms) { ml_hs_file = Nothing}

-- | Test a module graph and report if it matches expected invariant violations
testModuleGraph :: String -> ModuleGraph -> [ModuleGraphInvariantError] -> Ghc ()
testModuleGraph testName graph expectedErrors = do
  liftIO $ putStrLn $ "\nRunning test: " ++ testName
  let actualErrors = checkModuleGraph graph

  if length actualErrors /= length expectedErrors
    then do
      liftIO $ putStrLn "FAIL Test failed - wrong number of errors"
      liftIO $ putStrLn $ "Expected " ++ show (length expectedErrors) ++ " errors, got " ++ show (length actualErrors)
      liftIO $ putStrLn "Expected errors:"
      liftIO $ putStrLn $ showSDocUnsafe $ vcat (map ppr expectedErrors)
      liftIO $ putStrLn "Actual errors:"
      liftIO $ putStrLn $ showSDocUnsafe $ vcat (map ppr actualErrors)
    else do
      let matches = and $ zipWith (==)
                                 (sort actualErrors)
                                 (sort expectedErrors)
      if matches
        then liftIO $ putStrLn "PASS Test passed"
        else do
          liftIO $ putStrLn "FAIL Test failed - wrong error types"
          liftIO $ putStrLn "Expected errors:"
          liftIO $ putStrLn $ showSDocUnsafe $ vcat (map ppr expectedErrors)
          liftIO $ putStrLn "Actual errors:"
          liftIO $ putStrLn $ showSDocUnsafe $ vcat (map ppr actualErrors)

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

      -- Get ModSummaries for our test modules
      msA <- getModSummaryFromTarget "T1A.hs"
      msB <- getModSummaryFromTarget "T1B.hs"
      msC <- getModSummaryFromTarget "T1C.hs"

      -- Define NodeKeys
      let keyA = NodeKey_Module (msKey msA)
          keyB = NodeKey_Module (msKey msB)
          keyC = NodeKey_Module (msKey msC)

          edgeA = mkNormalEdge keyA
          edgeB = mkNormalEdge keyB
          edgeC = mkNormalEdge keyC

      -- Define ModuleNodeInfos
      let infoA_compile = ModuleNodeCompile msA
          infoB_compile = ModuleNodeCompile msB
          infoC_compile = ModuleNodeCompile msC

          infoA_fixed = convertToFixed $ ModuleNodeCompile msA
          infoB_fixed = convertToFixed $ ModuleNodeCompile msB
          infoC_fixed = convertToFixed $ ModuleNodeCompile msC

      -- Define the complete nodes
      let nodeA_compile = ModuleNode [] infoA_compile
          nodeB_compile = ModuleNode [edgeA] infoB_compile
          nodeC_compile = ModuleNode [edgeA, edgeB] infoC_compile

          nodeA_fixed = ModuleNode [] infoA_fixed
          nodeB_fixed = ModuleNode [edgeA] infoB_fixed
          nodeC_fixed = ModuleNode [edgeA, edgeB] infoC_fixed

      -- Test 1: Valid graph with all compile nodes
      let validGraph = mkModuleGraph [nodeA_compile, nodeB_compile, nodeC_compile]
      testModuleGraph "Valid compile nodes" validGraph []

      -- Test 2: Valid graph with all fixed nodes
      let allFixedGraph = mkModuleGraph [nodeA_fixed, nodeB_fixed, nodeC_fixed]
      testModuleGraph "Valid fixed nodes" allFixedGraph []

      -- Test 3: Invalid - Fixed node depending on compile node
      let mixedInvalidGraph = mkModuleGraph [nodeA_compile, nodeB_fixed]
      testModuleGraph "Fixed depending on compile" mixedInvalidGraph
        [FixedNodeDependsOnCompileNode (msKey msB) [keyA]]

      -- Test 4: Invalid - Duplicate node keys
      let duplicateGraph = mkModuleGraph [nodeA_compile, nodeA_compile]
      testModuleGraph "Duplicate keys" duplicateGraph
        [DuplicateModuleNodeKey keyA]

      -- Test 5: Invalid - Missing dependency
      let nodeB_noDepends = ModuleNode [] infoB_compile
          nodeC_missingDep = ModuleNode [edgeA] infoC_compile
          missingDepGraph = mkModuleGraph [nodeB_noDepends, nodeC_missingDep]
      testModuleGraph "Missing dependency" missingDepGraph
        [DependencyNotInGraph keyC [keyA]]

      where
        -- Helper to get ModSummary from a target file
        getModSummaryFromTarget :: FilePath -> Ghc ModSummary
        getModSummaryFromTarget file = do
          hsc_env <- getSession
          Right ms <- liftIO $ summariseFile hsc_env (DefiniteHomeUnit mainUnitId Nothing) mempty file Nothing Nothing
          return ms
