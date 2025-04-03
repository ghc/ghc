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
import Control.Monad (void)
import Data.Maybe (fromJust)
import GHC.Driver.Main
import GHC.Types.Error
import GHC.Unit.Home
import GHC.Driver.Env
import Control.Exception (ExceptionWithContext(..), SomeException)
import Control.Monad.Catch (handle, throwM)
import Control.Exception.Context
import GHC.Driver.MakeFile
import GHC.Utils.Outputable
-- | Convert a ModuleNodeCompile to a ModuleNodeFixed
convertToFixed :: ModuleNodeInfo -> ModuleNodeInfo
convertToFixed (ModuleNodeCompile ms) =
  -- Simulate having a pre-compiled module by creating a ModuleNodeFixed
  -- with the module summary information
  let modName = ms_mod_name ms
      modLoc = ms_location ms
  in ModuleNodeFixed (msKey ms) (ms_location ms) { ml_hs_file = Nothing}


-- | Load a module graph and report the result
loadModuleGraph :: ModuleGraph -> [ModuleName] -> Ghc ()
loadModuleGraph graph moduleNames = do
  liftIO $ putStrLn "Loading modules..."
  -- Check the module graph for invariant violations
  let invariantErrors = checkModuleGraph graph
  if not (null invariantErrors)
    then do
      liftIO $ putStrLn "Module graph invariant violations detected:"
      liftIO $ putStrLn $ showSDocUnsafe $ vcat (map ppr invariantErrors)
    else do
      liftIO $ putStrLn "Module graph passed invariant checks."
      dflags <- getSessionDynFlags
      doMkDependModuleGraph dflags graph
      loadResult <- load' Nothing LoadAllTargets mkUnknownDiagnostic (Just batchMsg) graph

      case loadResult of
        Failed -> do
          liftIO $ putStrLn "Compilation failed!"
          return ()
        Succeeded -> do
          liftIO $ putStrLn "Compilation succeeded!"
          liftIO $ putStrLn $ "Successfully loaded modules: " ++
                             showModuleNames moduleNames
  where
    showModuleNames [] = ""
    showModuleNames [m] = show m
    showModuleNames (m:ms) = show m ++ ", " ++ showModuleNames ms

data M = A | B | C

main :: IO ()
main = do
    [libdir] <- getArgs
    liftIO $ putStrLn "Starting GHC..."
    runGhc (Just libdir) $ handle (\(ExceptionWithContext c e :: ExceptionWithContext SomeException) -> liftIO $ putStrLn (displayExceptionContext  c) >> print e >> throwM e) $ do
      -- Set up session
      liftIO $ putStrLn "Setting up session..."
      dflags <- getSessionDynFlags
      setSessionDynFlags
        (gopt_set dflags Opt_ForceRecomp) { verbosity = 1 }
      hsc_env <- getSession
      setSession $ hscSetActiveUnitId mainUnitId hsc_env
      hsc_env <- getSession

      -- Create module names
      let mkModName name = mkModuleName name
          modNameA = mkModName "T1A"
          modNameB = mkModName "T1B"
          modNameC = mkModName "T1C"

      -- Create modules
      let modA = mkModule mainUnit modNameA
          modB = mkModule mainUnit modNameB
          modC = mkModule mainUnit modNameC
      -- Create ModSummary for each module
      -- Parse the imports from the module file
      liftIO $ putStrLn "Getting ModSummary for T1A.hs..."
      msA <- getModSummaryFromTarget "T1A.hs"
      msB <- getModSummaryFromTarget "T1B.hs"
      msC <- getModSummaryFromTarget "T1C.hs"

      -- Get file paths and create locations for our modules
      let findImports modName = case modName of
            "T1A" -> []
            "T1B" -> [NodeKey_Module (msKey msA)]
            "T1C" -> [NodeKey_Module (msKey msA), NodeKey_Module (msKey msB)]
            _     -> error "Unknown module"

      -- Create a ModuleGraph manually with the modules in the order we want
      let mgNodes f = [
            ModuleNode (findImports "T1A") (f A),
            ModuleNode (findImports "T1B") (f B),
            ModuleNode (findImports "T1C") (f C)
           ]



      -- Test 1: Normal nodes
      let
          test1 A = ModuleNodeCompile msA
          test1 B = ModuleNodeCompile msB
          test1 C = ModuleNodeCompile msC
          customGraph = mkModuleGraph (mgNodes test1)
      loadModuleGraph customGraph [modNameA, modNameB, modNameC]

      -- Test 2: All fixed nodes
      let
          test2 x = convertToFixed (test1 x)
          customGraph = mkModuleGraph (mgNodes test2)
      loadModuleGraph customGraph [modNameA, modNameB, modNameC]

      -- Test 3: Fixed, and then source
      let
          test3 C = ModuleNodeCompile msC
          test3 x = convertToFixed (test1 x)
          customGraph = mkModuleGraph (mgNodes test3)
      loadModuleGraph customGraph [modNameA, modNameB, modNameC]

      -- Test 4: Fixed depends on source, invariants fail.
      let
          test4 A = ModuleNodeCompile msA
          test4 x = convertToFixed (test1 x)
          customGraph = mkModuleGraph (mgNodes test4)
      loadModuleGraph customGraph [modNameA, modNameB, modNameC]


      -- You can now create and load different module graphs
      -- For example:
      -- let differentGraph = mkModuleGraph [...]
      -- loadModuleGraph differentGraph [...]

      where
        -- Helper to get ModSummary from a target file
        getModSummaryFromTarget :: FilePath -> Ghc ModSummary
        getModSummaryFromTarget file = do
          hsc_env <- getSession
          Right ms <- liftIO $ summariseFile hsc_env (DefiniteHomeUnit mainUnitId Nothing) mempty file Nothing Nothing
          return ms
