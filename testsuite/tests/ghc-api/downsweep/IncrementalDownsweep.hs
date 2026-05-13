{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow ((>>>))
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Directory (removeFile)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import GHC.Utils.Ppr (Mode (PageMode))
import GHC.Utils.Outputable (vcat, defaultSDocContext, printSDocLn, ppr)
import GHC.Utils.Logger (getLogger)
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Unit.Types (moduleName)
import GHC.Unit.Module.ModSummary (ms_mod)
import GHC.Unit.Module.Graph (ModuleGraph, mgModSummaries)
import GHC.Driver.DynFlags (defaultFatalMessager, defaultFlushOut)
import GHC.Driver.Monad (Ghc, getSession, getSessionDynFlags)
import GHC.Driver.Make (downsweep)
import GHC.Driver.Errors.Types (DriverMessages)
import GHC
       (
           defaultErrorHandler,
           guessTarget,
           setTargets,
           parseDynamicFlags,
           setSessionDynFlags,
           runGhc
       )

sourceDirectory :: String
sourceDirectory = "IncrementalDownsweep.modules"

withSimpleErrorHandler :: Ghc a -> Ghc a
withSimpleErrorHandler = defaultErrorHandler defaultFatalMessager
                                             defaultFlushOut

handleDriverMessages :: [DriverMessages] -> IO ()
handleDriverMessages driverMsgs
    = unless (null driverMsgs) $
      do
          printSDocLn defaultSDocContext
                      (PageMode True)
                      stderr
                      (vcat (map ppr driverMsgs))
          exitFailure

performDownsweepTurn :: Maybe ModuleGraph -> String -> Ghc ModuleGraph
performDownsweepTurn maybeGivenModuleGraph rootModuleName = do
    target <- guessTarget rootModuleName Nothing Nothing
    setTargets [target]
    session <- getSession
    (driverMsgs, resultingModuleGraph)
        <- liftIO $ downsweep session
                              mkUnknownDiagnostic
                              Nothing
                              []
                              maybeGivenModuleGraph
                              []
                              False
    liftIO $ handleDriverMessages driverMsgs
    return resultingModuleGraph

outputModuleNamesInGraph :: ModuleGraph -> IO ()
outputModuleNamesInGraph = mgModSummaries                                   >>>
                           map (ms_mod >>> moduleName >>> moduleNameString) >>>
                           sort                                             >>>
                           print

main :: IO ()
main = do
    libDir : otherArgs <- getArgs
    runGhc (Just libDir) $ withSimpleErrorHandler $ do

        -- Setup
        logger <- getLogger
        originalDynFlags <- getSessionDynFlags
        (finalDynFlags, _, _)
            <- parseDynamicFlags logger originalDynFlags $
               map noLoc (["-i", "-i" ++ sourceDirectory] ++ otherArgs)
        _ <- setSessionDynFlags finalDynFlags

        -- Turn 1: From scratch, using 'A' as root
        moduleGraph1 <- performDownsweepTurn Nothing "A"
        liftIO $ outputModuleNamesInGraph moduleGraph1

        -- Turn 2: From scratch, using 'X' as root
        -- NOTE: 'A' is not included, because it is not reachable.
        moduleGraph2 <- performDownsweepTurn Nothing "X"
        liftIO $ outputModuleNamesInGraph moduleGraph2

        -- Deletion of the source files used in turn 1
        _ <- liftIO $
             mapM_ (((sourceDirectory ++ "/") ++) >>> (++ ".hs") >>> removeFile)
                   ["A", "B", "C", "D"]

        -- Turn 3: Based on the result of turn 1, using 'X' as root
        -- NOTE: 'A' is included, because the result of turn 1 contains it.
        moduleGraph3 <- performDownsweepTurn (Just moduleGraph1) "X"
        liftIO $ outputModuleNamesInGraph moduleGraph3
