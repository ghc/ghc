-- 1. Load a set of modules with "nothing" target
-- 2. Load it again with "interpreted" target
-- 3. Execute some code
--    a. If the recompilation checker is buggy this will die due to missing
--       code
--    b. If it's correct, it will recompile because the target has changed.
--
-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Utils.Monad ( MonadIO(..) )
import GHC.Types.Basic ( failed )
import GHC.Data.Bag        ( bagToList )
import System.Environment
import Control.Monad
import System.IO

main = do
  libdir : args <- getArgs
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    logger <- getLogger
    (dflags, _, _) <- parseDynamicFlags logger dflags0
      (map (mkGeneralLocated "on the commandline") args)
    setSessionDynFlags $ dflags { backend   = noBackend
                                , ghcLink   = LinkInMemory
                                , verbosity = 0 -- silence please
                                }
    root_mod <- guessTarget "A.hs" Nothing Nothing
    setTargets [root_mod]
    ok <- load LoadAllTargets
    when (failed ok) $ error "Couldn't load A.hs in nothing mode"
    prn "target nothing: ok"

    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { backend = interpreterBackend }
    ok <- load LoadAllTargets
    when (failed ok) $ error "Couldn't load A.hs in interpreted mode"
    prn "target interpreted: ok"

    -- set context to module "A"
    mg <- getModuleGraph
    let [mod] = [ ms_mod m
                | m <- mgModSummaries mg
                , moduleNameString (ms_mod_name m) == "A" ]
    setContext [IIModule mod]
    liftIO $ hFlush stdout  -- make sure things above are printed before
                            -- interactive output
    r <- execStmt "main" execOptions
    case r of
      ExecComplete { execResult = Right _ } -> prn "ok"
      ExecComplete { execResult = Left _ } -> prn "exception"
      ExecBreak{} -> prn "breakpoint"
    liftIO $ hFlush stdout
    return ()

prn :: MonadIO m => String -> m ()
prn = liftIO . putStrLn
