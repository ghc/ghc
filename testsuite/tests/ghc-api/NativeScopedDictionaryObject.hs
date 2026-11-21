module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC (TcMPluginHandling(StartAndKeepRunningTcMPlugins), coreModule, depanal, desugarModule,
            getSession, getSessionDynFlags, guessTarget, mgModSummaries, parseModule,
            runGhc, setSessionDynFlags, setTargets, typecheckModule)
import GHC.Core.Lint (lintScopedDictionaryPassResult)
import GHC.Core.Opt.Pipeline.Types (CoreToDo(CoreDesugar))
import GHC.Core.SimpleOpt (simpleOptNativeProgram)
import GHC.Driver.Config.Core.Lint (initLintPassResultConfig)
import GHC.Driver.DynFlags (dopt_set, gopt_set, gopt_unset)
import GHC.Driver.Env (hscSetFlags, hsc_logger, hsc_dflags)
import GHC.Driver.Flags (DumpFlag(Opt_D_ppr_debug), GeneralFlag(Opt_DoCoreLinting, Opt_OmitInterfacePragmas))
import GHC.Driver.Main.Compile (hscGenScopedDictionaryHardCode)
import GHC.Driver.Main.Passes (hscTidyScopedDictionary)
import GHC.Driver.Session (updOptLevel)
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Unit.Module.ModSummary (ms_hspp_opts, ms_location)
import System.Environment (getArgs)

-- | Compile one source module to native assembly without constructing a ModIface.
main :: IO ()
main = do
    [lib, input, output, opt] <- getArgs
    runGhc (Just lib) $ do
        flags <- getSessionDynFlags
        _ <- setSessionDynFlags $
            (\df -> gopt_unset df Opt_DoCoreLinting)
            (gopt_set (dopt_set (updOptLevel (read opt) flags) Opt_D_ppr_debug) Opt_OmitInterfacePragmas)
        target <- guessTarget input Nothing Nothing
        setTargets [target]
        [summary] <- mgModSummaries <$> depanal [] True
        parsed <- parseModule summary
        checked <- typecheckModule StartAndKeepRunningTcMPlugins parsed
        desugared <- desugarModule checked
        env <- hscSetFlags (ms_hspp_opts summary) <$> getSession
        liftIO $ do
            putStrLn "source desugared"
            let modGuts = coreModule desugared
                config = initLintPassResultConfig (hsc_dflags env) [] CoreDesugar
            lintScopedDictionaryPassResult (hsc_logger env) config (mg_binds modGuts)
            let simplified = modGuts { mg_binds = simpleOptNativeProgram (mg_binds modGuts) }
            (guts, _) <- hscTidyScopedDictionary env simplified
            putStrLn "scoped Core checked and tidied"
            (assembly, stub, foreignObjects, _, _) <-
                hscGenScopedDictionaryHardCode env guts (ms_location summary) output
            unless (assembly == output && stub == Nothing && null foreignObjects) $
                fail "unexpected native output"
            putStrLn assembly
