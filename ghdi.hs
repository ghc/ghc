-- Import necessary modules
import GHC
import GHC.Driver.Config.Parser
import GHC.Driver.Env.Types
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Unit.Types
import GHC.Unit.Module.ModGuts
import GHC.Data.StringBuffer
import GHC.Data.FastString
import qualified GHC.Parser.Lexer as L
import qualified GHC.Parser as P
import GHC.Types.SrcLoc
import GHC.Core
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import Data.List
import GHC.Types.Name
import GHC.Core.Semantics
import qualified GHC.LanguageExtensions as LangExt

import System.Console.Haskeline

indent :: Int -> String -> String
indent n = unlines . map (\s -> replicate n ' ' ++ s) . lines

pprPrint :: Outputable a => a -> IO ()
pprPrint = putStrLn . showSDocUnsafe . ppr

compileToCore :: String -> [String] -> String -> IO CoreExpr
compileToCore libdir args expression = do
  tmp <- getTemporaryDirectory
  let file = tmp </> "_interactive_.hs"
  writeFile file ("module Interactive where import GHC.Exts; it = " ++ indent 2 expression)
  -- Initialize GHC session
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      -- Set up GHC session
      dflags <- getSessionDynFlags
      logger <- getLogger
      (dflags, rest_args, err_messages) <- parseDynamicFlags logger dflags (map (L noSrcSpan) args)
      when (not (null rest_args)) $ liftIO $ putStrLn ("Unhandled args: " ++ show rest_args) >> exitFailure
      when (not (null err_messages)) $ liftIO $ pprPrint err_messages >> exitFailure

      setSessionDynFlags $
        flip gopt_unset Opt_FullLaziness $
        flip gopt_unset Opt_WorkerWrapper $
        flip gopt_unset Opt_LocalFloatOutTopLevel $
        flip gopt_unset Opt_IgnoreInterfacePragmas $ -- This enables cross-module inlining
        updOptLevel 1 $ -- if you want to compile with -O1 opts, make sure to unset -ffull-laziness and -fworker-wrapper above in addition to -flocal-float-out-top-level
        flip xopt_set LangExt.MagicHash $
        dflags
      mod_guts <- compileToCoreSimplified file
      let binds = cm_binds mod_guts
      let Just (NonRec _ e) = find (\b -> case b of NonRec x e -> getOccString x == "it"; _ -> False) binds
      return e

-- Main function to handle command-line arguments
main :: IO ()
main = do
  args <- getArgs
  tmp <- getTemporaryDirectory
  let settings = defaultSettings { historyFile = Just (tmp </> ".ghdi.hist") }
  case args of
    (libdir:rest) -> runInputT settings (loop libdir rest)
    _             -> putStrLn "Usage: `ghdi <libdir>`, for example `ghdi $(ghc --print-libdir)`"

loop :: FilePath -> [String] -> InputT IO ()
loop libdir args = do
  minput <- getInputLine "prompt> "
  case minput of
    Nothing      -> return ()
    Just ":quit" -> return ()
    Just input   -> do
      e <- liftIO $ compileToCore libdir args input
      outputStrLn (showSDocUnsafe (hang (text "Above expression as (optimised) Core:") 2 (ppr e)))
      outputStrLn "Trace of denotational interpreter:"
      outputStrLn (showSDocOneLine defaultSDocContext (hang empty 2 (ppr (evalByNeed e))))
      loop libdir args
