module Main (main) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC
import GHC.Core (CoreExpr)
import GHC.Driver.Backend (platformDefaultBackend)
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Env.Types (HscEnv (hsc_hooks))
import GHC.Driver.Hooks (Hooks (hscCompileCoreExprHook))
import GHC.Driver.Main (hscCompileCoreExpr')
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Session (gopt_set, gopt_unset)
import GHC.Linker.Types (LinkableUsage, PkgsLoaded, linkableModule, linkableParts)
import GHC.Runtime.Interpreter (evalString)
import GHC.Runtime.Interpreter.Types (Interp (interpInstance), InterpInstance (ExternalInterp))
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Unit.Module.Deps (LinkablePartUsage (..))
import Numeric.Natural (Natural)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (replaceExtension)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir, source, interpreter, code, result]
            | interpreter `elem` ["internal", "external"]
            , code `elem` ["bytecode", "object"]
            , Just expected <- readMaybe result ->
                runGhc (Just libdir) (probe source interpreter code expected)
        _ -> die "usage: direct-eval LIBDIR PRODUCER internal|external bytecode|object NATURAL"

-- | Evaluate a closed call and check the interpreter and producer linkable.
probe :: FilePath -> String -> String -> Natural -> Ghc ()
probe source interpreter code expected = do
    flags <- getSessionDynFlags
    let useObject = code == "object"
        producerBackend = if useObject then platformDefaultBackend (targetPlatform flags) else bytecodeBackend
        interactive = flags{backend = producerBackend, ghcLink = LinkInMemory, verbosity = 0}
        configure = if interpreter == "external" then gopt_set else gopt_unset
    void (setSessionDynFlags (configure interactive Opt_ExternalInterpreter))
    target <- guessTarget source Nothing Nothing
    setTargets [target{targetAllowObjCode = useObject}]
    loaded <- load LoadAllTargets
    unless (succeeded loaded) (fail "could not load ClosedLengthProducer")
    setContext (map (IIDecl . simpleImportDecl . mkModuleName) ["Prelude", "Numeric.Natural", "ClosedLengthProducer"])
    interp <- hscInterp <$> getSession
    let actualInterpreter = case interpInstance interp of
            ExternalInterp{} -> "external"
            _ -> "internal"
    unless (actualInterpreter == interpreter) $
        fail ("expected " ++ interpreter ++ " interpreter, got " ++ actualInterpreter)

    object <- liftIO (makeAbsolute (replaceExtension source "o"))
    sawDependency <- liftIO (newIORef False)
    let compile env location expression = do
            result <- compileWithDependency useObject object env location expression
            writeIORef sawDependency True
            pure result
    modifySession $ \env ->
        env{hsc_hooks = (hsc_hooks env){hscCompileCoreExprHook = Just compile}}

    -- The type annotation gives evalString its expected IO String value.
    closure <-
        compileExprRemote
            "(pure (show (ClosedLengthProducer.length [1, 2, 3] :: Natural)) :: IO String)"
    checked <- liftIO (readIORef sawDependency)
    unless checked (fail "closed call bypassed the compiler dependency check")
    canonical <- liftIO (evalString interp closure)
    unless (canonical == show expected) $
        fail ("expected canonical Natural " ++ show expected ++ ", got " ++ show canonical)
    liftIO $ putStrLn (interpreter ++ " " ++ code ++ ": Natural " ++ canonical)

-- | Verify the producer dependency returned by the actual compiler path.
compileWithDependency ::
    Bool ->
    FilePath ->
    HscEnv ->
    SrcSpan ->
    CoreExpr ->
    IO (ForeignHValue, [LinkableUsage], PkgsLoaded)
compileWithDependency useObject object env location expression = do
    result@(_, linkables, _) <- hscCompileCoreExpr' env location expression
    let producerParts =
            [ part
            | linkable <- linkables
            , moduleNameString (moduleName (linkableModule linkable)) == "ClosedLengthProducer"
            , part <- toList (linkableParts linkable)
            ]
    case producerParts of
        [FileLinkablePartUsage{flu_file = file}] | useObject -> do
            actual <- makeAbsolute file
            unless (actual == object) $
                fail ("expected producer object " ++ object ++ ", got " ++ actual)
        [ByteCodeLinkablePartUsage{}] | not useObject -> pure ()
        _ -> fail "expected exactly one matching producer linkable"
    pure result
