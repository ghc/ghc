module Playground
  ( myMain,
  )
where

import Control.Monad
import Data.Coerce
import Data.IORef
import GHC
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Utils.Exception
import GHC.Wasm.Prim

newtype JSFunction t = JSFunction JSVal

type ExportedMainFunction = JSString -> JSString -> IO ()

-- main entry point of playground001, returns a js async function that
-- takes ghc args and Main.hs content, interprets Main.hs and runs
-- Main.main.
myMain :: JSString -> IO (JSFunction ExportedMainFunction)
myMain js_libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    libdir <- evaluate $ fromJSString js_libdir
    freeJSVal $ coerce js_libdir
    -- we don't use runGhc since we want to share a session to be
    -- reused.
    session <- Session <$> newIORef undefined
    -- save a fresh default dflags, otherwise user input ghc args are
    -- not properly reset.
    dflags0 <- flip reflectGhc session $ do
      initGhcMonad (Just libdir)
      dflags0 <- getSessionDynFlags
      setSessionDynFlags $
        dflags0
          { ghcMode = CompManager,
            backend = bytecodeBackend,
            ghcLink = LinkInMemory,
            verbosity = 1
          }
      getSessionDynFlags
    -- this is always run in a forked thread. which is fine as long as
    -- the sesssion is not reused concurrently, but it's up to the
    -- caller in js to ensure that. we simply disable the run button
    -- until each run completes in the playground ui logic.
    toMainFunc $ \js_args js_src ->
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        args <- evaluate $ words $ fromJSString js_args
        freeJSVal $ coerce js_args
        writeFile f $ fromJSString js_src
        freeJSVal $ coerce js_src
        -- it's fine to call withCleanupSession since it just cleans up
        -- tmpfs for now. in the future if it does more cleanup that
        -- makes the session state invalid for reuse, just remove it;
        -- everything will be cleaned up anyway when the browser tab is
        -- closed
        flip reflectGhc session $ withCleanupSession $ do
          setSessionDynFlags dflags0
          logger0 <- getLogger
          (dflags1, _, dynamicFlagWarnings) <-
            parseDynamicFlags logger0 dflags0 $ map noLoc args
          setSessionDynFlags dflags1
          logger1 <- getLogger
          liftIO
            $ printOrThrowDiagnostics
              logger1
              (initPrintConfig dflags1)
              (initDiagOpts dflags1)
            $ GhcDriverMessage
              <$> dynamicFlagWarnings
          setTargets =<< (: []) <$> guessTarget f Nothing Nothing
          r <- load LoadAllTargets
          when (failed r) $ fail "load returned Failed"
          setContext [IIDecl $ simpleImportDecl $ mkModuleName "Main"]
          fhv <- compileExprRemote "Main.main"
          hsc_env <- getSession
          liftIO $ evalIO (hscInterp hsc_env) fhv
  where
    f = "/tmp/Main.hs"

foreign import javascript "wrapper"
  toMainFunc ::
    ExportedMainFunction ->
    IO (JSFunction ExportedMainFunction)

foreign export javascript "myMain"
  myMain ::
    JSString ->
    IO
      (JSFunction ExportedMainFunction)
