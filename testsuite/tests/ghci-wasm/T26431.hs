import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe
import GHC
import GHC.Plugins
import GHC.Runtime.Interpreter
import System.Environment.Blank

main :: IO ()
main = do
  [libdir] <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $
      do
        dflags0 <- getSessionDynFlags
        let dflags1 =
              dflags0
                { ghcMode = CompManager,
                  backend = interpreterBackend,
                  ghcLink = LinkInMemory
                }
        logger <- getLogger
        (dflags2, _, _) <-
          parseDynamicFlags logger dflags1 $
            map noLoc ["-package", "ghc"]
        _ <- setSessionDynFlags dflags2
        addTarget =<< guessTarget "hello.hs" Nothing Nothing
        _ <- load LoadAllTargets
        setContext
          [ IIDecl $ simpleImportDecl $ mkModuleName "Prelude",
            IIDecl $ simpleImportDecl $ mkModuleName "Main"
          ]
        hsc_env <- getSession
        fhv <- compileExprRemote "main"
        liftIO $ evalIO (fromJust $ hsc_interp hsc_env) fhv
