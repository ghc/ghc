module Main where

import GHC
import GhcMonad
import Outputable
import System.IO
import System.Environment( getArgs )

main
 = do { [libdir] <- getArgs
      ; runGhc (Just libdir) $ do
           flags <- getSessionDynFlags
           setSessionDynFlags (flags{ hscTarget = HscInterpreted, ghcLink = LinkInMemory})
           target <- guessTarget "T8639_api_a.hs" Nothing
           setTargets [target]
           load LoadAllTargets
           imps <- mapM parseImportDecl ["import Prelude", "import System.IO", "import T8639_api_a"]
           setContext (map IIDecl imps)

           -- With the next line, you get an "Not in scope" exception.
           -- If you comment out this runStmt, it runs without error and prints the  type.
           runStmt "putStrLn (show 3)" RunToCompletion
           runStmt "hFlush stdout" RunToCompletion

           ty <- exprType "T8639_api_a.it"
           liftIO (putStrLn (showPpr flags ty))
       ; hFlush stdout }
