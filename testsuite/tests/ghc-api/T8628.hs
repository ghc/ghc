module Main where

import System.IO
import DynFlags
import GHC
import Exception
import Module
import FastString
import MonadUtils
import Outputable
import Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import PrelNames

main :: IO()
main
  = do  [libdir] <- getArgs
        ok <- runGhc (Just libdir) $ do
          dflags <- getSessionDynFlags
          setSessionDynFlags dflags
          liftIO (setUnsafeGlobalDynFlags dflags)

          setContext [ IIDecl (simpleImportDecl pRELUDE_NAME)
                     , IIDecl (simpleImportDecl (mkModuleNameFS (fsLit "System.IO")))]
          runDecls "data X = Y ()"
          runStmt "print True" RunToCompletion
          gtry $ runStmt "print (Y ())" RunToCompletion :: GhcMonad m => m (Either SomeException RunResult)
          runDecls "data X = Y () deriving Show"
          _ <- dynCompileExpr "'x'"
          runStmt "print (Y ())" RunToCompletion
          runStmt "System.IO.hFlush System.IO.stdout" RunToCompletion
        print "done"
