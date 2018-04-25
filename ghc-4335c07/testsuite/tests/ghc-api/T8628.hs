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
          execStmt "print True" execOptions
          gtry $ execStmt "print (Y ())" execOptions :: GhcMonad m => m (Either SomeException ExecResult)
          runDecls "data X = Y () deriving Show"
          _ <- dynCompileExpr "'x'"
          execStmt "print (Y ())" execOptions
          execStmt "System.IO.hFlush System.IO.stdout" execOptions
        print "done"
