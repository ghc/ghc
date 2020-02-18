module Main where

import GHC.Driver.Session
import GHC
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags
      `gopt_unset` Opt_ImplicitImportQualified
      `xopt_unset` LangExt.ImplicitPrelude

    forM_ exprs $ \expr ->
      handleSourceError printException $ do
        dyn <- dynCompileExpr expr
        liftIO $ print dyn
  where
  exprs =
    [ ""
    , "(),()"
    , "()"
    , "\"test\""
    , unlines [ "[()]"
              , " :: [()]"
              ]
    ]
