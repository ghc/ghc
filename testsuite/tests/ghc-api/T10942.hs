module Main where

import GHC.Driver.Session
import GHC.Driver.Config.Parser
import GHC

import Control.Monad.IO.Class (liftIO)
import System.Environment
import GHC.Parser.Header
import GHC.Types.SourceError (initSourceErrorContext)
import GHC.Utils.Outputable
import GHC.Data.StringBuffer

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags `gopt_set` Opt_KeepRawTokenStream
                         `gopt_set` Opt_Haddock
        filename = "T10942_A.hs"
        parser_opts = initParserOpts dflags'
        sec = initSourceErrorContext dflags'
    setSessionDynFlags dflags'
    stringBuffer <- liftIO $ hGetStringBuffer filename
    liftIO $ print (map unLoc (snd $ getOptions parser_opts sec (supportedLanguagePragmas dflags) stringBuffer filename))
