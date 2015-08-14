{-# LANGUAGE CPP #-}


import Data.List

import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml


checkConfig :: CheckConfig Xhtml
checkConfig = CheckConfig
    { ccfgRead = \_ input -> strip <$> parseXhtml input
    , ccfgDump = dumpXhtml
    , ccfgEqual = (==)
    }
  where
    strip = stripAnchors' . stripLinks' . stripFooter
    stripLinks' = stripLinksWhen $ \href -> "#local-" `isPrefixOf` href
    stripAnchors' = stripAnchorsWhen $ \name -> "local-" `isPrefixOf` name


dirConfig :: DirConfig
dirConfig = defaultDirConfig $ takeDirectory __FILE__


main :: IO ()
main = runAndCheck =<< parseArgs checkConfig dirConfig =<< getArgs
