{-# LANGUAGE CPP #-}


import Data.Char
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
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    { dcfgCheckIgnore = checkIgnore
    }


main :: IO ()
main = runAndCheck =<< parseArgs checkConfig dirConfig =<< getArgs


checkIgnore :: FilePath -> Bool
checkIgnore file
    | and . map ($ file) $ [isHtmlFile, isSourceFile, isModuleFile] = False
  where
    isHtmlFile = (== ".html") . takeExtension
    isSourceFile = (== "src") . takeDirectory
    isModuleFile = isUpper . head . takeBaseName
checkIgnore _ = True
