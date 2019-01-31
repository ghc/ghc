{-# LANGUAGE CPP #-}


import Data.Char
import Data.List
import Data.Function (on)

import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml


checkConfig :: CheckConfig Xml
checkConfig = CheckConfig
    { ccfgRead = parseXml
    , ccfgClean = strip
    , ccfgDump = dumpXml
    , ccfgEqual = (==) `on` dumpXml
    }
  where
    -- The whole point of the ClangCppBug is to demonstrate a situation where
    -- line numbers may vary (and test that links still work). Consequently, we
    -- strip out line numbers for this test case.
    strip f | takeBaseName f == "ClangCppBug"
            = stripAnchors' . stripLinks' . stripIds' . stripIds'' . stripFooter
            | otherwise
            = stripAnchors' . stripLinks' . stripIds' . stripFooter
    
    stripLinks' = stripLinksWhen $ \href -> "#local-" `isPrefixOf` href
    stripAnchors' = stripAnchorsWhen $ \name -> "local-" `isPrefixOf` name
    stripIds' = stripIdsWhen $ \name -> "local-" `isPrefixOf` name
    stripIds'' = stripIdsWhen $ \name -> "line-" `isPrefixOf` name


dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    { dcfgCheckIgnore = checkIgnore
    }


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++
            [ "--pretty-html"
            , "--hyperlinked-source"
            ]
        }


checkIgnore :: FilePath -> Bool
checkIgnore file
    | and . map ($ file) $ [isHtmlFile, isSourceFile, isModuleFile] = False
  where
    isHtmlFile = (== ".html") . takeExtension
    isSourceFile = (== "src") . takeDirectory
    isModuleFile = isUpper . head . takeBaseName
checkIgnore _ = True
