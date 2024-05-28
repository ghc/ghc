{-# LANGUAGE CPP #-}


import Data.Char
import Data.List (isPrefixOf)
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
    strip _ = fixPaths . stripAnchors' . stripLinks' . stripIds' . stripFooter

    stripLinks' = stripLinksWhen $ \href -> "#local-" `isPrefixOf` href
    stripAnchors' = stripAnchorsWhen $ \name -> "local-" `isPrefixOf` name
    stripIds' = stripIdsWhen $ \name -> "local-" `isPrefixOf` name
    -- One-shot hyperlinked source links to other modules as if they are in another package
    fixPaths = fixAttrValueWhen "href" (drop 7) ("../src/" `isPrefixOf`)


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
    isModuleFile f
      | c : _ <- takeBaseName f
      , isUpper c
      = True
      | otherwise
      = False
checkIgnore _ = True
