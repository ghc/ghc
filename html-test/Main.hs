{-# LANGUAGE CPP #-}


import Data.Char
import Data.Function (on)

import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml


checkConfig :: CheckConfig Xml
checkConfig = CheckConfig
    { ccfgRead = parseXml
    , ccfgClean = stripIfRequired
    , ccfgDump = dumpXml
    , ccfgEqual = (==) `on` dumpXml
    }


dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    { dcfgCheckIgnore = checkIgnore
    }


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++ ["--pretty-html", "--html"]
        }


stripIfRequired :: String -> Xml -> Xml
stripIfRequired mdl =
    stripLinks' . stripFooter
  where
    stripLinks'
        | mdl `elem` preserveLinksModules = id
        | otherwise = stripLinks


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]

ingoredTests :: [FilePath]
ingoredTests =
  [
    -- Currently some declarations are exported twice
    -- we need a reliable way to deduplicate here.
    -- Happens since PR #688.
    "B"

    -- ignore-exports flag broke with PR #688. We use
    -- the Avails calculated by GHC now. Probably
    -- requires a change to GHC to "ignore" a modules
    -- export list reliably.
  , "IgnoreExports"
  ]

checkIgnore :: FilePath -> Bool
checkIgnore file | takeBaseName file `elem` ingoredTests = True
checkIgnore file@(c:_) | takeExtension file == ".html" && isUpper c = False
checkIgnore _ = True
