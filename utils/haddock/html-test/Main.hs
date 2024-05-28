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
    , dcfgCheckIgnoreOneShot = (`elem` ignoredOneShotTests) . takeBaseName
    }


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++ ["--pretty-html", "--html"]
#ifdef mingw32_HOST_OS
        , cfgSkipOneShot = False -- Current test setup makes the argument list too long on Windows
#else
        , cfgSkipOneShot = False
#endif
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
preserveLinksModules = ["Bug253.html", "NamespacedIdentifiers.html"]

ingoredTests :: [String]
ingoredTests =
  [
    -- Currently some declarations are exported twice
    -- we need a reliable way to deduplicate here.
    -- Happens since PR #688.
    "B"
  ]

ignoredOneShotTests :: [String]
ignoredOneShotTests =
  [
    -- Class instances don't travel up the dependency chain in one-shot mode
    "Bug1004"
  , "OrphanInstancesClass"
  , "OrphanInstancesType"
  , "TypeFamilies2"
    -- Warnings are not stored in .haddock files https://github.com/haskell/haddock/issues/223
  , "DeprecatedReExport"
  , "HiddenInstancesB"
  ]

checkIgnore :: FilePath -> Bool
checkIgnore file | takeBaseName file `elem` ingoredTests = True
checkIgnore file@(c:_) | takeExtension file == ".html" && isUpper c = False
checkIgnore _ = True
