{-# LANGUAGE CPP #-}


import Data.Char

import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml


checkConfig :: CheckConfig Xml
checkConfig = CheckConfig
    { ccfgRead = \mdl input -> stripIfRequired mdl <$> parseXml input
    , ccfgDump = dumpXml
    , ccfgEqual = (==)
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


checkIgnore :: FilePath -> Bool
checkIgnore file@(c:_) | takeExtension file == ".html" && isUpper c = False
checkIgnore _ = True
