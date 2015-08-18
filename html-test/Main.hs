{-# LANGUAGE CPP #-}


import Data.Char

import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml


checkConfig :: CheckConfig Xhtml
checkConfig = CheckConfig
    { ccfgRead = \mdl input -> stripIfRequired mdl <$> parseXhtml input
    , ccfgDump = dumpXhtml
    , ccfgEqual = (==)
    }


dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    { dcfgCheckIgnore = checkIgnore
    }


main :: IO ()
main = runAndCheck =<< parseArgs checkConfig dirConfig =<< getArgs


stripIfRequired :: String -> Xhtml -> Xhtml
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
