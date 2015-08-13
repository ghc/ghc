{-# LANGUAGE CPP #-}


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
dirConfig = defaultDirConfig $ takeDirectory __FILE__


main :: IO ()
main = runAndCheck =<< parseArgs checkConfig dirConfig =<< getArgs


stripIfRequired :: String -> Xhtml -> Xhtml
stripIfRequired mdl =
    stripLinks' . stripFooter
  where
    stripLinks'
        | mdl `elem` preserveLinksModules = id
        | otherwise = stripFooter


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]
