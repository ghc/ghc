{-# LANGUAGE CPP #-}


import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml

import qualified Text.XML.Light as Xml


checkConfig :: CheckConfig Xml.Element
checkConfig = CheckConfig
    { ccfgRead = \_ input -> strip <$> Xml.parseXMLDoc input
    , ccfgDump = Xml.ppElement
    , ccfgEqual = (==)
    }


dirConfig :: DirConfig
dirConfig = defaultDirConfig $ takeDirectory __FILE__


main :: IO ()
main = runAndCheck =<< parseArgs checkConfig dirConfig =<< getArgs


-- *** OLD TEST RUNNER UTILITY FUNCTIONS ***
-- These are considered bad and should be replaced as soon as possible.


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]
