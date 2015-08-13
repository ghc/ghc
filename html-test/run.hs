{-# LANGUAGE CPP #-}


import System.Environment
import System.FilePath

import Test.Haddock


baseDir :: FilePath
baseDir = takeDirectory __FILE__


main :: IO ()
main = do
    let dcfg = defaultDirConfig baseDir
    cfg <- uncurry (loadConfig dcfg) =<< checkOpt =<< getArgs
    runHaddock cfg
    checkFiles cfg


-- *** OLD TEST RUNNER UTILITY FUNCTIONS ***
-- These are considered bad and should be replaced as soon as possible.


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]
