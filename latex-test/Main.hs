{-# LANGUAGE CPP #-}


import System.Environment
import System.FilePath

import Test.Haddock


checkConfig :: CheckConfig String
checkConfig = CheckConfig
    { ccfgRead = Just
    , ccfgClean = \_ -> id
    , ccfgDump = id
    , ccfgEqual = (==)
    }


dirConfig :: DirConfig
dirConfig = defaultDirConfig $ takeDirectory __FILE__


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++ ["--latex"]
        }
