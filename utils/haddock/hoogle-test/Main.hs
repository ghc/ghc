{-# LANGUAGE CPP #-}


import Data.Function
import System.Environment
import System.FilePath

import Test.Haddock
import Test.Haddock.Utils


checkConfig :: CheckConfig String
checkConfig = CheckConfig
    { ccfgRead = Just
    , ccfgClean = \_ -> id
    , ccfgDump = id
    , ccfgEqual = (==) `on` crlfToLf
    }

multiModuleTests :: [String]
multiModuleTests = ["modules", "type-sigs"]

dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    -- Multi-module hoogle tests don't make sense for one-shot mode
    { dcfgCheckIgnoreOneShot = (`elem` (fmap (</> "test.txt") multiModuleTests))
    }


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++
            [ "--package-name=test"
            , "--package-version=0.0.0"
            , "--hoogle"
            ]
        }
