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


dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
  { dcfgCheckIgnore = (`elem` ["haddock.sty", "main.tex"]) . takeFileName
  -- Just a discrepancy in output order
  , dcfgCheckIgnoreOneShot = (`elem` ["ConstructorArgs.tex"]) . takeFileName
  , dcfgCheckIgnoreNoCompilation = (`elem` ["ConstructorArgs.tex"]) . takeFileName
  }


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++ ["--latex"]
        }
