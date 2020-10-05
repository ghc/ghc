module Main where

import System.Process
import System.Directory
import System.Environment

main :: IO ()
main = do
    ghcPath <- getEnv "TEST_HC"
    out <- readProcess ghcPath ["--info"] ""
    let settings :: [(String, String)]
        settings = read out
    let Just hcPkgPath = lookup "hc-pkg path" settings
    _versionOut <- readProcess hcPkgPath ["--version"] ""
    return ()
