-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : internal
--
module Main where

import WithCli (HasArguments(..), withCli)
import Parser.Text (genModules)
import GHC.Generics (Generic)

data CLIOptions =
    CLIOptions
        { input :: String
        , output :: String
        , core_prop :: [String]
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts = genModules (input opts) (output opts) (core_prop opts)

main :: IO ()
main = withCli cliClient
