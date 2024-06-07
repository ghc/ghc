-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : The GHC Developers <ghc-devs@haskell.org>"
-- Stability   : internal
--
module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import GHC.Generics (Generic)
import WithCli (HasArguments(..), withCli)

import UCD2Haskell.ModuleGenerators (genModules)

data CLIOptions =
    CLIOptions
        { input :: String
        , output :: String
        , core_prop :: [String]
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts = genModules
    opts.input
    opts.output
    (BS.toShort . B8.pack <$> opts.core_prop)

main :: IO ()
main = withCli cliClient
