module GHC.SysTools.Tasks where

import GHC.Prelude

import {-# SOURCE #-} GHC.Driver.Session
import GHC.Utils.Logger
import GHC.Utils.CliOption
import GHC.CmmToLlvm.LlvmVersion

runAs, runClang :: Logger -> DynFlags -> [Option] -> IO ()
figureLlvmVersion :: Logger -> DynFlags -> IO (Maybe LlvmVersion)
