module GHC.Driver.Config.CmmToLlvm
where

import GHC.Prelude ( IO )

import {-# SOURCE #-} GHC.CmmToLlvm.Config ( LlvmCgConfig )
import GHC.Utils.Logger
import {-# SOURCE #-} GHC.Driver.Session ( DynFlags )

initLlvmCgConfig :: Logger -> DynFlags -> IO LlvmCgConfig
