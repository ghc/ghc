module GHC.CmmToLlvm
where

import GHC.Prelude

import GHC.IO.Handle.Types
import {-# SOURCE #-} GHC.CmmToLlvm.Config (LlvmCgConfig)
import GHC.Utils.Logger
import GHC.Data.Stream
import GHC.Cmm

llvmCodeGen :: Logger -> LlvmCgConfig -> Handle
               -> Stream IO RawCmmGroup a
               -> IO a
