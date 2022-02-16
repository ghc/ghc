module GHC.CmmToAsm
where

import GHC.Prelude

import GHC.IO.Handle.Types
import GHC.CmmToAsm.Config
import GHC.Types.Unique.Supply
import GHC.Utils.Logger
import GHC.Unit.Module.Location
import GHC.Data.Stream
import GHC.Cmm

nativeCodeGen :: forall a . Logger -> NCGConfig -> ModLocation -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup a
              -> IO a
