module GHC.Driver.Ppr where

import GHC.Prelude
import GHC.Stack
import {-# SOURCE #-} GHC.Driver.Session
import {-# SOURCE #-} GHC.Utils.Outputable

showSDoc :: DynFlags -> SDoc -> String
warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc -> a -> a
