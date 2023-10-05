{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Core.Ppr where

import {-# SOURCE #-} GHC.Core
import {-# SOURCE #-} GHC.Types.Var (Var)
import GHC.Utils.Outputable (OutputableBndr, Outputable)

instance OutputableBndr b => Outputable (Expr b)

instance OutputableBndr Var
