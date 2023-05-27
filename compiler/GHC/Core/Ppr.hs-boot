{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Core.Ppr where

import {-# SOURCE #-} GHC.Core
import {-# SOURCE #-} GHC.Types.Var (Var, Id)
import GHC.Utils.Outputable (OutputableBndr, Outputable, SDoc)

instance OutputableBndr b => Outputable (Expr b)

instance OutputableBndr Var

pprId :: Id -> SDoc
