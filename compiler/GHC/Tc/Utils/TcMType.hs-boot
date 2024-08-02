module GHC.Tc.Utils.TcMType where

import GHC.Tc.Types
import GHC.Types.Name
import GHC.Core.TyCo.Rep

tcCheckUsage :: Name -> Mult -> TcM a -> TcM a
