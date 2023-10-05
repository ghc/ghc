module GHC.Tc.Utils.TcMType where

import GHC.Tc.Types
import GHC.Types.Name
import GHC.Core.TyCo.Rep
import GHC.Tc.Types.Evidence

tcCheckUsage :: Name -> Mult -> TcM a -> TcM (a, HsWrapper)
