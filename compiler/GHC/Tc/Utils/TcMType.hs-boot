module GHC.Tc.Utils.TcMType where
import GHC.Types.Var.Env ( TidyEnv )
import {-# SOURCE #-} GHC.Tc.Types.Origin ( SkolemInfo )

tidySkolemInfo :: TidyEnv -> SkolemInfo -> SkolemInfo
