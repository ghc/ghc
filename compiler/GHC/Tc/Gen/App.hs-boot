module GHC.Tc.Gen.App where

import GHC.Hs (GhcRn, GhcTc, LHsExpr)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.TcType (TcRhoType, TcSigmaType)

tcRuleAppPrefix :: LHsExpr GhcRn -> TcM (TcSigmaType, TcM (LHsExpr GhcTc, TcRhoType))
