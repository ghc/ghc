module GHC.Tc.Gen.App where

import GHC.Hs (GhcRn, GhcTc, LHsExpr)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.TcType (TcRhoType, TcSigmaType)

data RuleAppPrefix
  = CheckRulePrefix (TcM (TcSigmaType, RuleAppPrefix))
  | FinishRuleApp (TcM (LHsExpr GhcTc, TcRhoType))

tcRuleAppPrefix :: LHsExpr GhcRn -> TcM (TcSigmaType, RuleAppPrefix)
