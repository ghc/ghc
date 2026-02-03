module GHC.Tc.Gen.App where

import GHC.Hs ( LHsExpr )
import GHC.Tc.Types  ( TcM )
import GHC.Tc.Types.Origin  ( CtOrigin )
import GHC.Tc.Utils.TcType ( TcSigmaType )
import GHC.Tc.Utils.Unify ( DeepSubsumptionFlag )
import GHC.Hs.Extension ( GhcRn, GhcTc )


import GHC.Prelude (Bool)

tcLExprSigma :: Bool -> CtOrigin -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, DeepSubsumptionFlag, TcSigmaType)
