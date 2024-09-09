module GHC.Tc.Gen.App where

import GHC.Hs ( HsExpr )
import GHC.Tc.Types  ( TcM )
import GHC.Tc.Types.Origin  ( CtOrigin )
import GHC.Tc.Utils.TcType ( TcSigmaType )
import GHC.Tc.Utils.Unify ( DeepSubsumptionFlag )
import GHC.Hs.Extension ( GhcRn, GhcTc )


import GHC.Prelude (Bool)

tcExprSigma :: Bool -> CtOrigin -> HsExpr GhcRn -> TcM (HsExpr GhcTc, DeepSubsumptionFlag, TcSigmaType)
