module GHC.Tc.Gen.App where

import GHC.Hs ( HsExpr )
import GHC.Tc.Types  ( TcM )
import GHC.Tc.Utils.TcType ( TcSigmaType )
import GHC.Hs.Extension ( GhcRn, GhcTc )

tcInferSigmaX :: HsExpr GhcRn -> TcM (HsExpr GhcTc, TcSigmaType)