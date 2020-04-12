module GHC.HsToCore.Binds where
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Tc.Types.Evidence    (HsWrapper)

dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
