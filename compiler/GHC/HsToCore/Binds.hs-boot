module GHC.HsToCore.Binds where
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Tc.Types.Evidence    (HsWrapper)
import GHC.Stack (HasCallStack)

dsHsWrapper :: HasCallStack => HsWrapper -> ((CoreExpr -> CoreExpr) -> DsM a) -> DsM a
