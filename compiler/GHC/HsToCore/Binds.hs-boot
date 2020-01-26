module GHC.HsToCore.Binds where
import GHC.HsToCore.Monad ( DsM )
import CoreSyn            ( CoreExpr )
import TcEvidence (HsWrapper)

dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
