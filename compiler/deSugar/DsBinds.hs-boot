module DsBinds where
import DsMonad     ( DsM )
import CoreSyn     ( CoreExpr )
import TcEvidence (HsWrapper)

dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
