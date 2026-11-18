module GHC.HsToCore.Binds where
import GHC.HsToCore.Monad ( DsM )
import GHC.Core           ( CoreExpr )
import GHC.Data.OrdList   ( OrdList )
import GHC.Hs             ( GhcTc, LHsBinds )
import GHC.Types.Var      ( Id )
import GHC.Tc.Types.Evidence    (HsWrapper)

dsHsWrapper :: HsWrapper -> ((CoreExpr -> CoreExpr) -> DsM a) -> DsM a

dsTopLHsBinds :: LHsBinds GhcTc -> DsM (OrdList (Id, CoreExpr))
