module TcExpr where
import HsSyn    ( HsExpr, LHsExpr )
import Name     ( Name )
import TcType   ( TcType, TcSigmaType )
import TcRnTypes( TcM, TcId, CtOrigin )

tcPolyExpr, tcPolyExprNC ::
          LHsExpr Name
       -> TcSigmaType
       -> TcM (LHsExpr TcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcSigmaType)

tcSyntaxOp :: CtOrigin
           -> HsExpr Name
           -> TcType
           -> TcM (HsExpr TcId)

tcCheckId :: Name -> TcSigmaType -> TcM (HsExpr TcId)
