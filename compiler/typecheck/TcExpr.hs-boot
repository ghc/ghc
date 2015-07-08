module TcExpr where
import HsSyn    ( HsExpr, LHsExpr )
import Name     ( Name )
import TcType   ( TcType, TcRhoType )
import TcRnTypes( TcM, TcId, CtOrigin )

tcMonoExpr, tcMonoExprNC ::
          LHsExpr Name
       -> TcRhoType
       -> TcM (LHsExpr TcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcSigmaType)

tcSyntaxOp :: CtOrigin
           -> HsExpr Name
           -> TcType
           -> TcM (HsExpr TcId)

tcCheckId :: Name -> TcSigmaType -> TcM (HsExpr TcId)
