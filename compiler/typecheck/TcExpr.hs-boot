module TcExpr where
import HsSyn    ( HsExpr, LHsExpr )
import Name     ( Name )
import TcType   ( TcType, TcRhoType, TcSigmaType )
import TcRnTypes( TcM, TcId, CtOrigin )

tcPolyExpr ::
          LHsExpr Name
       -> TcSigmaType
       -> TcM (LHsExpr TcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr Name
       -> TcRhoType
       -> TcM (LHsExpr TcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcSigmaType)

tcInferRho, tcInferRhoNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> HsExpr Name
           -> TcType
           -> TcM (HsExpr TcId)

tcCheckId :: Name -> TcRhoType -> TcM (HsExpr TcId)
