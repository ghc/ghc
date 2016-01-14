module TcExpr where
import HsSyn    ( HsExpr, LHsExpr, SyntaxExpr )
import Name     ( Name )
import TcType   ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import TcRnTypes( TcM, TcId, CtOrigin )

tcPolyExpr ::
          LHsExpr Name
       -> TcSigmaType
       -> TcM (LHsExpr TcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr Name
       -> ExpRhoType
       -> TcM (LHsExpr TcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcSigmaType)

tcInferRho ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExpr Name
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExpr TcId)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExpr Name
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExpr TcId)


tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr TcId)
