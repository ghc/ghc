module TcExpr where
import Name
import GHC.Hs    ( HsExpr, LHsExpr, SyntaxExprRn, SyntaxExprTc )
import TcType   ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import TcRnTypes( TcM )
import TcOrigin ( CtOrigin )
import GHC.Hs.Extension ( GhcRn, GhcTcId )

tcCheckPolyExpr ::
          LHsExpr GhcRn
       -> TcSigmaType
       -> TcM (LHsExpr GhcTcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr GhcRn
       -> ExpRhoType
       -> TcM (LHsExpr GhcTcId)
tcCheckMonoExpr, tcCheckMonoExprNC ::
          LHsExpr GhcRn
       -> TcRhoType
       -> TcM (LHsExpr GhcTcId)

tcInferSigma ::
          LHsExpr GhcRn
       -> TcM (LHsExpr GhcTcId, TcSigmaType)

tcInferRho, tcInferRhoNC ::
          LHsExpr GhcRn
       -> TcM (LHsExpr GhcTcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExprRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExprTc)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExprRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExprTc)


tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTcId)
