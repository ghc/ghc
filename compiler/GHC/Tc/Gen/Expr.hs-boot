module GHC.Tc.Gen.Expr where
import GHC.Types.Name
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn, SyntaxExprTc )
import GHC.Tc.Utils.TcType ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Hs.Extension    ( GhcRn, GhcTcId )

tcCheckExpr :: LHsExpr GhcRn -> TcSigmaType -> TcM (LHsExpr GhcTcId)

tcLExpr, tcLExprNC
       :: LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTcId)
tcExpr :: HsExpr GhcRn  -> ExpRhoType -> TcM (HsExpr GhcTcId)

tcInferRho, tcInferRhoNC
  :: LHsExpr GhcRn-> TcM (LHsExpr GhcTcId, TcRhoType)

tcInferSigma :: LHsExpr GhcRn-> TcM (LHsExpr GhcTcId, TcSigmaType)

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
