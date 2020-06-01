module GHC.Tc.Gen.Expr where
import GHC.Types.Name
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn, SyntaxExprTc )
import GHC.Tc.Utils.TcType ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Hs.Extension    ( GhcRn, GhcTc )

tcCheckExpr :: LHsExpr GhcRn -> TcSigmaType -> TcM (LHsExpr GhcTc)

tcLExpr, tcLExprNC
       :: LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTc)
tcExpr :: HsExpr GhcRn  -> ExpRhoType -> TcM (HsExpr GhcTc)

tcInferRho, tcInferRhoNC
  :: LHsExpr GhcRn-> TcM (LHsExpr GhcTc, TcRhoType)

tcInferSigma :: LHsExpr GhcRn-> TcM (LHsExpr GhcTc, TcSigmaType)

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


tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTc)
