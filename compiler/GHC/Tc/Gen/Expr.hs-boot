module GHC.Tc.Gen.Expr where
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn, SyntaxExprTc
                           , LHsSigWcType )
import GHC.Tc.Utils.TcType ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Hs.Extension    ( GhcRn, GhcTc, NoGhcTc )

tcCheckPolyExpr, tcCheckPolyExprNC ::
          LHsExpr GhcRn
       -> TcSigmaType
       -> TcM (LHsExpr GhcTc)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr GhcRn
       -> ExpRhoType
       -> TcM (LHsExpr GhcTc)
tcCheckMonoExpr, tcCheckMonoExprNC ::
          LHsExpr GhcRn
       -> TcRhoType
       -> TcM (LHsExpr GhcTc)

tcExpr :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

tcExprWithSig :: LHsExpr GhcRn -> LHsSigWcType (NoGhcTc GhcRn)
              -> TcM (HsExpr GhcTc, TcSigmaType)

tcInferSigma :: LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcSigmaType)

tcInferRho, tcInferRhoNC ::
          LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)

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
