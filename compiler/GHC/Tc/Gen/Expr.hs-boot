module GHC.Tc.Gen.Expr where
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn
                           , SyntaxExprTc )
import GHC.Tc.Utils.TcType ( TcRhoType, TcSigmaType, TcSigmaTypeFRR
                           , SyntaxOpType
                           , ExpType, ExpRhoType, ExpSigmaType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Core.Type ( Mult )
import GHC.Hs.Extension ( GhcRn, GhcTc )

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

tcPolyExpr :: HsExpr GhcRn -> ExpSigmaType -> TcM (HsExpr GhcTc)
tcExpr     :: HsExpr GhcRn -> ExpRhoType   -> TcM (HsExpr GhcTc)

tcInferRho, tcInferRhoNC ::
          LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExprRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExprTc)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExprRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a)
              -> TcM (a, SyntaxExprTc)

