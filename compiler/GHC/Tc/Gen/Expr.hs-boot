module GHC.Tc.Gen.Expr where
import GHC.Hs              ( HsExpr, LHsExpr, SyntaxExprRn
                           , SyntaxExprTc )
import GHC.Tc.Utils.TcType ( TcType, TcRhoType, TcSigmaType, TcSigmaTypeFRR
                           , SyntaxOpType, InferInstFlag
                           , ExpType, ExpRhoType, ExpSigmaType )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.BasicTypes( TcCompleteSig )
import GHC.Tc.Types.Origin ( CtOrigin, FixedRuntimeRepContext )
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

tcPolyLExpr    :: LHsExpr GhcRn -> ExpSigmaType -> TcM (LHsExpr GhcTc)
tcPolyLExprSig :: LHsExpr GhcRn -> TcCompleteSig -> TcM (LHsExpr GhcTc)

tcPolyExpr :: HsExpr GhcRn -> ExpSigmaType -> TcM (HsExpr GhcTc)
tcExpr     :: HsExpr GhcRn -> ExpRhoType   -> TcM (HsExpr GhcTc)

tcInferRho, tcInferRhoNC ::
          LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)
tcInferRhoFRR, tcInferRhoFRRNC ::
  FixedRuntimeRepContext -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)

tcInferExpr :: InferInstFlag -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcType)

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
