module GHC.Tc.Gen.Match where
import GHC.Hs           ( GRHSs, MatchGroup, LHsExpr )
import GHC.Tc.Types.Evidence  ( HsWrapper )
import GHC.Types.Name   ( Name )
import GHC.Tc.Utils.TcType( ExpSigmaType, ExpRhoType )
import GHC.Tc.Types     ( TcM )
import GHC.Hs.Extension ( GhcRn, GhcTc )
import GHC.Parser.Annotation ( LocatedN, SrcSpanAnnL, SrcSpanAnnA )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> ExpRhoType
              -> TcM (GRHSs GhcTc (LHsExpr GhcTc))

tcMatchesFun :: LocatedN Name
             -> MatchGroup SrcSpanAnnL SrcSpanAnnA GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup SrcSpanAnnL SrcSpanAnnA GhcTc (LHsExpr GhcTc))
