module GHC.Tc.Gen.Match where
import GHC.Hs           ( GRHSs, MatchGroup, LHsExpr )
import GHC.Tc.Types.Evidence  ( HsWrapper )
import GHC.Tc.Utils.TcType( ExpSigmaType, ExpRhoType )
import GHC.Tc.Types     ( TcM )
import GHC.Hs.Extension ( GhcRn, GhcTc )
import GHC.Parser.Annotation ( LocatedN )
import GHC.Types.Id (Id)

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> ExpRhoType
              -> TcM (GRHSs GhcTc (LHsExpr GhcTc))

tcMatchesFun :: LocatedN Id
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
