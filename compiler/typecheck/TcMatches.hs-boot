module TcMatches where
import GHC.Hs   ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import Name     ( Name )
import TcType   ( ExpSigmaType, TcRhoType )
import TcRnTypes( TcM )
import GHC.Hs.Extension ( GhcRn, GhcTcId, LocatedA )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: LocatedA Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
