module TcMatches where
import HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import Name     ( Name )
import TcType   ( ExpSigmaType, TcRhoType )
import TcRnTypes( TcM )
import SrcLoc   ( Located )
import HsExtension ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
