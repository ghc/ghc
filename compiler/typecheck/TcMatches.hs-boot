module TcMatches where
import HsSyn    ( GRHSs, MatchGroup, LHsExpr, LMatch )
import BasicTypes ( Origin )
import TcEvidence( HsWrapper )
import Name     ( Name )
import TcType   ( TcRhoType )
import TcRnTypes( TcM, TcId )
--import SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Name -> Bool
             -> MatchGroup Name (LHsExpr Name)
             -> TcRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))

tcMatchFun :: Name -> Bool
           -> LMatch Name (LHsExpr Name)
           -> Origin       -- from the MatchGroup
           -> TcRhoType     -- Expected type of function (just a TauTv)
           -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
