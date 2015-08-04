module TcMatches where
import HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import Name     ( Name )
import TcType   ( TcRhoType )
import TcRnTypes( TcM, TcId, CtOrigin )
--import SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId), CtOrigin)

tcMatchesFun :: Name -> Bool
             -> MatchGroup Name (LHsExpr Name)
             -> TcRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId), CtOrigin)
