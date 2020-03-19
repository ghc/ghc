module GHC.Tc.Gen.Match where
import GHC.Hs           ( GRHSs, MatchGroup, LHsExpr )
import GHC.Tc.Types.Evidence  ( HsWrapper )
import GHC.Types.Name   ( Name )
import GHC.Tc.Utils.TcType( ExpSigmaType, TcRhoType )
import GHC.Tc.Types     ( TcM )
import GHC.Types.SrcLoc ( Located )
import GHC.Hs.Extension ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
