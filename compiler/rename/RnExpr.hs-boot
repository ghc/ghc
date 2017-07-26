module RnExpr where
import Name
import HsSyn
import NameSet     ( FreeVars )
import TcRnTypes
import SrcLoc      ( Located )
import Outputable  ( Outputable )
import HsExtension ( GhcPs, GhcRn )

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body (GHC GhcPs)) => HsStmtContext Name
        -> (Located (body (GHC GhcPs)) -> RnM (Located (body (GHC GhcRn)), FreeVars))
        -> [LStmt GhcPs (Located (body (GHC GhcPs)))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (Located (body (GHC GhcRn)))], thing), FreeVars)
