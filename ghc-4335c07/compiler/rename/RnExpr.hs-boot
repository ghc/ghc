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
           Outputable (body GhcPs) => HsStmtContext Name
        -> (Located (body GhcPs) -> RnM (Located (body GhcRn), FreeVars))
        -> [LStmt GhcPs (Located (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (Located (body GhcRn))], thing), FreeVars)
