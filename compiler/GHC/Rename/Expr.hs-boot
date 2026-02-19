module GHC.Rename.Expr where

import GHC.Types.Name
import GHC.Hs
import GHC.Types.Name.Set ( FreeNames )
import GHC.Tc.Types
import GHC.Utils.Outputable  ( Outputable )

rnExpr :: HsExpr GhcPs
        -> RnM (HsExpr GhcRn, FreeNames)

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeNames)

type AnnoBody body
  = ( Outputable (body GhcPs)
    )

rnStmts :: --forall thing body.
           AnnoBody body => HsStmtContextRn
        -> (body GhcPs -> RnM (body GhcRn, FreeNames))
        -> [LStmt GhcPs (LocatedA (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeNames))
        -> RnM (([LStmt GhcRn (LocatedA (body GhcRn))], thing), FreeNames)
