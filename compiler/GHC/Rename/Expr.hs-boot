{-# LANGUAGE ConstraintKinds #-}
module GHC.Rename.Expr where

import GHC.Types.Name
import GHC.Hs
import GHC.Types.Name.Set ( FreeVars )
import GHC.Tc.Types
import GHC.Utils.Outputable  ( Outputable )
import GHC.Stack

rnExpr :: HasCallStack => HsExpr GhcPs
        -> RnM (HsExpr GhcRn, FreeVars)

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

type AnnoBody body
  = ( Outputable (body GhcPs)
    )

rnStmts :: --forall thing body.
           AnnoBody body => HsStmtContextRn
        -> (body GhcPs -> RnM (body GhcRn, FreeVars))
        -> [LStmt GhcPs (LocatedA (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (LocatedA (body GhcRn))], thing), FreeVars)
