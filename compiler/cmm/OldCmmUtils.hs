-----------------------------------------------------------------------------
--
-- Old-style Cmm utilities.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module OldCmmUtils(
        CmmStmts, noStmts, oneStmt, mkStmts, plusStmts, stmtList,
        isNopStmt,

        maybeAssignTemp, loadArgsIntoTemps,

        module CmmUtils,
  ) where

#include "HsVersions.h"

import OldCmm
import CmmUtils
import OrdList
import DynFlags
import Unique

---------------------------------------------------
--
--      CmmStmts
--
---------------------------------------------------

type CmmStmts = OrdList CmmStmt

noStmts :: CmmStmts
noStmts = nilOL

oneStmt :: CmmStmt -> CmmStmts
oneStmt = unitOL

mkStmts :: [CmmStmt] -> CmmStmts
mkStmts = toOL

plusStmts :: CmmStmts -> CmmStmts -> CmmStmts
plusStmts = appOL

stmtList :: CmmStmts -> [CmmStmt]
stmtList = fromOL


---------------------------------------------------
--
--      CmmStmt
--
---------------------------------------------------

isNopStmt :: CmmStmt -> Bool
-- If isNopStmt returns True, the stmt is definitely a no-op;
-- but it might be a no-op even if isNopStmt returns False
isNopStmt CmmNop                       = True
isNopStmt (CmmAssign r e)              = cheapEqReg r e
isNopStmt (CmmStore e1 (CmmLoad e2 _)) = cheapEqExpr e1 e2
isNopStmt _                            = False

cheapEqExpr :: CmmExpr -> CmmExpr -> Bool
cheapEqExpr (CmmReg r)      e                 = cheapEqReg r e
cheapEqExpr (CmmRegOff r 0) e                 = cheapEqReg r e
cheapEqExpr (CmmRegOff r n) (CmmRegOff r' n') = r==r' && n==n'
cheapEqExpr _               _                 = False

cheapEqReg :: CmmReg -> CmmExpr -> Bool
cheapEqReg r (CmmReg r')      = r==r'
cheapEqReg r (CmmRegOff r' 0) = r==r'
cheapEqReg _ _                = False

---------------------------------------------------
--
--      Helpers for foreign call arguments
--
---------------------------------------------------

loadArgsIntoTemps :: DynFlags -> [Unique]
                  -> [HintedCmmActual]
                  -> ([Unique], [CmmStmt], [HintedCmmActual])
loadArgsIntoTemps _      uniques [] = (uniques, [], [])
loadArgsIntoTemps dflags uniques ((CmmHinted e hint):args) =
    (uniques'',
     new_stmts ++ remaining_stmts,
     (CmmHinted new_e hint) : remaining_e)
    where
      (uniques', new_stmts, new_e) = maybeAssignTemp dflags uniques e
      (uniques'', remaining_stmts, remaining_e) =
          loadArgsIntoTemps dflags uniques' args


maybeAssignTemp :: DynFlags -> [Unique] -> CmmExpr -> ([Unique], [CmmStmt], CmmExpr)
maybeAssignTemp dflags uniques e
    | hasNoGlobalRegs e = (uniques, [], e)
    | otherwise         = (tail uniques, [CmmAssign local e], CmmReg local)
    where local = CmmLocal (LocalReg (head uniques) (cmmExprType dflags e))

