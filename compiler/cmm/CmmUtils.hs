-----------------------------------------------------------------------------
--
-- Cmm utilities.
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

module CmmUtils( 
	CmmStmts, noStmts, oneStmt, mkStmts, plusStmts, stmtList,
	isNopStmt,

	isTrivialCmmExpr, hasNoGlobalRegs,

	cmmRegOff, cmmLabelOff, cmmOffset, cmmOffsetLit, cmmIndex,
	cmmOffsetExpr, cmmIndexExpr, cmmLoadIndex,

 	mkIntCLit, zeroCLit,

	mkLblExpr,
  ) where

#include "HsVersions.h"

import CLabel		( CLabel )
import Cmm
import MachOp
import OrdList
import Outputable

---------------------------------------------------
--
--	CmmStmts
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
--	CmmStmt
--
---------------------------------------------------

isNopStmt :: CmmStmt -> Bool
-- If isNopStmt returns True, the stmt is definitely a no-op;
-- but it might be a no-op even if isNopStmt returns False
isNopStmt CmmNop 		       = True
isNopStmt (CmmAssign r e) 	       = cheapEqReg r e
isNopStmt (CmmStore e1 (CmmLoad e2 _)) = cheapEqExpr e1 e2
isNopStmt s 			       = False

cheapEqExpr :: CmmExpr -> CmmExpr -> Bool
cheapEqExpr (CmmReg r)      e 		      = cheapEqReg r e
cheapEqExpr (CmmRegOff r 0) e 		      = cheapEqReg r e
cheapEqExpr (CmmRegOff r n) (CmmRegOff r' n') = r==r' && n==n'
cheapEqExpr e1 		    e2		      = False

cheapEqReg :: CmmReg -> CmmExpr -> Bool
cheapEqReg r (CmmReg r')      = r==r'
cheapEqReg r (CmmRegOff r' 0) = r==r'
cheapEqReg r e		      = False

---------------------------------------------------
--
--	CmmExpr
--
---------------------------------------------------

isTrivialCmmExpr :: CmmExpr -> Bool
isTrivialCmmExpr (CmmLoad _ _)   = False
isTrivialCmmExpr (CmmMachOp _ _) = False
isTrivialCmmExpr (CmmLit _)      = True
isTrivialCmmExpr (CmmReg _)      = True
isTrivialCmmExpr (CmmRegOff _ _) = True

hasNoGlobalRegs :: CmmExpr -> Bool
hasNoGlobalRegs (CmmLoad e _)   	   = hasNoGlobalRegs e
hasNoGlobalRegs (CmmMachOp _ es) 	   = all hasNoGlobalRegs es
hasNoGlobalRegs (CmmLit _)      	   = True
hasNoGlobalRegs (CmmReg (CmmLocal _))      = True
hasNoGlobalRegs (CmmRegOff (CmmLocal _) _) = True
hasNoGlobalRegs _ = False

---------------------------------------------------
--
--	Expr Construction helpers
--
---------------------------------------------------

cmmOffsetExpr :: CmmExpr -> CmmExpr -> CmmExpr
-- assumes base and offset have the same MachRep
cmmOffsetExpr e (CmmLit (CmmInt n _)) = cmmOffset e (fromInteger n)
cmmOffsetExpr e byte_off = CmmMachOp (MO_Add (cmmExprRep e)) [e, byte_off]

-- NB. Do *not* inspect the value of the offset in these smart constructors!!!
--
-- because the offset is sometimes involved in a loop in the code generator
-- (we don't know the real Hp offset until we've generated code for the entire
-- basic block, for example).  So we cannot eliminate zero offsets at this
-- stage; they're eliminated later instead (either during printing or
-- a later optimisation step on Cmm).
--
cmmOffset :: CmmExpr -> Int -> CmmExpr
cmmOffset (CmmReg reg)      byte_off = cmmRegOff reg byte_off
cmmOffset (CmmRegOff reg m) byte_off = cmmRegOff reg (m+byte_off)
cmmOffset (CmmLit lit)      byte_off = CmmLit (cmmOffsetLit lit byte_off)
cmmOffset (CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt byte_off1 _rep)]) byte_off2
  = CmmMachOp (MO_Add rep) 
	      [expr, CmmLit (CmmInt (byte_off1 + toInteger byte_off2) rep)]
cmmOffset expr byte_off
  = CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt (toInteger byte_off) rep)]
  where
    rep = cmmExprRep expr

-- Smart constructor for CmmRegOff.  Same caveats as cmmOffset above.
cmmRegOff :: CmmReg -> Int -> CmmExpr
cmmRegOff reg byte_off = CmmRegOff reg byte_off

cmmOffsetLit :: CmmLit -> Int -> CmmLit
cmmOffsetLit (CmmLabel l)      byte_off = cmmLabelOff	l byte_off
cmmOffsetLit (CmmLabelOff l m) byte_off = cmmLabelOff	l (m+byte_off)
cmmOffsetLit (CmmInt m rep)    byte_off = CmmInt (m + fromIntegral byte_off) rep
cmmOffsetLit other	       byte_off = pprPanic "cmmOffsetLit" (ppr byte_off)

cmmLabelOff :: CLabel -> Int -> CmmLit
-- Smart constructor for CmmLabelOff
cmmLabelOff lbl 0        = CmmLabel lbl
cmmLabelOff lbl byte_off = CmmLabelOff lbl byte_off

-- | Useful for creating an index into an array, with a staticaly known offset.
cmmIndex :: MachRep -> CmmExpr -> Int -> CmmExpr
cmmIndex rep base idx = cmmOffset base (idx * machRepByteWidth rep)

-- | Useful for creating an index into an array, with an unknown offset.
cmmIndexExpr :: MachRep -> CmmExpr -> CmmExpr -> CmmExpr
cmmIndexExpr rep base (CmmLit (CmmInt n _)) = cmmIndex rep base (fromInteger n)
cmmIndexExpr rep base idx =
  cmmOffsetExpr base byte_off
  where
    idx_rep = cmmExprRep idx
    byte_off = CmmMachOp (MO_Shl idx_rep) [
		  idx, CmmLit (mkIntCLit (machRepLogWidth rep))]

cmmLoadIndex :: MachRep -> CmmExpr -> Int -> CmmExpr
cmmLoadIndex rep expr ix = CmmLoad (cmmIndex rep expr ix) rep

---------------------------------------------------
--
--	Literal construction functions
--
---------------------------------------------------

mkIntCLit :: Int -> CmmLit
mkIntCLit i = CmmInt (toInteger i) wordRep

zeroCLit :: CmmLit
zeroCLit = CmmInt 0 wordRep

mkLblExpr :: CLabel -> CmmExpr
mkLblExpr lbl = CmmLit (CmmLabel lbl)
