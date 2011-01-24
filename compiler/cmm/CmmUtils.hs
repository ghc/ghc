-----------------------------------------------------------------------------
--
-- Cmm utilities.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CmmUtils( 
	primRepCmmType, primRepForeignHint,
	typeCmmType, typeForeignHint,

	isTrivialCmmExpr, hasNoGlobalRegs,

	cmmRegOff, cmmLabelOff, cmmOffset, cmmOffsetLit, cmmIndex,
	cmmOffsetExpr, cmmIndexExpr, cmmLoadIndex,

 	mkIntCLit, zeroCLit,

	mkLblExpr,
  ) where

#include "HsVersions.h"

import TyCon	( PrimRep(..) )
import Type	( Type, typePrimRep )

import CLabel
import CmmDecl
import CmmExpr
import Outputable

---------------------------------------------------
--
--	CmmTypes
--
---------------------------------------------------

primRepCmmType :: PrimRep -> CmmType
primRepCmmType VoidRep    = panic "primRepCmmType:VoidRep"
primRepCmmType PtrRep     = gcWord
primRepCmmType IntRep	  = bWord
primRepCmmType WordRep	  = bWord
primRepCmmType Int64Rep   = b64
primRepCmmType Word64Rep  = b64
primRepCmmType AddrRep    = bWord
primRepCmmType FloatRep   = f32
primRepCmmType DoubleRep  = f64

typeCmmType :: Type -> CmmType
typeCmmType ty = primRepCmmType (typePrimRep ty)

primRepForeignHint :: PrimRep -> ForeignHint
primRepForeignHint VoidRep	= panic "primRepForeignHint:VoidRep"
primRepForeignHint PtrRep	= AddrHint
primRepForeignHint IntRep	= SignedHint
primRepForeignHint WordRep	= NoHint
primRepForeignHint Int64Rep	= SignedHint
primRepForeignHint Word64Rep	= NoHint
primRepForeignHint AddrRep      = AddrHint -- NB! AddrHint, but NonPtrArg
primRepForeignHint FloatRep	= NoHint
primRepForeignHint DoubleRep	= NoHint

typeForeignHint :: Type -> ForeignHint
typeForeignHint = primRepForeignHint . typePrimRep


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
isTrivialCmmExpr (CmmStackSlot _ _) = panic "isTrivialCmmExpr CmmStackSlot"

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
-- assumes base and offset have the same CmmType
cmmOffsetExpr e (CmmLit (CmmInt n _)) = cmmOffset e (fromInteger n)
cmmOffsetExpr e byte_off = CmmMachOp (MO_Add (cmmExprWidth e)) [e, byte_off]

-- NB. Do *not* inspect the value of the offset in these smart constructors!!!
-- because the offset is sometimes involved in a loop in the code generator
-- (we don't know the real Hp offset until we've generated code for the entire
-- basic block, for example).  So we cannot eliminate zero offsets at this
-- stage; they're eliminated later instead (either during printing or
-- a later optimisation step on Cmm).
--
cmmOffset :: CmmExpr -> Int -> CmmExpr
cmmOffset e                 0        = e
cmmOffset (CmmReg reg)      byte_off = cmmRegOff reg byte_off
cmmOffset (CmmRegOff reg m) byte_off = cmmRegOff reg (m+byte_off)
cmmOffset (CmmLit lit)      byte_off = CmmLit (cmmOffsetLit lit byte_off)
cmmOffset (CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt byte_off1 _rep)]) byte_off2
  = CmmMachOp (MO_Add rep) 
	      [expr, CmmLit (CmmInt (byte_off1 + toInteger byte_off2) rep)]
cmmOffset expr byte_off
  = CmmMachOp (MO_Add width) [expr, CmmLit (CmmInt (toInteger byte_off) width)]
  where
    width = cmmExprWidth expr

-- Smart constructor for CmmRegOff.  Same caveats as cmmOffset above.
cmmRegOff :: CmmReg -> Int -> CmmExpr
cmmRegOff reg byte_off = CmmRegOff reg byte_off

cmmOffsetLit :: CmmLit -> Int -> CmmLit
cmmOffsetLit (CmmLabel l)      byte_off = cmmLabelOff	l byte_off
cmmOffsetLit (CmmLabelOff l m) byte_off = cmmLabelOff	l (m+byte_off)
cmmOffsetLit (CmmInt m rep)    byte_off = CmmInt (m + fromIntegral byte_off) rep
cmmOffsetLit _    	       byte_off = pprPanic "cmmOffsetLit" (ppr byte_off)

cmmLabelOff :: CLabel -> Int -> CmmLit
-- Smart constructor for CmmLabelOff
cmmLabelOff lbl 0        = CmmLabel lbl
cmmLabelOff lbl byte_off = CmmLabelOff lbl byte_off

-- | Useful for creating an index into an array, with a staticaly known offset.
-- The type is the element type; used for making the multiplier
cmmIndex :: Width	-- Width w
	 -> CmmExpr	-- Address of vector of items of width w
	 -> Int		-- Which element of the vector (0 based)
	 -> CmmExpr	-- Address of i'th element
cmmIndex width base idx = cmmOffset base (idx * widthInBytes width)

-- | Useful for creating an index into an array, with an unknown offset.
cmmIndexExpr :: Width		-- Width w
	     -> CmmExpr		-- Address of vector of items of width w
	     -> CmmExpr		-- Which element of the vector (0 based)
	     -> CmmExpr		-- Address of i'th element
cmmIndexExpr width base (CmmLit (CmmInt n _)) = cmmIndex width base (fromInteger n)
cmmIndexExpr width base idx =
  cmmOffsetExpr base byte_off
  where
    idx_w = cmmExprWidth idx
    byte_off = CmmMachOp (MO_Shl idx_w) [idx, CmmLit (mkIntCLit (widthInLog width))]

cmmLoadIndex :: CmmType -> CmmExpr -> Int -> CmmExpr
cmmLoadIndex ty expr ix = CmmLoad (cmmIndex (typeWidth ty) expr ix) ty

---------------------------------------------------
--
--	Literal construction functions
--
---------------------------------------------------

mkIntCLit :: Int -> CmmLit
mkIntCLit i = CmmInt (toInteger i) wordWidth

zeroCLit :: CmmLit
zeroCLit = CmmInt 0 wordWidth

mkLblExpr :: CLabel -> CmmExpr
mkLblExpr lbl = CmmLit (CmmLabel lbl)
