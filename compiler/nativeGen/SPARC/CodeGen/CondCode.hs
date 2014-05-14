
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SPARC.CodeGen.CondCode (
	getCondCode,
	condIntCode,
	condFltCode
)

where

import {-# SOURCE #-} SPARC.CodeGen.Gen32
import SPARC.CodeGen.Base
import SPARC.Instr
import SPARC.Regs
import SPARC.Cond
import SPARC.Imm
import SPARC.Base
import NCGMonad
import Size

import Cmm

import OrdList
import Outputable


getCondCode :: CmmExpr -> NatM CondCode
getCondCode (CmmMachOp mop [x, y])
  = 
    case mop of
      MO_F_Eq W32 -> condFltCode EQQ x y
      MO_F_Ne W32 -> condFltCode NE  x y
      MO_F_Gt W32 -> condFltCode GTT x y
      MO_F_Ge W32 -> condFltCode GE  x y
      MO_F_Lt W32 -> condFltCode LTT x y
      MO_F_Le W32 -> condFltCode LE  x y

      MO_F_Eq W64 -> condFltCode EQQ x y
      MO_F_Ne W64 -> condFltCode NE  x y
      MO_F_Gt W64 -> condFltCode GTT x y
      MO_F_Ge W64 -> condFltCode GE  x y
      MO_F_Lt W64 -> condFltCode LTT x y
      MO_F_Le W64 -> condFltCode LE  x y

      MO_Eq   _   -> condIntCode EQQ  x y
      MO_Ne   _   -> condIntCode NE   x y

      MO_S_Gt _   -> condIntCode GTT  x y
      MO_S_Ge _   -> condIntCode GE   x y
      MO_S_Lt _   -> condIntCode LTT  x y
      MO_S_Le _   -> condIntCode LE   x y

      MO_U_Gt _   -> condIntCode GU   x y
      MO_U_Ge _   -> condIntCode GEU  x y
      MO_U_Lt _   -> condIntCode LU   x y
      MO_U_Le _   -> condIntCode LEU  x y

      _           -> pprPanic "SPARC.CodeGen.CondCode.getCondCode" (ppr (CmmMachOp mop [x,y]))

getCondCode other = pprPanic "SPARC.CodeGen.CondCode.getCondCode" (ppr other)





-- @cond(Int|Flt)Code@: Turn a boolean expression into a condition, to be
-- passed back up the tree.

condIntCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condIntCode cond x (CmmLit (CmmInt y _))
  | fits13Bits y
  = do
       (src1, code) <- getSomeReg x
       let
           src2 = ImmInt (fromInteger y)
           code' = code `snocOL` SUB False True src1 (RIImm src2) g0
       return (CondCode False cond code')

condIntCode cond x y = do
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    let
	code__2 = code1 `appOL` code2 `snocOL`
    	    	  SUB False True src1 (RIReg src2) g0
    return (CondCode False cond code__2)


condFltCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condFltCode cond x y = do
    dflags <- getDynFlags
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    tmp <- getNewRegNat FF64
    let
   	promote x = FxTOy FF32 FF64 x tmp

    	pk1   = cmmExprType dflags x
    	pk2   = cmmExprType dflags y

    	code__2 =
		if pk1 `cmmEqType` pk2 then
    	            code1 `appOL` code2 `snocOL`
    	    	    FCMP True (cmmTypeSize pk1) src1 src2
    	    	else if typeWidth pk1 == W32 then
    	    	    code1 `snocOL` promote src1 `appOL` code2 `snocOL`
    	    	    FCMP True FF64 tmp src2
    	    	else
    	    	    code1 `appOL` code2 `snocOL` promote src2 `snocOL`
    	    	    FCMP True FF64 src1 tmp
    return (CondCode True cond code__2)
