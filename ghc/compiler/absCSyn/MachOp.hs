
module MachOp 	( MachOp(..), pprMachOp,
		  isDefinitelyInlineMachOp, 
		  isCommutableMachOp,
		  isComparisonMachOp,
                  resultRepOfMachOp
                 )
where

#include "HsVersions.h"

import PrimRep		( PrimRep(..) )
import Outputable


{- Machine-level primops; ones which we can reasonably delegate to the
   native code generators to handle.  Basically contains C's primops
   and no others.

   Nomenclature: all ops indicate width and signedness, where
   appropriate.  Widths: 8/16/32/64 means the given size, obviously.
   Nat means the operation works on STG word sized objects.
   Signedness: S means signed, U means unsigned.  For operations where
   signedness is irrelevant or makes no difference (for example
   integer add), the signedness component is omitted.

   An exception: NatP is a ptr-typed native word.  From the point of
   view of the native code generators this distinction is irrelevant,
   but the C code generator sometimes needs this info to emit the
   right casts.  
-}

data MachOp

  -- OPS at the native word size
  = MO_Nat_Add		-- +
  | MO_Nat_Sub		-- -
  | MO_Nat_Eq
  | MO_Nat_Ne

  | MO_NatS_Ge
  | MO_NatS_Le
  | MO_NatS_Gt
  | MO_NatS_Lt

  | MO_NatU_Ge
  | MO_NatU_Le
  | MO_NatU_Gt
  | MO_NatU_Lt

  | MO_NatS_Mul		-- low word of signed *
  | MO_NatS_MulMayOflo	-- nonzero if high word of signed * might contain useful info
  | MO_NatS_Quot	-- signed / (same semantics as IntQuotOp)
  | MO_NatS_Rem		-- signed % (same semantics as IntRemOp)
  | MO_NatS_Neg		-- unary -

  | MO_NatU_Mul		-- low word of unsigned *
  | MO_NatU_Quot	-- unsigned / (same semantics as WordQuotOp)
  | MO_NatU_Rem		-- unsigned % (same semantics as WordRemOp)

  | MO_Nat_And
  | MO_Nat_Or
  | MO_Nat_Xor
  | MO_Nat_Not
  | MO_Nat_Shl
  | MO_Nat_Shr
  | MO_Nat_Sar

  -- OPS at 32 bits regardless of word size
  | MO_32U_Eq
  | MO_32U_Ne
  | MO_32U_Ge
  | MO_32U_Le
  | MO_32U_Gt
  | MO_32U_Lt

  -- IEEE754 Double ops
  | MO_Dbl_Eq
  | MO_Dbl_Ne
  | MO_Dbl_Ge
  | MO_Dbl_Le
  | MO_Dbl_Gt
  | MO_Dbl_Lt

  | MO_Dbl_Add
  | MO_Dbl_Sub
  | MO_Dbl_Mul
  | MO_Dbl_Div
  | MO_Dbl_Pwr

  | MO_Dbl_Sin
  | MO_Dbl_Cos
  | MO_Dbl_Tan
  | MO_Dbl_Sinh
  | MO_Dbl_Cosh
  | MO_Dbl_Tanh
  | MO_Dbl_Asin
  | MO_Dbl_Acos
  | MO_Dbl_Atan
  | MO_Dbl_Log
  | MO_Dbl_Exp
  | MO_Dbl_Sqrt
  | MO_Dbl_Neg

  -- IEEE754 Float ops
  | MO_Flt_Add
  | MO_Flt_Sub
  | MO_Flt_Mul
  | MO_Flt_Div
  | MO_Flt_Pwr

  | MO_Flt_Eq
  | MO_Flt_Ne
  | MO_Flt_Ge
  | MO_Flt_Le
  | MO_Flt_Gt
  | MO_Flt_Lt

  | MO_Flt_Sin
  | MO_Flt_Cos
  | MO_Flt_Tan
  | MO_Flt_Sinh
  | MO_Flt_Cosh
  | MO_Flt_Tanh
  | MO_Flt_Asin
  | MO_Flt_Acos
  | MO_Flt_Atan
  | MO_Flt_Log
  | MO_Flt_Exp
  | MO_Flt_Neg
  | MO_Flt_Sqrt

  -- Conversions.  Some of these are NOPs, in which case they
  -- are here usually to placate the C code generator.
  | MO_32U_to_NatS
  | MO_NatS_to_32U

  | MO_NatS_to_Dbl
  | MO_Dbl_to_NatS

  | MO_NatS_to_Flt
  | MO_Flt_to_NatS

  | MO_NatS_to_NatU
  | MO_NatU_to_NatS

  | MO_NatS_to_NatP
  | MO_NatP_to_NatS
  | MO_NatU_to_NatP
  | MO_NatP_to_NatU

  | MO_Dbl_to_Flt
  | MO_Flt_to_Dbl

  | MO_8S_to_NatS
  | MO_16S_to_NatS
  | MO_32S_to_NatS
  | MO_8U_to_NatU
  | MO_16U_to_NatU
  | MO_32U_to_NatU

  | MO_8U_to_32U	-- zero extend
  | MO_32U_to_8U	-- mask out all but lowest byte

    deriving Eq



-- Almost, but not quite == text . derived show
pprMachOp :: MachOp -> SDoc

pprMachOp MO_Nat_Add       = text "MO_Nat_Add"
pprMachOp MO_Nat_Sub       = text "MO_Nat_Sub"
pprMachOp MO_Nat_Eq        = text "MO_Nat_Eq"
pprMachOp MO_Nat_Ne        = text "MO_Nat_Ne"

pprMachOp MO_NatS_Ge       = text "MO_NatS_Ge"
pprMachOp MO_NatS_Le       = text "MO_NatS_Le"
pprMachOp MO_NatS_Gt       = text "MO_NatS_Gt"
pprMachOp MO_NatS_Lt       = text "MO_NatS_Lt"

pprMachOp MO_NatU_Ge       = text "MO_NatU_Ge"
pprMachOp MO_NatU_Le       = text "MO_NatU_Le"
pprMachOp MO_NatU_Gt       = text "MO_NatU_Gt"
pprMachOp MO_NatU_Lt       = text "MO_NatU_Lt"

pprMachOp MO_NatS_Mul      = text "MO_NatS_Mul"
pprMachOp MO_NatS_MulMayOflo = text "MO_NatS_MulMayOflo"
pprMachOp MO_NatS_Quot     = text "MO_NatS_Quot"
pprMachOp MO_NatS_Rem      = text "MO_NatS_Rem"
pprMachOp MO_NatS_Neg      = text "MO_NatS_Neg"

pprMachOp MO_NatU_Mul      = text "MO_NatU_Mul"
pprMachOp MO_NatU_Quot     = text "MO_NatU_Quot"
pprMachOp MO_NatU_Rem      = text "MO_NatU_Rem"

pprMachOp MO_Nat_And       = text "MO_Nat_And"
pprMachOp MO_Nat_Or        = text "MO_Nat_Or"
pprMachOp MO_Nat_Xor       = text "MO_Nat_Xor"
pprMachOp MO_Nat_Not       = text "MO_Nat_Not"
pprMachOp MO_Nat_Shl       = text "MO_Nat_Shl"
pprMachOp MO_Nat_Shr       = text "MO_Nat_Shr"
pprMachOp MO_Nat_Sar       = text "MO_Nat_Sar"

pprMachOp MO_32U_Eq        = text "MO_32U_Eq"
pprMachOp MO_32U_Ne        = text "MO_32U_Ne"
pprMachOp MO_32U_Ge        = text "MO_32U_Ge"
pprMachOp MO_32U_Le        = text "MO_32U_Le"
pprMachOp MO_32U_Gt        = text "MO_32U_Gt"
pprMachOp MO_32U_Lt        = text "MO_32U_Lt"

pprMachOp MO_Dbl_Eq        = text "MO_Dbl_Eq"
pprMachOp MO_Dbl_Ne        = text "MO_Dbl_Ne"
pprMachOp MO_Dbl_Ge        = text "MO_Dbl_Ge"
pprMachOp MO_Dbl_Le        = text "MO_Dbl_Le"
pprMachOp MO_Dbl_Gt        = text "MO_Dbl_Gt"
pprMachOp MO_Dbl_Lt        = text "MO_Dbl_Lt"

pprMachOp MO_Dbl_Add       = text "MO_Dbl_Add"
pprMachOp MO_Dbl_Sub       = text "MO_Dbl_Sub"
pprMachOp MO_Dbl_Mul       = text "MO_Dbl_Mul"
pprMachOp MO_Dbl_Div       = text "MO_Dbl_Div"
pprMachOp MO_Dbl_Pwr       = text "MO_Dbl_Pwr"

pprMachOp MO_Dbl_Sin       = text "MO_Dbl_Sin"
pprMachOp MO_Dbl_Cos       = text "MO_Dbl_Cos"
pprMachOp MO_Dbl_Tan       = text "MO_Dbl_Tan"
pprMachOp MO_Dbl_Sinh      = text "MO_Dbl_Sinh"
pprMachOp MO_Dbl_Cosh      = text "MO_Dbl_Cosh"
pprMachOp MO_Dbl_Tanh      = text "MO_Dbl_Tanh"
pprMachOp MO_Dbl_Asin      = text "MO_Dbl_Asin"
pprMachOp MO_Dbl_Acos      = text "MO_Dbl_Acos"
pprMachOp MO_Dbl_Atan      = text "MO_Dbl_Atan"
pprMachOp MO_Dbl_Log       = text "MO_Dbl_Log"
pprMachOp MO_Dbl_Exp       = text "MO_Dbl_Exp"
pprMachOp MO_Dbl_Sqrt      = text "MO_Dbl_Sqrt"
pprMachOp MO_Dbl_Neg       = text "MO_Dbl_Neg"

pprMachOp MO_Flt_Add       = text "MO_Flt_Add"
pprMachOp MO_Flt_Sub       = text "MO_Flt_Sub"
pprMachOp MO_Flt_Mul       = text "MO_Flt_Mul"
pprMachOp MO_Flt_Div       = text "MO_Flt_Div"
pprMachOp MO_Flt_Pwr       = text "MO_Flt_Pwr"

pprMachOp MO_Flt_Eq        = text "MO_Flt_Eq"
pprMachOp MO_Flt_Ne        = text "MO_Flt_Ne"
pprMachOp MO_Flt_Ge        = text "MO_Flt_Ge"
pprMachOp MO_Flt_Le        = text "MO_Flt_Le"
pprMachOp MO_Flt_Gt        = text "MO_Flt_Gt"
pprMachOp MO_Flt_Lt        = text "MO_Flt_Lt"

pprMachOp MO_Flt_Sin       = text "MO_Flt_Sin"
pprMachOp MO_Flt_Cos       = text "MO_Flt_Cos"
pprMachOp MO_Flt_Tan       = text "MO_Flt_Tan"
pprMachOp MO_Flt_Sinh      = text "MO_Flt_Sinh"
pprMachOp MO_Flt_Cosh      = text "MO_Flt_Cosh"
pprMachOp MO_Flt_Tanh      = text "MO_Flt_Tanh"
pprMachOp MO_Flt_Asin      = text "MO_Flt_Asin"
pprMachOp MO_Flt_Acos      = text "MO_Flt_Acos"
pprMachOp MO_Flt_Atan      = text "MO_Flt_Atan"
pprMachOp MO_Flt_Log       = text "MO_Flt_Log"
pprMachOp MO_Flt_Exp       = text "MO_Flt_Exp"
pprMachOp MO_Flt_Sqrt      = text "MO_Flt_Sqrt"
pprMachOp MO_Flt_Neg       = text "MO_Flt_Neg"

pprMachOp MO_32U_to_NatS   = text "MO_32U_to_NatS"
pprMachOp MO_NatS_to_32U   = text "MO_NatS_to_32U"

pprMachOp MO_NatS_to_Dbl   = text "MO_NatS_to_Dbl"
pprMachOp MO_Dbl_to_NatS   = text "MO_Dbl_to_NatS"

pprMachOp MO_NatS_to_Flt   = text "MO_NatS_to_Flt"
pprMachOp MO_Flt_to_NatS   = text "MO_Flt_to_NatS"

pprMachOp MO_NatS_to_NatU  = text "MO_NatS_to_NatU"
pprMachOp MO_NatU_to_NatS  = text "MO_NatU_to_NatS"

pprMachOp MO_NatS_to_NatP  = text "MO_NatS_to_NatP"
pprMachOp MO_NatP_to_NatS  = text "MO_NatP_to_NatS"
pprMachOp MO_NatU_to_NatP  = text "MO_NatU_to_NatP"
pprMachOp MO_NatP_to_NatU  = text "MO_NatP_to_NatU"

pprMachOp MO_Dbl_to_Flt    = text "MO_Dbl_to_Flt"
pprMachOp MO_Flt_to_Dbl    = text "MO_Flt_to_Dbl"

pprMachOp MO_8S_to_NatS    = text "MO_8S_to_NatS"
pprMachOp MO_16S_to_NatS   = text "MO_16S_to_NatS"
pprMachOp MO_32S_to_NatS   = text "MO_32S_to_NatS"

pprMachOp MO_8U_to_NatU    = text "MO_8U_to_NatU"
pprMachOp MO_16U_to_NatU   = text "MO_16U_to_NatU"
pprMachOp MO_32U_to_NatU   = text "MO_32U_to_NatU"

pprMachOp MO_8U_to_32U     = text "MO_8U_to_32U"
pprMachOp MO_32U_to_8U     = text "MO_32U_to_8U"



-- Non-exported helper enumeration:
data MO_Prop 
   = MO_Commutable 
   | MO_DefinitelyInline 
   | MO_Comparison
     deriving Eq

comm   = MO_Commutable
inline = MO_DefinitelyInline
comp   = MO_Comparison


-- If in doubt, return False.  This generates worse code on the
-- via-C route, but has no effect on the native code routes.
-- Remember that claims about definitely inline have to be true
-- regardless of what the C compiler does, so we need to be 
-- careful about boundary cases like sqrt which are sometimes
-- implemented in software and sometimes in hardware.
isDefinitelyInlineMachOp :: MachOp -> Bool
isDefinitelyInlineMachOp mop = inline `elem` snd (machOpProps mop)

-- If in doubt, return False.  This generates worse code on the
-- native routes, but is otherwise harmless.
isCommutableMachOp :: MachOp -> Bool
isCommutableMachOp mop = comm `elem` snd (machOpProps mop)

-- If in doubt, return False.  This generates worse code on the
-- native routes, but is otherwise harmless.
isComparisonMachOp :: MachOp -> Bool
isComparisonMachOp mop = comp `elem` snd (machOpProps mop)

-- Find the PrimRep for the returned value of the MachOp.
resultRepOfMachOp :: MachOp -> PrimRep
resultRepOfMachOp mop = fst (machOpProps mop)

-- This bit does the real work.
machOpProps :: MachOp -> (PrimRep, [MO_Prop])

machOpProps MO_Nat_Add       = (IntRep, [inline, comm])
machOpProps MO_Nat_Sub       = (IntRep, [inline])
machOpProps MO_Nat_Eq        = (IntRep, [inline, comp, comm])
machOpProps MO_Nat_Ne        = (IntRep, [inline, comp, comm])

machOpProps MO_NatS_Ge       = (IntRep, [inline, comp])
machOpProps MO_NatS_Le       = (IntRep, [inline, comp])
machOpProps MO_NatS_Gt       = (IntRep, [inline, comp])
machOpProps MO_NatS_Lt       = (IntRep, [inline, comp])

machOpProps MO_NatU_Ge       = (IntRep, [inline, comp])
machOpProps MO_NatU_Le       = (IntRep, [inline, comp])
machOpProps MO_NatU_Gt       = (IntRep, [inline, comp])
machOpProps MO_NatU_Lt       = (IntRep, [inline, comp])

machOpProps MO_NatS_Mul      = (IntRep, [inline, comm])
machOpProps MO_NatS_MulMayOflo = (IntRep, [inline, comm])
machOpProps MO_NatS_Quot     = (IntRep, [inline])
machOpProps MO_NatS_Rem      = (IntRep, [inline])
machOpProps MO_NatS_Neg      = (IntRep, [inline])

machOpProps MO_NatU_Mul      = (WordRep, [inline, comm])
machOpProps MO_NatU_Quot     = (WordRep, [inline])
machOpProps MO_NatU_Rem      = (WordRep, [inline])

machOpProps MO_Nat_And       = (IntRep, [inline, comm])
machOpProps MO_Nat_Or        = (IntRep, [inline, comm])
machOpProps MO_Nat_Xor       = (IntRep, [inline, comm])
machOpProps MO_Nat_Not       = (IntRep, [inline])
machOpProps MO_Nat_Shl       = (IntRep, [inline])
machOpProps MO_Nat_Shr       = (IntRep, [inline])
machOpProps MO_Nat_Sar       = (IntRep, [inline])

machOpProps MO_32U_Eq        = (IntRep, [inline, comp, comm])
machOpProps MO_32U_Ne        = (IntRep, [inline, comp, comm])
machOpProps MO_32U_Ge        = (IntRep, [inline, comp])
machOpProps MO_32U_Le        = (IntRep, [inline, comp])
machOpProps MO_32U_Gt        = (IntRep, [inline, comp])
machOpProps MO_32U_Lt        = (IntRep, [inline, comp])

machOpProps MO_Dbl_Eq        = (IntRep, [inline, comp, comm])
machOpProps MO_Dbl_Ne        = (IntRep, [inline, comp, comm])
machOpProps MO_Dbl_Ge        = (IntRep, [inline, comp])
machOpProps MO_Dbl_Le        = (IntRep, [inline, comp])
machOpProps MO_Dbl_Gt        = (IntRep, [inline, comp])
machOpProps MO_Dbl_Lt        = (IntRep, [inline, comp])

machOpProps MO_Dbl_Add       = (DoubleRep, [inline, comm])
machOpProps MO_Dbl_Sub       = (DoubleRep, [inline])
machOpProps MO_Dbl_Mul       = (DoubleRep, [inline, comm])
machOpProps MO_Dbl_Div       = (DoubleRep, [inline])
machOpProps MO_Dbl_Pwr       = (DoubleRep, [])

machOpProps MO_Dbl_Sin       = (DoubleRep, [])
machOpProps MO_Dbl_Cos       = (DoubleRep, [])
machOpProps MO_Dbl_Tan       = (DoubleRep, [])
machOpProps MO_Dbl_Sinh      = (DoubleRep, [])
machOpProps MO_Dbl_Cosh      = (DoubleRep, [])
machOpProps MO_Dbl_Tanh      = (DoubleRep, [])
machOpProps MO_Dbl_Asin      = (DoubleRep, [])
machOpProps MO_Dbl_Acos      = (DoubleRep, [])
machOpProps MO_Dbl_Atan      = (DoubleRep, [])
machOpProps MO_Dbl_Log       = (DoubleRep, [])
machOpProps MO_Dbl_Exp       = (DoubleRep, [])
machOpProps MO_Dbl_Sqrt      = (DoubleRep, [])
machOpProps MO_Dbl_Neg       = (DoubleRep, [inline])

machOpProps MO_Flt_Add       = (FloatRep, [inline, comm])
machOpProps MO_Flt_Sub       = (FloatRep, [inline])
machOpProps MO_Flt_Mul       = (FloatRep, [inline, comm])
machOpProps MO_Flt_Div       = (FloatRep, [inline])
machOpProps MO_Flt_Pwr       = (FloatRep, [])

machOpProps MO_Flt_Eq        = (IntRep, [inline, comp, comm])
machOpProps MO_Flt_Ne        = (IntRep, [inline, comp, comm])
machOpProps MO_Flt_Ge        = (IntRep, [inline, comp])
machOpProps MO_Flt_Le        = (IntRep, [inline, comp])
machOpProps MO_Flt_Gt        = (IntRep, [inline, comp])
machOpProps MO_Flt_Lt        = (IntRep, [inline, comp])

machOpProps MO_Flt_Sin       = (FloatRep, [])
machOpProps MO_Flt_Cos       = (FloatRep, [])
machOpProps MO_Flt_Tan       = (FloatRep, [])
machOpProps MO_Flt_Sinh      = (FloatRep, [])
machOpProps MO_Flt_Cosh      = (FloatRep, [])
machOpProps MO_Flt_Tanh      = (FloatRep, [])
machOpProps MO_Flt_Asin      = (FloatRep, [])
machOpProps MO_Flt_Acos      = (FloatRep, [])
machOpProps MO_Flt_Atan      = (FloatRep, [])
machOpProps MO_Flt_Log       = (FloatRep, [])
machOpProps MO_Flt_Exp       = (FloatRep, [])
machOpProps MO_Flt_Sqrt      = (FloatRep, [])
machOpProps MO_Flt_Neg       = (FloatRep, [inline])

machOpProps MO_32U_to_NatS   = (IntRep, [inline])
machOpProps MO_NatS_to_32U   = (Word32Rep, [inline])

machOpProps MO_NatS_to_Dbl   = (DoubleRep, [inline])
machOpProps MO_Dbl_to_NatS   = (IntRep, [inline])

machOpProps MO_NatS_to_Flt   = (FloatRep, [inline])
machOpProps MO_Flt_to_NatS   = (IntRep, [inline])

machOpProps MO_NatS_to_NatU  = (WordRep, [inline])
machOpProps MO_NatU_to_NatS  = (IntRep, [inline])

machOpProps MO_NatS_to_NatP  = (PtrRep, [inline])
machOpProps MO_NatP_to_NatS  = (IntRep, [inline])
machOpProps MO_NatU_to_NatP  = (PtrRep, [inline])
machOpProps MO_NatP_to_NatU  = (WordRep, [inline])

machOpProps MO_Dbl_to_Flt    = (FloatRep, [inline])
machOpProps MO_Flt_to_Dbl    = (DoubleRep, [inline])

machOpProps MO_8S_to_NatS    = (IntRep, [inline])
machOpProps MO_16S_to_NatS   = (IntRep, [inline])
machOpProps MO_32S_to_NatS   = (IntRep, [inline])

machOpProps MO_8U_to_NatU    = (WordRep, [inline])
machOpProps MO_16U_to_NatU   = (WordRep, [inline])
machOpProps MO_32U_to_NatU   = (WordRep, [inline])

machOpProps MO_8U_to_32U     = (Word32Rep, [inline])
machOpProps MO_32U_to_8U     = (Word8Rep, [inline])
