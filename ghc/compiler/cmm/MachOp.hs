-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2002-2004
--
-- Low-level machine operations, used in the Cmm datatype.
--
-----------------------------------------------------------------------------

module MachOp 	( 
	MachRep(..), 
	machRepBitWidth,
	machRepByteWidth,
	machRepLogWidth,
	isFloatingRep,

	MachHint(..),

	MachOp(..), 
	pprMachOp,
	isCommutableMachOp,
	isAssociativeMachOp,
	isComparisonMachOp,
	resultRepOfMachOp,
	machOpArgReps,
	maybeInvertComparison,

	CallishMachOp(..),
	pprCallishMachOp,

	wordRep,
	halfWordRep,
	cIntRep, cLongRep,

	mo_wordAdd,
	mo_wordSub,
	mo_wordEq,
	mo_wordNe,
	mo_wordMul,
	mo_wordSQuot,
	mo_wordSRem,
	mo_wordSNeg,
	mo_wordUQuot,
	mo_wordURem,

	mo_wordSGe,
	mo_wordSLe,
	mo_wordSGt,
	mo_wordSLt,

	mo_wordUGe,
	mo_wordULe,
	mo_wordUGt,
	mo_wordULt,

	mo_wordAnd,
	mo_wordOr,
	mo_wordXor,
	mo_wordNot,
	mo_wordShl,
	mo_wordSShr,
	mo_wordUShr,

	mo_u_8To32,
	mo_s_8To32,
	mo_u_16To32,
	mo_s_16To32,

	mo_u_8ToWord,
	mo_s_8ToWord,
	mo_u_16ToWord,
	mo_s_16ToWord,
	mo_u_32ToWord,
	mo_s_32ToWord,

	mo_32To8,
	mo_32To16,
	mo_WordTo8,
	mo_WordTo16,
	mo_WordTo32,
  ) where

#include "HsVersions.h"

import Constants
import Outputable

-- -----------------------------------------------------------------------------
-- MachRep

{- |
A MachRep is the "representation" of a value in Cmm.  It is used for
resource allocation: eg. which kind of register a value should be
stored in.  

The primary requirement is that there exists a function

  cmmExprRep :: CmmExpr -> MachRep

This means that:

  - a register has an implicit MachRep
  - a literal has an implicit MachRep
  - an operation (MachOp) has an implicit result MachRep

It also means that we can check that the arguments to a MachOp have
the correct MachRep, i.e. we can do a kind of lint-style type checking
on Cmm.
-}

data MachRep
  = I8
  | I16
  | I32
  | I64
  | I128
  | F32
  | F64
  | F80		-- extended double-precision, used in x86 native codegen only.
  deriving (Eq, Ord, Show)

mrStr I8   = SLIT("I8")
mrStr I16  = SLIT("I16")
mrStr I32  = SLIT("I32")
mrStr I64  = SLIT("I64")
mrStr I128 = SLIT("I128")
mrStr F32  = SLIT("F32")
mrStr F64  = SLIT("F64")
mrStr F80  = SLIT("F80")

instance Outputable MachRep where
   ppr rep = ptext (mrStr rep)

{- 
Implementation notes:

It might suffice to keep just a width, without distinguishing between
floating and integer types.  However, keeping the distinction will
help the native code generator to assign registers more easily.
-}

{-
Should a MachRep include a signed vs. unsigned distinction?

This is very much like a "hint" in C-- terminology: it isn't necessary
in order to generate correct code, but it might be useful in that the
compiler can generate better code if it has access to higher-level
hints about data.  This is important at call boundaries, because the
definition of a function is not visible at all of its call sites, so
the compiler cannot infer the hints.

Here in Cmm, we're taking a slightly different approach.  We include
the int vs. float hint in the MachRep, because (a) the majority of
platforms have a strong distinction between float and int registers,
and (b) we don't want to do any heavyweight hint-inference in the
native code backend in order to get good code.  We're treating the
hint more like a type: our Cmm is always completely consistent with
respect to hints.  All coercions between float and int are explicit.

What about the signed vs. unsigned hint?  This information might be
useful if we want to keep sub-word-sized values in word-size
registers, which we must do if we only have word-sized registers.

On such a system, there are two straightforward conventions for
representing sub-word-sized values:

(a) Leave the upper bits undefined.  Comparison operations must
    sign- or zero-extend both operands before comparing them,
    depending on whether the comparison is signed or unsigned.

(b) Always keep the values sign- or zero-extended as appropriate.
    Arithmetic operations must narrow the result to the appropriate
    size.

A clever compiler might not use either (a) or (b) exclusively, instead
it would attempt to minimize the coercions by analysis: the same kind
of analysis that propagates hints around.  In Cmm we don't want to
have to do this, so we plump for having richer types and keeping the
type information consistent.

If signed/unsigned hints are missing from MachRep, then the only
choice we have is (a), because we don't know whether the result of an
operation should be sign- or zero-extended.

Many architectures have extending load operations, which work well
with (b).  To make use of them with (a), you need to know whether the
value is going to be sign- or zero-extended by an enclosing comparison
(for example), which involves knowing above the context.  This is
doable but more complex.

Further complicating the issue is foreign calls: a foreign calling
convention can specify that signed 8-bit quantities are passed as
sign-extended 32 bit quantities, for example (this is the case on the
PowerPC).  So we *do* need sign information on foreign call arguments.

Pros for adding signed vs. unsigned to MachRep:

  - It would let us use convention (b) above, and get easier
    code generation for extending loads.

  - Less information required on foreign calls.
  
  - MachOp type would be simpler

Cons:

  - More complexity

  - What is the MachRep for a VanillaReg?  Currently it is
    always wordRep, but now we have to decide whether it is
    signed or unsigned.  The same VanillaReg can thus have
    different MachReps in different parts of the program.

  - Extra coercions cluttering up expressions.

Currently for GHC, the foreign call point is moot, because we do our
own promotion of sub-word-sized values to word-sized values.  The Int8
type is represnted by an Int# which is kept sign-extended at all times
(this is slightly naughty, because we're making assumptions about the
C calling convention rather early on in the compiler).  However, given
this, the cons outweigh the pros.

-}


machRepBitWidth :: MachRep -> Int
machRepBitWidth I8   = 8
machRepBitWidth I16  = 16
machRepBitWidth I32  = 32
machRepBitWidth I64  = 64
machRepBitWidth I128 = 128
machRepBitWidth F32  = 32
machRepBitWidth F64  = 64
machRepBitWidth F80  = 80

machRepByteWidth :: MachRep -> Int
machRepByteWidth I8   = 1
machRepByteWidth I16  = 2
machRepByteWidth I32  = 4
machRepByteWidth I64  = 8
machRepByteWidth I128 = 16
machRepByteWidth F32  = 4
machRepByteWidth F64  = 8
machRepByteWidth F80  = 10

-- log_2 of the width in bytes, useful for generating shifts.
machRepLogWidth :: MachRep -> Int
machRepLogWidth I8   = 0
machRepLogWidth I16  = 1
machRepLogWidth I32  = 2
machRepLogWidth I64  = 3
machRepLogWidth I128 = 4
machRepLogWidth F32  = 2
machRepLogWidth F64  = 3
machRepLogWidth F80  = panic "machRepLogWidth: F80"

isFloatingRep :: MachRep -> Bool
isFloatingRep F32 = True
isFloatingRep F64 = True
isFloatingRep F80 = True
isFloatingRep _   = False

-- -----------------------------------------------------------------------------
-- Hints

{-
A hint gives a little more information about a data value.  Hints are
used on the arguments to a foreign call, where the code generator needs
to know some extra information on top of the MachRep of each argument in
order to generate a correct call.
-}

data MachHint
  = NoHint
  | PtrHint
  | SignedHint
  | FloatHint
  deriving Eq

mhStr NoHint     = SLIT("NoHint")
mhStr PtrHint    = SLIT("PtrHint")
mhStr SignedHint = SLIT("SignedHint")
mhStr FloatHint  = SLIT("FloatHint")

instance Outputable MachHint where
   ppr hint = ptext (mhStr hint)

-- -----------------------------------------------------------------------------
-- MachOp

{- |
Machine-level primops; ones which we can reasonably delegate to the
native code generators to handle.  Basically contains C's primops
and no others.

Nomenclature: all ops indicate width and signedness, where
appropriate.  Widths: 8\/16\/32\/64 means the given size, obviously.
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

  -- Integer operations
  = MO_Add    MachRep
  | MO_Sub    MachRep
  | MO_Eq     MachRep
  | MO_Ne     MachRep
  | MO_Mul    MachRep		-- low word of multiply
  | MO_S_MulMayOflo MachRep 	-- nonzero if signed multiply overflows
  | MO_S_Quot MachRep		-- signed / (same semantics as IntQuotOp)
  | MO_S_Rem  MachRep		-- signed % (same semantics as IntRemOp)
  | MO_S_Neg  MachRep		-- unary -
  | MO_U_MulMayOflo MachRep	-- nonzero if unsigned multiply overflows
  | MO_U_Quot MachRep		-- unsigned / (same semantics as WordQuotOp)
  | MO_U_Rem  MachRep		-- unsigned % (same semantics as WordRemOp)

  -- Signed comparisons (floating-point comparisons also use these)
  | MO_S_Ge MachRep
  | MO_S_Le MachRep
  | MO_S_Gt MachRep
  | MO_S_Lt MachRep

  -- Unsigned comparisons
  | MO_U_Ge MachRep
  | MO_U_Le MachRep
  | MO_U_Gt MachRep
  | MO_U_Lt MachRep

  -- Bitwise operations.  Not all of these may be supported at all sizes,
  -- and only integral MachReps are valid.
  | MO_And   MachRep
  | MO_Or    MachRep
  | MO_Xor   MachRep
  | MO_Not   MachRep
  | MO_Shl   MachRep
  | MO_U_Shr MachRep	-- unsigned shift right
  | MO_S_Shr MachRep	-- signed shift right

  -- Conversions.  Some of these will be NOPs.
  -- Floating-point conversions use the signed variant.
  | MO_S_Conv MachRep{-from-} MachRep{-to-}	-- signed conversion
  | MO_U_Conv MachRep{-from-} MachRep{-to-}	-- unsigned conversion

  deriving (Eq, Show)

pprMachOp :: MachOp -> SDoc
pprMachOp mo = text (show mo)


-- These MachOps tend to be implemented by foreign calls in some backends,
-- so we separate them out.  In Cmm, these can only occur in a
-- statement position, in contrast to an ordinary MachOp which can occur
-- anywhere in an expression.
data CallishMachOp
  = MO_F64_Pwr
  | MO_F64_Sin
  | MO_F64_Cos
  | MO_F64_Tan
  | MO_F64_Sinh
  | MO_F64_Cosh
  | MO_F64_Tanh
  | MO_F64_Asin
  | MO_F64_Acos
  | MO_F64_Atan
  | MO_F64_Log
  | MO_F64_Exp
  | MO_F64_Sqrt
  | MO_F32_Pwr
  | MO_F32_Sin
  | MO_F32_Cos
  | MO_F32_Tan
  | MO_F32_Sinh
  | MO_F32_Cosh
  | MO_F32_Tanh
  | MO_F32_Asin
  | MO_F32_Acos
  | MO_F32_Atan
  | MO_F32_Log
  | MO_F32_Exp
  | MO_F32_Sqrt
  deriving (Eq, Show)

pprCallishMachOp :: CallishMachOp -> SDoc
pprCallishMachOp mo = text (show mo)

-- -----------------------------------------------------------------------------
-- Some common MachReps

-- A 'wordRep' is a machine word on the target architecture
-- Specifically, it is the size of an Int#, Word#, Addr# 
-- and the unit of allocation on the stack and the heap
-- Any pointer is also guaranteed to be a wordRep.

wordRep | wORD_SIZE == 4 = I32
	| wORD_SIZE == 8 = I64
	| otherwise      = panic "MachOp.wordRep: Unknown word size"

halfWordRep | wORD_SIZE == 4 = I16
	    | wORD_SIZE == 8 = I32
	    | otherwise      = panic "MachOp.halfWordRep: Unknown word size"

mo_wordAdd	= MO_Add wordRep
mo_wordSub	= MO_Sub wordRep
mo_wordEq 	= MO_Eq  wordRep
mo_wordNe 	= MO_Ne  wordRep
mo_wordMul	= MO_Mul wordRep
mo_wordSQuot	= MO_S_Quot wordRep
mo_wordSRem	= MO_S_Rem wordRep
mo_wordSNeg	= MO_S_Neg wordRep
mo_wordUQuot	= MO_U_Quot wordRep
mo_wordURem	= MO_U_Rem wordRep

mo_wordSGe	= MO_S_Ge  wordRep
mo_wordSLe	= MO_S_Le  wordRep
mo_wordSGt	= MO_S_Gt  wordRep
mo_wordSLt	= MO_S_Lt  wordRep

mo_wordUGe	= MO_U_Ge  wordRep
mo_wordULe	= MO_U_Le  wordRep
mo_wordUGt	= MO_U_Gt  wordRep
mo_wordULt	= MO_U_Lt  wordRep

mo_wordAnd	= MO_And wordRep
mo_wordOr 	= MO_Or	 wordRep
mo_wordXor	= MO_Xor wordRep
mo_wordNot	= MO_Not wordRep
mo_wordShl	= MO_Shl wordRep
mo_wordSShr	= MO_S_Shr wordRep 
mo_wordUShr	= MO_U_Shr wordRep 

mo_u_8To32	= MO_U_Conv I8 I32
mo_s_8To32	= MO_S_Conv I8 I32
mo_u_16To32	= MO_U_Conv I16 I32
mo_s_16To32	= MO_S_Conv I16 I32

mo_u_8ToWord	= MO_U_Conv I8  wordRep
mo_s_8ToWord	= MO_S_Conv I8  wordRep
mo_u_16ToWord	= MO_U_Conv I16 wordRep
mo_s_16ToWord	= MO_S_Conv I16 wordRep
mo_s_32ToWord	= MO_S_Conv I32 wordRep
mo_u_32ToWord	= MO_U_Conv I32 wordRep

mo_WordTo8	= MO_U_Conv wordRep I8
mo_WordTo16	= MO_U_Conv wordRep I16
mo_WordTo32	= MO_U_Conv wordRep I32

mo_32To8	= MO_U_Conv I32 I8
mo_32To16	= MO_U_Conv I32 I16

-- cIntRep is the MachRep for a C-language 'int'
#if SIZEOF_INT == 4
cIntRep = I32
#elif  SIZEOF_INT == 8
cIntRep = I64
#endif

#if SIZEOF_LONG == 4
cLongRep = I32
#elif  SIZEOF_LONG == 8
cLongRep = I64
#endif

-- ----------------------------------------------------------------------------
-- isCommutableMachOp

{- |
Returns 'True' if the MachOp has commutable arguments.  This is used
in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isCommutableMachOp :: MachOp -> Bool
isCommutableMachOp mop = 
  case mop of
	MO_Add _ 		-> True
	MO_Eq _			-> True
	MO_Ne _			-> True
	MO_Mul _		-> True
	MO_S_MulMayOflo _	-> True
	MO_U_MulMayOflo _	-> True
	MO_And _		-> True
	MO_Or _			-> True
	MO_Xor _		-> True
	_other			-> False

-- ----------------------------------------------------------------------------
-- isAssociativeMachOp

{- |
Returns 'True' if the MachOp is associative (i.e. @(x+y)+z == x+(y+z)@)
This is used in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isAssociativeMachOp :: MachOp -> Bool
isAssociativeMachOp mop = 
  case mop of
	MO_Add r 	-> not (isFloatingRep r)
	MO_Mul r	-> not (isFloatingRep r)
	MO_And _	-> True
	MO_Or _		-> True
	MO_Xor _	-> True
	_other		-> False

-- ----------------------------------------------------------------------------
-- isComparisonMachOp

{- | 
Returns 'True' if the MachOp is a comparison.

If in doubt, return False.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isComparisonMachOp :: MachOp -> Bool
isComparisonMachOp mop = 
  case mop of
    MO_Eq   _  -> True
    MO_Ne   _  -> True
    MO_S_Ge _  -> True
    MO_S_Le _  -> True
    MO_S_Gt _  -> True
    MO_S_Lt _  -> True
    MO_U_Ge _  -> True
    MO_U_Le _  -> True
    MO_U_Gt _  -> True
    MO_U_Lt _  -> True
    _other     -> False

-- -----------------------------------------------------------------------------
-- Inverting conditions

-- Sometimes it's useful to be able to invert the sense of a
-- condition.  Not all conditional tests are invertible: in
-- particular, floating point conditionals cannot be inverted, because
-- there exist floating-point values which return False for both senses
-- of a condition (eg. !(NaN > NaN) && !(NaN /<= NaN)).

maybeInvertComparison :: MachOp -> Maybe MachOp
maybeInvertComparison op
  = case op of
	MO_Eq r    | not (isFloatingRep r) -> Just (MO_Ne r)
	MO_Ne r	   | not (isFloatingRep r) -> Just (MO_Eq r)
	MO_U_Lt r  | not (isFloatingRep r) -> Just (MO_U_Ge r)
	MO_U_Gt r  | not (isFloatingRep r) -> Just (MO_U_Le r)
	MO_U_Le r  | not (isFloatingRep r) -> Just (MO_U_Gt r)
	MO_U_Ge r  | not (isFloatingRep r) -> Just (MO_U_Lt r)
	MO_S_Lt r  | not (isFloatingRep r) -> Just (MO_S_Ge r)
	MO_S_Gt r  | not (isFloatingRep r) -> Just (MO_S_Le r)
	MO_S_Le r  | not (isFloatingRep r) -> Just (MO_S_Gt r)
	MO_S_Ge r  | not (isFloatingRep r) -> Just (MO_S_Lt r)
	_other  -> Nothing

-- ----------------------------------------------------------------------------
-- resultRepOfMachOp

{- |
Returns the MachRep of the result of a MachOp.
-}
resultRepOfMachOp :: MachOp -> MachRep
resultRepOfMachOp mop =
  case mop of
    MO_Add    r		-> r
    MO_Sub    r		-> r
    MO_Eq     r		-> comparisonResultRep
    MO_Ne     r		-> comparisonResultRep
    MO_Mul    r		-> r
    MO_S_MulMayOflo r	-> r
    MO_S_Quot r		-> r
    MO_S_Rem  r		-> r
    MO_S_Neg  r		-> r
    MO_U_MulMayOflo r	-> r
    MO_U_Quot r		-> r
    MO_U_Rem  r		-> r

    MO_S_Ge r		-> comparisonResultRep
    MO_S_Le r		-> comparisonResultRep
    MO_S_Gt r		-> comparisonResultRep
    MO_S_Lt r		-> comparisonResultRep

    MO_U_Ge r		-> comparisonResultRep
    MO_U_Le r		-> comparisonResultRep
    MO_U_Gt r		-> comparisonResultRep
    MO_U_Lt r		-> comparisonResultRep

    MO_And   r		-> r
    MO_Or    r		-> r
    MO_Xor   r		-> r
    MO_Not   r		-> r
    MO_Shl   r		-> r
    MO_U_Shr r		-> r
    MO_S_Shr r		-> r

    MO_S_Conv from to	-> to
    MO_U_Conv from to	-> to


comparisonResultRep = wordRep  -- is it?


-- -----------------------------------------------------------------------------
-- machOpArgReps

-- | This function is used for debugging only: we can check whether an
-- application of a MachOp is "type-correct" by checking that the MachReps of
-- its arguments are the same as the MachOp expects.  This is used when 
-- linting a CmmExpr.

machOpArgReps :: MachOp -> [MachRep]
machOpArgReps op = 
  case op of
    MO_Add    r		-> [r,r]
    MO_Sub    r		-> [r,r]
    MO_Eq     r		-> [r,r]
    MO_Ne     r		-> [r,r]
    MO_Mul    r		-> [r,r]
    MO_S_MulMayOflo r	-> [r,r]
    MO_S_Quot r		-> [r,r]
    MO_S_Rem  r		-> [r,r]
    MO_S_Neg  r		-> [r]
    MO_U_MulMayOflo r	-> [r,r]
    MO_U_Quot r		-> [r,r]
    MO_U_Rem  r		-> [r,r]

    MO_S_Ge r		-> [r,r]
    MO_S_Le r		-> [r,r]
    MO_S_Gt r		-> [r,r]
    MO_S_Lt r		-> [r,r]

    MO_U_Ge r		-> [r,r]
    MO_U_Le r		-> [r,r]
    MO_U_Gt r		-> [r,r]
    MO_U_Lt r		-> [r,r]

    MO_And   r		-> [r,r]
    MO_Or    r		-> [r,r]
    MO_Xor   r		-> [r,r]
    MO_Not   r		-> [r]
    MO_Shl   r		-> [r,wordRep]
    MO_U_Shr r		-> [r,wordRep]
    MO_S_Shr r		-> [r,wordRep]

    MO_S_Conv from to	-> [from]
    MO_U_Conv from to	-> [from]
