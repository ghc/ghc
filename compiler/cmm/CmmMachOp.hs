
module CmmMachOp
    ( MachOp(..)
    , pprMachOp, isCommutableMachOp, isAssociativeMachOp
    , isComparisonMachOp, machOpResultType
    , machOpArgReps, maybeInvertComparison

    -- MachOp builders
    , mo_wordAdd, mo_wordSub, mo_wordEq, mo_wordNe,mo_wordMul, mo_wordSQuot
    , mo_wordSRem, mo_wordSNeg, mo_wordUQuot, mo_wordURem
    , mo_wordSGe, mo_wordSLe, mo_wordSGt, mo_wordSLt, mo_wordUGe
    , mo_wordULe, mo_wordUGt, mo_wordULt
    , mo_wordAnd, mo_wordOr, mo_wordXor, mo_wordNot, mo_wordShl, mo_wordSShr, mo_wordUShr
    , mo_u_8To32, mo_s_8To32, mo_u_16To32, mo_s_16To32
    , mo_u_8ToWord, mo_s_8ToWord, mo_u_16ToWord, mo_s_16ToWord, mo_u_32ToWord, mo_s_32ToWord
    , mo_32To8, mo_32To16, mo_WordTo8, mo_WordTo16, mo_WordTo32, mo_WordTo64

    -- CallishMachOp
    , CallishMachOp(..), callishMachOpHints
    , pprCallishMachOp
   )
where

#include "HsVersions.h"

import CmmType
import Outputable
import DynFlags

-----------------------------------------------------------------------------
--              MachOp
-----------------------------------------------------------------------------

{- |
Machine-level primops; ones which we can reasonably delegate to the
native code generators to handle.

Most operations are parameterised by the 'Width' that they operate on.
Some operations have separate signed and unsigned versions, and float
and integer versions.
-}

data MachOp
  -- Integer operations (insensitive to signed/unsigned)
  = MO_Add Width
  | MO_Sub Width
  | MO_Eq  Width
  | MO_Ne  Width
  | MO_Mul Width                -- low word of multiply

  -- Signed multiply/divide
  | MO_S_MulMayOflo Width       -- nonzero if signed multiply overflows
  | MO_S_Quot Width             -- signed / (same semantics as IntQuotOp)
  | MO_S_Rem  Width             -- signed % (same semantics as IntRemOp)
  | MO_S_Neg  Width             -- unary -

  -- Unsigned multiply/divide
  | MO_U_MulMayOflo Width       -- nonzero if unsigned multiply overflows
  | MO_U_Quot Width             -- unsigned / (same semantics as WordQuotOp)
  | MO_U_Rem  Width             -- unsigned % (same semantics as WordRemOp)

  -- Signed comparisons
  | MO_S_Ge Width
  | MO_S_Le Width
  | MO_S_Gt Width
  | MO_S_Lt Width

  -- Unsigned comparisons
  | MO_U_Ge Width
  | MO_U_Le Width
  | MO_U_Gt Width
  | MO_U_Lt Width

  -- Floating point arithmetic
  | MO_F_Add  Width
  | MO_F_Sub  Width
  | MO_F_Neg  Width             -- unary -
  | MO_F_Mul  Width
  | MO_F_Quot Width

  -- Floating point comparison
  | MO_F_Eq Width
  | MO_F_Ne Width
  | MO_F_Ge Width
  | MO_F_Le Width
  | MO_F_Gt Width
  | MO_F_Lt Width

  -- Bitwise operations.  Not all of these may be supported
  -- at all sizes, and only integral Widths are valid.
  | MO_And   Width
  | MO_Or    Width
  | MO_Xor   Width
  | MO_Not   Width
  | MO_Shl   Width
  | MO_U_Shr Width      -- unsigned shift right
  | MO_S_Shr Width      -- signed shift right

  -- Conversions.  Some of these will be NOPs.
  -- Floating-point conversions use the signed variant.
  | MO_SF_Conv Width Width      -- Signed int -> Float
  | MO_FS_Conv Width Width      -- Float -> Signed int
  | MO_SS_Conv Width Width      -- Signed int -> Signed int
  | MO_UU_Conv Width Width      -- unsigned int -> unsigned int
  | MO_FF_Conv Width Width      -- Float -> Float

  -- Vector element insertion and extraction operations
  | MO_V_Insert  Length Width   -- Insert scalar into vector
  | MO_V_Extract Length Width   -- Extract scalar from vector
  
  -- Integer vector operations
  | MO_V_Add Length Width  
  | MO_V_Sub Length Width  
  | MO_V_Mul Length Width

  -- Signed vector multiply/divide
  | MO_VS_Quot Length Width
  | MO_VS_Rem  Length Width
  | MO_VS_Neg  Length Width

  -- Unsigned vector multiply/divide
  | MO_VU_Quot Length Width
  | MO_VU_Rem  Length Width

  -- Floting point vector element insertion and extraction operations
  | MO_VF_Insert  Length Width   -- Insert scalar into vector
  | MO_VF_Extract Length Width   -- Extract scalar from vector

  -- Floating point vector operations
  | MO_VF_Add  Length Width  
  | MO_VF_Sub  Length Width  
  | MO_VF_Neg  Length Width             -- unary -
  | MO_VF_Mul  Length Width
  | MO_VF_Quot Length Width
  deriving (Eq, Show)

pprMachOp :: MachOp -> SDoc
pprMachOp mo = text (show mo)



-- -----------------------------------------------------------------------------
-- Some common MachReps

-- A 'wordRep' is a machine word on the target architecture
-- Specifically, it is the size of an Int#, Word#, Addr#
-- and the unit of allocation on the stack and the heap
-- Any pointer is also guaranteed to be a wordRep.

mo_wordAdd, mo_wordSub, mo_wordEq, mo_wordNe,mo_wordMul, mo_wordSQuot
    , mo_wordSRem, mo_wordSNeg, mo_wordUQuot, mo_wordURem
    , mo_wordSGe, mo_wordSLe, mo_wordSGt, mo_wordSLt, mo_wordUGe
    , mo_wordULe, mo_wordUGt, mo_wordULt
    , mo_wordAnd, mo_wordOr, mo_wordXor, mo_wordNot, mo_wordShl, mo_wordSShr, mo_wordUShr
    , mo_u_8ToWord, mo_s_8ToWord, mo_u_16ToWord, mo_s_16ToWord, mo_u_32ToWord, mo_s_32ToWord
    , mo_WordTo8, mo_WordTo16, mo_WordTo32, mo_WordTo64
    :: DynFlags -> MachOp

mo_u_8To32, mo_s_8To32, mo_u_16To32, mo_s_16To32
    , mo_32To8, mo_32To16
    :: MachOp

mo_wordAdd      dflags = MO_Add (wordWidth dflags)
mo_wordSub      dflags = MO_Sub (wordWidth dflags)
mo_wordEq       dflags = MO_Eq  (wordWidth dflags)
mo_wordNe       dflags = MO_Ne  (wordWidth dflags)
mo_wordMul      dflags = MO_Mul (wordWidth dflags)
mo_wordSQuot    dflags = MO_S_Quot (wordWidth dflags)
mo_wordSRem     dflags = MO_S_Rem (wordWidth dflags)
mo_wordSNeg     dflags = MO_S_Neg (wordWidth dflags)
mo_wordUQuot    dflags = MO_U_Quot (wordWidth dflags)
mo_wordURem     dflags = MO_U_Rem (wordWidth dflags)

mo_wordSGe      dflags = MO_S_Ge  (wordWidth dflags)
mo_wordSLe      dflags = MO_S_Le  (wordWidth dflags)
mo_wordSGt      dflags = MO_S_Gt  (wordWidth dflags)
mo_wordSLt      dflags = MO_S_Lt  (wordWidth dflags)

mo_wordUGe      dflags = MO_U_Ge  (wordWidth dflags)
mo_wordULe      dflags = MO_U_Le  (wordWidth dflags)
mo_wordUGt      dflags = MO_U_Gt  (wordWidth dflags)
mo_wordULt      dflags = MO_U_Lt  (wordWidth dflags)

mo_wordAnd      dflags = MO_And (wordWidth dflags)
mo_wordOr       dflags = MO_Or  (wordWidth dflags)
mo_wordXor      dflags = MO_Xor (wordWidth dflags)
mo_wordNot      dflags = MO_Not (wordWidth dflags)
mo_wordShl      dflags = MO_Shl (wordWidth dflags)
mo_wordSShr     dflags = MO_S_Shr (wordWidth dflags)
mo_wordUShr     dflags = MO_U_Shr (wordWidth dflags)

mo_u_8To32             = MO_UU_Conv W8 W32
mo_s_8To32             = MO_SS_Conv W8 W32
mo_u_16To32            = MO_UU_Conv W16 W32
mo_s_16To32            = MO_SS_Conv W16 W32

mo_u_8ToWord    dflags = MO_UU_Conv W8  (wordWidth dflags)
mo_s_8ToWord    dflags = MO_SS_Conv W8  (wordWidth dflags)
mo_u_16ToWord   dflags = MO_UU_Conv W16 (wordWidth dflags)
mo_s_16ToWord   dflags = MO_SS_Conv W16 (wordWidth dflags)
mo_s_32ToWord   dflags = MO_SS_Conv W32 (wordWidth dflags)
mo_u_32ToWord   dflags = MO_UU_Conv W32 (wordWidth dflags)

mo_WordTo8      dflags = MO_UU_Conv (wordWidth dflags) W8
mo_WordTo16     dflags = MO_UU_Conv (wordWidth dflags) W16
mo_WordTo32     dflags = MO_UU_Conv (wordWidth dflags) W32
mo_WordTo64     dflags = MO_UU_Conv (wordWidth dflags) W64

mo_32To8               = MO_UU_Conv W32 W8
mo_32To16              = MO_UU_Conv W32 W16


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
        MO_Add _                -> True
        MO_Eq _                 -> True
        MO_Ne _                 -> True
        MO_Mul _                -> True
        MO_S_MulMayOflo _       -> True
        MO_U_MulMayOflo _       -> True
        MO_And _                -> True
        MO_Or _                 -> True
        MO_Xor _                -> True
        MO_F_Add _              -> True
        MO_F_Mul _              -> True
        _other                  -> False

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
        MO_Add {} -> True       -- NB: does not include
        MO_Mul {} -> True --     floatint point!
        MO_And {} -> True
        MO_Or  {} -> True
        MO_Xor {} -> True
        _other    -> False

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
    MO_F_Eq {} -> True
    MO_F_Ne {} -> True
    MO_F_Ge {} -> True
    MO_F_Le {} -> True
    MO_F_Gt {} -> True
    MO_F_Lt {} -> True
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
  = case op of  -- None of these Just cases include floating point
        MO_Eq r   -> Just (MO_Ne r)
        MO_Ne r   -> Just (MO_Eq r)
        MO_U_Lt r -> Just (MO_U_Ge r)
        MO_U_Gt r -> Just (MO_U_Le r)
        MO_U_Le r -> Just (MO_U_Gt r)
        MO_U_Ge r -> Just (MO_U_Lt r)
        MO_S_Lt r -> Just (MO_S_Ge r)
        MO_S_Gt r -> Just (MO_S_Le r)
        MO_S_Le r -> Just (MO_S_Gt r)
        MO_S_Ge r -> Just (MO_S_Lt r)
        _other    -> Nothing

-- ----------------------------------------------------------------------------
-- machOpResultType

{- |
Returns the MachRep of the result of a MachOp.
-}
machOpResultType :: DynFlags -> MachOp -> [CmmType] -> CmmType
machOpResultType dflags mop tys =
  case mop of
    MO_Add {}           -> ty1  -- Preserve GC-ptr-hood
    MO_Sub {}           -> ty1  -- of first arg
    MO_Mul    r         -> cmmBits r
    MO_S_MulMayOflo r   -> cmmBits r
    MO_S_Quot r         -> cmmBits r
    MO_S_Rem  r         -> cmmBits r
    MO_S_Neg  r         -> cmmBits r
    MO_U_MulMayOflo r   -> cmmBits r
    MO_U_Quot r         -> cmmBits r
    MO_U_Rem  r         -> cmmBits r

    MO_Eq {}            -> comparisonResultRep dflags
    MO_Ne {}            -> comparisonResultRep dflags
    MO_S_Ge {}          -> comparisonResultRep dflags
    MO_S_Le {}          -> comparisonResultRep dflags
    MO_S_Gt {}          -> comparisonResultRep dflags
    MO_S_Lt {}          -> comparisonResultRep dflags

    MO_U_Ge {}          -> comparisonResultRep dflags
    MO_U_Le {}          -> comparisonResultRep dflags
    MO_U_Gt {}          -> comparisonResultRep dflags
    MO_U_Lt {}          -> comparisonResultRep dflags

    MO_F_Add r          -> cmmFloat r
    MO_F_Sub r          -> cmmFloat r
    MO_F_Mul r          -> cmmFloat r
    MO_F_Quot r         -> cmmFloat r
    MO_F_Neg r          -> cmmFloat r
    MO_F_Eq  {}         -> comparisonResultRep dflags
    MO_F_Ne  {}         -> comparisonResultRep dflags
    MO_F_Ge  {}         -> comparisonResultRep dflags
    MO_F_Le  {}         -> comparisonResultRep dflags
    MO_F_Gt  {}         -> comparisonResultRep dflags
    MO_F_Lt  {}         -> comparisonResultRep dflags

    MO_And {}           -> ty1  -- Used for pointer masking
    MO_Or {}            -> ty1
    MO_Xor {}           -> ty1
    MO_Not   r          -> cmmBits r
    MO_Shl   r          -> cmmBits r
    MO_U_Shr r          -> cmmBits r
    MO_S_Shr r          -> cmmBits r

    MO_SS_Conv _ to     -> cmmBits to
    MO_UU_Conv _ to     -> cmmBits to
    MO_FS_Conv _ to     -> cmmBits to
    MO_SF_Conv _ to     -> cmmFloat to
    MO_FF_Conv _ to     -> cmmFloat to

    MO_V_Insert  l w    -> cmmVec l (cmmBits w)
    MO_V_Extract _ w    -> cmmBits w

    MO_V_Add l w        -> cmmVec l (cmmBits w)
    MO_V_Sub l w        -> cmmVec l (cmmBits w)
    MO_V_Mul l w        -> cmmVec l (cmmBits w)

    MO_VS_Quot l w      -> cmmVec l (cmmBits w)
    MO_VS_Rem  l w      -> cmmVec l (cmmBits w)
    MO_VS_Neg  l w      -> cmmVec l (cmmBits w)

    MO_VU_Quot l w      -> cmmVec l (cmmBits w)
    MO_VU_Rem  l w      -> cmmVec l (cmmBits w)

    MO_VF_Insert  l w   -> cmmVec l (cmmFloat w)
    MO_VF_Extract _ w   -> cmmFloat w

    MO_VF_Add  l w      -> cmmVec l (cmmFloat w)
    MO_VF_Sub  l w      -> cmmVec l (cmmFloat w)
    MO_VF_Mul  l w      -> cmmVec l (cmmFloat w)
    MO_VF_Quot l w      -> cmmVec l (cmmFloat w)
    MO_VF_Neg  l w      -> cmmVec l (cmmFloat w)
  where
    (ty1:_) = tys

comparisonResultRep :: DynFlags -> CmmType
comparisonResultRep = bWord  -- is it?


-- -----------------------------------------------------------------------------
-- machOpArgReps

-- | This function is used for debugging only: we can check whether an
-- application of a MachOp is "type-correct" by checking that the MachReps of
-- its arguments are the same as the MachOp expects.  This is used when
-- linting a CmmExpr.

machOpArgReps :: DynFlags -> MachOp -> [Width]
machOpArgReps dflags op =
  case op of
    MO_Add    r         -> [r,r]
    MO_Sub    r         -> [r,r]
    MO_Eq     r         -> [r,r]
    MO_Ne     r         -> [r,r]
    MO_Mul    r         -> [r,r]
    MO_S_MulMayOflo r   -> [r,r]
    MO_S_Quot r         -> [r,r]
    MO_S_Rem  r         -> [r,r]
    MO_S_Neg  r         -> [r]
    MO_U_MulMayOflo r   -> [r,r]
    MO_U_Quot r         -> [r,r]
    MO_U_Rem  r         -> [r,r]

    MO_S_Ge r           -> [r,r]
    MO_S_Le r           -> [r,r]
    MO_S_Gt r           -> [r,r]
    MO_S_Lt r           -> [r,r]

    MO_U_Ge r           -> [r,r]
    MO_U_Le r           -> [r,r]
    MO_U_Gt r           -> [r,r]
    MO_U_Lt r           -> [r,r]

    MO_F_Add r          -> [r,r]
    MO_F_Sub r          -> [r,r]
    MO_F_Mul r          -> [r,r]
    MO_F_Quot r         -> [r,r]
    MO_F_Neg r          -> [r]
    MO_F_Eq  r          -> [r,r]
    MO_F_Ne  r          -> [r,r]
    MO_F_Ge  r          -> [r,r]
    MO_F_Le  r          -> [r,r]
    MO_F_Gt  r          -> [r,r]
    MO_F_Lt  r          -> [r,r]

    MO_And   r          -> [r,r]
    MO_Or    r          -> [r,r]
    MO_Xor   r          -> [r,r]
    MO_Not   r          -> [r]
    MO_Shl   r          -> [r, wordWidth dflags]
    MO_U_Shr r          -> [r, wordWidth dflags]
    MO_S_Shr r          -> [r, wordWidth dflags]

    MO_SS_Conv from _   -> [from]
    MO_UU_Conv from _   -> [from]
    MO_SF_Conv from _   -> [from]
    MO_FS_Conv from _   -> [from]
    MO_FF_Conv from _   -> [from]

    MO_V_Insert  l r    -> [typeWidth (vec l (cmmBits r)),r,wordWidth dflags]
    MO_V_Extract l r    -> [typeWidth (vec l (cmmBits r)),wordWidth dflags]

    MO_V_Add _ r        -> [r,r]
    MO_V_Sub _ r        -> [r,r]
    MO_V_Mul _ r        -> [r,r]

    MO_VS_Quot _ r      -> [r,r]
    MO_VS_Rem  _ r      -> [r,r]
    MO_VS_Neg  _ r      -> [r]

    MO_VU_Quot _ r      -> [r,r]
    MO_VU_Rem  _ r      -> [r,r]

    MO_VF_Insert  l r   -> [typeWidth (vec l (cmmFloat r)),r,wordWidth dflags]
    MO_VF_Extract l r   -> [typeWidth (vec l (cmmFloat r)),wordWidth dflags]

    MO_VF_Add  _ r      -> [r,r]
    MO_VF_Sub  _ r      -> [r,r]
    MO_VF_Mul  _ r      -> [r,r]
    MO_VF_Quot _ r      -> [r,r]
    MO_VF_Neg  _ r      -> [r]

-----------------------------------------------------------------------------
-- CallishMachOp
-----------------------------------------------------------------------------

-- CallishMachOps tend to be implemented by foreign calls in some backends,
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

  | MO_UF_Conv Width

  | MO_S_QuotRem Width
  | MO_U_QuotRem Width
  | MO_U_QuotRem2 Width
  | MO_Add2      Width
  | MO_U_Mul2    Width

  | MO_WriteBarrier
  | MO_Touch         -- Keep variables live (when using interior pointers)

  -- Prefetch
  | MO_Prefetch_Data -- Prefetch hint. May change program performance but not
                     -- program behavior.

  -- Note that these three MachOps all take 1 extra parameter than the
  -- standard C lib versions. The extra (last) parameter contains
  -- alignment of the pointers. Used for optimisation in backends.
  | MO_Memcpy
  | MO_Memset
  | MO_Memmove

  | MO_PopCnt Width
  | MO_BSwap Width
  deriving (Eq, Show)

pprCallishMachOp :: CallishMachOp -> SDoc
pprCallishMachOp mo = text (show mo)

callishMachOpHints :: CallishMachOp -> ([ForeignHint], [ForeignHint])
callishMachOpHints op = case op of
  MO_Memcpy  -> ([], [AddrHint,AddrHint,NoHint,NoHint])
  MO_Memset  -> ([], [AddrHint,NoHint,NoHint,NoHint])
  MO_Memmove -> ([], [AddrHint,AddrHint,NoHint,NoHint])
  _          -> ([],[])
  -- empty lists indicate NoHint
