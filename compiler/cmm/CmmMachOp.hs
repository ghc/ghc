module CmmMachOp
    ( MachOp(..)
    , pprMachOp, isCommutableMachOp, isAssociativeMachOp
    , isComparisonMachOp, maybeIntComparison, machOpResultType
    , machOpArgReps, maybeInvertComparison

    -- MachOp builders
    , mo_wordAdd, mo_wordSub, mo_wordEq, mo_wordNe,mo_wordMul, mo_wordSQuot
    , mo_wordSRem, mo_wordSNeg, mo_wordUQuot, mo_wordURem
    , mo_wordSGe, mo_wordSLe, mo_wordSGt, mo_wordSLt, mo_wordUGe
    , mo_wordULe, mo_wordUGt, mo_wordULt
    , mo_wordAnd, mo_wordOr, mo_wordXor, mo_wordNot
    , mo_wordShl, mo_wordSShr, mo_wordUShr
    , mo_u_8To32, mo_s_8To32, mo_u_16To32, mo_s_16To32
    , mo_u_8ToWord, mo_s_8ToWord, mo_u_16ToWord, mo_s_16ToWord
    , mo_u_32ToWord, mo_s_32ToWord
    , mo_32To8, mo_32To16, mo_WordTo8, mo_WordTo16, mo_WordTo32, mo_WordTo64

    -- CallishMachOp
    , CallishMachOp(..), callishMachOpHints
    , pprCallishMachOp
    , machOpMemcpyishAlign

    -- Atomic read-modify-write
    , AtomicMachOp(..)
   )
where

import GhcPrelude

import CmmType
import Outputable
import DynFlags
import Binary

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
  | MO_VF_Neg  Length Width      -- unary negation
  | MO_VF_Mul  Length Width
  | MO_VF_Quot Length Width

  -- Alignment check (for -falignment-sanitisation)
  | MO_AlignmentCheck Int Width
  deriving (Eq, Show)

pprMachOp :: MachOp -> SDoc
pprMachOp mo = text (show mo)

instance Binary MachOp where
  put_ bh (MO_Add            a)   = putByte bh 0 >> put_ bh a
  put_ bh (MO_Sub            a)   = putByte bh 1 >> put_ bh a
  put_ bh (MO_Eq             a)   = putByte bh 2 >> put_ bh a
  put_ bh (MO_Ne             a)   = putByte bh 3 >> put_ bh a
  put_ bh (MO_Mul            a)   = putByte bh 4 >> put_ bh a
  put_ bh (MO_S_MulMayOflo   a)   = putByte bh 5 >> put_ bh a
  put_ bh (MO_S_Quot         a)   = putByte bh 6 >> put_ bh a
  put_ bh (MO_S_Rem          a)   = putByte bh 7 >> put_ bh a
  put_ bh (MO_S_Neg          a)   = putByte bh 8 >> put_ bh a
  put_ bh (MO_U_MulMayOflo   a)   = putByte bh 9 >> put_ bh a
  put_ bh (MO_U_Quot         a)   = putByte bh 10 >> put_ bh a
  put_ bh (MO_U_Rem          a)   = putByte bh 11 >> put_ bh a
  put_ bh (MO_S_Ge           a)   = putByte bh 12 >> put_ bh a
  put_ bh (MO_S_Le           a)   = putByte bh 13 >> put_ bh a
  put_ bh (MO_S_Gt           a)   = putByte bh 14 >> put_ bh a
  put_ bh (MO_S_Lt           a)   = putByte bh 15 >> put_ bh a
  put_ bh (MO_U_Ge           a)   = putByte bh 16 >> put_ bh a
  put_ bh (MO_U_Le           a)   = putByte bh 17 >> put_ bh a
  put_ bh (MO_U_Gt           a)   = putByte bh 18 >> put_ bh a
  put_ bh (MO_U_Lt           a)   = putByte bh 19 >> put_ bh a
  put_ bh (MO_F_Add          a)   = putByte bh 20 >> put_ bh a
  put_ bh (MO_F_Sub          a)   = putByte bh 21 >> put_ bh a
  put_ bh (MO_F_Neg          a)   = putByte bh 22 >> put_ bh a
  put_ bh (MO_F_Mul          a)   = putByte bh 23 >> put_ bh a
  put_ bh (MO_F_Quot         a)   = putByte bh 24 >> put_ bh a
  put_ bh (MO_F_Eq           a)   = putByte bh 25 >> put_ bh a
  put_ bh (MO_F_Ne           a)   = putByte bh 26 >> put_ bh a
  put_ bh (MO_F_Ge           a)   = putByte bh 27 >> put_ bh a
  put_ bh (MO_F_Le           a)   = putByte bh 28 >> put_ bh a
  put_ bh (MO_F_Gt           a)   = putByte bh 29 >> put_ bh a
  put_ bh (MO_F_Lt           a)   = putByte bh 30 >> put_ bh a
  put_ bh (MO_And            a)   = putByte bh 31 >> put_ bh a
  put_ bh (MO_Or             a)   = putByte bh 32 >> put_ bh a
  put_ bh (MO_Xor            a)   = putByte bh 33 >> put_ bh a
  put_ bh (MO_Not            a)   = putByte bh 34 >> put_ bh a
  put_ bh (MO_Shl            a)   = putByte bh 35 >> put_ bh a
  put_ bh (MO_U_Shr          a)   = putByte bh 36 >> put_ bh a
  put_ bh (MO_S_Shr          a)   = putByte bh 37 >> put_ bh a
  put_ bh (MO_SF_Conv        a b) = putByte bh 38 >> put_ bh a >> put_ bh b
  put_ bh (MO_FS_Conv        a b) = putByte bh 39 >> put_ bh a >> put_ bh b
  put_ bh (MO_SS_Conv        a b) = putByte bh 40 >> put_ bh a >> put_ bh b
  put_ bh (MO_UU_Conv        a b) = putByte bh 41 >> put_ bh a >> put_ bh b
  put_ bh (MO_FF_Conv        a b) = putByte bh 42 >> put_ bh a >> put_ bh b
  put_ bh (MO_V_Insert       a b) = putByte bh 43 >> put_ bh a >> put_ bh b
  put_ bh (MO_V_Extract      a b) = putByte bh 44 >> put_ bh a >> put_ bh b
  put_ bh (MO_V_Add          a b) = putByte bh 45 >> put_ bh a >> put_ bh b
  put_ bh (MO_V_Sub          a b) = putByte bh 46 >> put_ bh a >> put_ bh b
  put_ bh (MO_V_Mul          a b) = putByte bh 47 >> put_ bh a >> put_ bh b
  put_ bh (MO_VS_Quot        a b) = putByte bh 48 >> put_ bh a >> put_ bh b
  put_ bh (MO_VS_Rem         a b) = putByte bh 49 >> put_ bh a >> put_ bh b
  put_ bh (MO_VS_Neg         a b) = putByte bh 50 >> put_ bh a >> put_ bh b
  put_ bh (MO_VU_Quot        a b) = putByte bh 51 >> put_ bh a >> put_ bh b
  put_ bh (MO_VU_Rem         a b) = putByte bh 52 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Insert      a b) = putByte bh 53 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Extract     a b) = putByte bh 54 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Add         a b) = putByte bh 55 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Sub         a b) = putByte bh 56 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Neg         a b) = putByte bh 57 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Mul         a b) = putByte bh 58 >> put_ bh a >> put_ bh b
  put_ bh (MO_VF_Quot        a b) = putByte bh 59 >> put_ bh a >> put_ bh b
  put_ bh (MO_AlignmentCheck a b) = putByte bh 60 >> put_ bh a >> put_ bh b

  get bh = do
    tag <- getByte bh
    case tag of      
      0 -> MO_Add            <$> get bh
      1 -> MO_Sub            <$> get bh
      2 -> MO_Eq             <$> get bh
      3 -> MO_Ne             <$> get bh
      4 -> MO_Mul            <$> get bh
      5 -> MO_S_MulMayOflo   <$> get bh
      6 -> MO_S_Quot         <$> get bh
      7 -> MO_S_Rem          <$> get bh
      8 -> MO_S_Neg          <$> get bh
      9 -> MO_U_MulMayOflo   <$> get bh
      10 -> MO_U_Quot         <$> get bh
      11 -> MO_U_Rem          <$> get bh
      12 -> MO_S_Ge           <$> get bh
      13 -> MO_S_Le           <$> get bh
      14 -> MO_S_Gt           <$> get bh
      15 -> MO_S_Lt           <$> get bh
      16 -> MO_U_Ge           <$> get bh
      17 -> MO_U_Le           <$> get bh
      18 -> MO_U_Gt           <$> get bh
      19 -> MO_U_Lt           <$> get bh
      20 -> MO_F_Add          <$> get bh
      21 -> MO_F_Sub          <$> get bh
      22 -> MO_F_Neg          <$> get bh
      23 -> MO_F_Mul          <$> get bh
      24 -> MO_F_Quot         <$> get bh
      25 -> MO_F_Eq           <$> get bh
      26 -> MO_F_Ne           <$> get bh
      27 -> MO_F_Ge           <$> get bh
      28 -> MO_F_Le           <$> get bh
      29 -> MO_F_Gt           <$> get bh
      30 -> MO_F_Lt           <$> get bh
      31 -> MO_And            <$> get bh
      32 -> MO_Or             <$> get bh
      33 -> MO_Xor            <$> get bh
      34 -> MO_Not            <$> get bh
      35 -> MO_Shl            <$> get bh
      36 -> MO_U_Shr          <$> get bh
      37 -> MO_S_Shr          <$> get bh
      38 -> MO_SF_Conv        <$> get bh <*> get bh
      39 -> MO_FS_Conv        <$> get bh <*> get bh
      40 -> MO_SS_Conv        <$> get bh <*> get bh
      41 -> MO_UU_Conv        <$> get bh <*> get bh
      42 -> MO_FF_Conv        <$> get bh <*> get bh
      43 -> MO_V_Insert       <$> get bh <*> get bh
      44 -> MO_V_Extract      <$> get bh <*> get bh
      45 -> MO_V_Add          <$> get bh <*> get bh
      46 -> MO_V_Sub          <$> get bh <*> get bh
      47 -> MO_V_Mul          <$> get bh <*> get bh
      48 -> MO_VS_Quot        <$> get bh <*> get bh
      49 -> MO_VS_Rem         <$> get bh <*> get bh
      50 -> MO_VS_Neg         <$> get bh <*> get bh
      51 -> MO_VU_Quot        <$> get bh <*> get bh
      52 -> MO_VU_Rem         <$> get bh <*> get bh
      53 -> MO_VF_Insert      <$> get bh <*> get bh
      54 -> MO_VF_Extract     <$> get bh <*> get bh
      55 -> MO_VF_Add         <$> get bh <*> get bh
      56 -> MO_VF_Sub         <$> get bh <*> get bh
      57 -> MO_VF_Neg         <$> get bh <*> get bh
      58 -> MO_VF_Mul         <$> get bh <*> get bh
      59 -> MO_VF_Quot        <$> get bh <*> get bh
      60 -> MO_AlignmentCheck <$> get bh <*> get bh
      _ -> fail "Binary.putMachOp: invalid tag"
  

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

{- |
Returns @Just w@ if the operation is an integer comparison with width
@w@, or @Nothing@ otherwise.
-}
maybeIntComparison :: MachOp -> Maybe Width
maybeIntComparison mop =
  case mop of
    MO_Eq   w  -> Just w
    MO_Ne   w  -> Just w
    MO_S_Ge w  -> Just w
    MO_S_Le w  -> Just w
    MO_S_Gt w  -> Just w
    MO_S_Lt w  -> Just w
    MO_U_Ge w  -> Just w
    MO_U_Le w  -> Just w
    MO_U_Gt w  -> Just w
    MO_U_Lt w  -> Just w
    _ -> Nothing

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

    MO_AlignmentCheck _ _ -> ty1
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

    MO_AlignmentCheck _ r -> [r]

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
  | MO_F64_Fabs
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
  | MO_F32_Fabs
  | MO_F32_Sqrt

  | MO_UF_Conv Width

  | MO_S_QuotRem Width
  | MO_U_QuotRem Width
  | MO_U_QuotRem2 Width
  | MO_Add2      Width
  | MO_AddWordC  Width
  | MO_SubWordC  Width
  | MO_AddIntC   Width
  | MO_SubIntC   Width
  | MO_U_Mul2    Width

  | MO_WriteBarrier
  | MO_Touch         -- Keep variables live (when using interior pointers)

  -- Prefetch
  | MO_Prefetch_Data Int -- Prefetch hint. May change program performance but not
                     -- program behavior.
                     -- the Int can be 0-3. Needs to be known at compile time
                     -- to interact with code generation correctly.
                     --  TODO: add support for prefetch WRITES,
                     --  currently only exposes prefetch reads, which
                     -- would the majority of use cases in ghc anyways


  -- These three MachOps are parameterised by the known alignment
  -- of the destination and source (for memcpy/memmove) pointers.
  -- This information may be used for optimisation in backends.
  | MO_Memcpy Int
  | MO_Memset Int
  | MO_Memmove Int
  | MO_Memcmp Int

  | MO_PopCnt Width
  | MO_Pdep Width
  | MO_Pext Width
  | MO_Clz Width
  | MO_Ctz Width

  | MO_BSwap Width

  -- Atomic read-modify-write.
  | MO_AtomicRMW Width AtomicMachOp
  | MO_AtomicRead Width
  | MO_AtomicWrite Width
  | MO_Cmpxchg Width
  deriving (Eq, Show)

instance Binary CallishMachOp where
  put_ bh MO_F64_Pwr             = putByte bh 0
  put_ bh MO_F64_Sin             = putByte bh 1
  put_ bh MO_F64_Cos             = putByte bh 2
  put_ bh MO_F64_Tan             = putByte bh 3
  put_ bh MO_F64_Sinh            = putByte bh 4
  put_ bh MO_F64_Cosh            = putByte bh 5
  put_ bh MO_F64_Tanh            = putByte bh 6
  put_ bh MO_F64_Asin            = putByte bh 7
  put_ bh MO_F64_Acos            = putByte bh 8
  put_ bh MO_F64_Atan            = putByte bh 9
  put_ bh MO_F64_Log             = putByte bh 10
  put_ bh MO_F64_Exp             = putByte bh 11
  put_ bh MO_F64_Fabs            = putByte bh 12
  put_ bh MO_F64_Sqrt            = putByte bh 13
  put_ bh MO_F32_Pwr             = putByte bh 14
  put_ bh MO_F32_Sin             = putByte bh 15
  put_ bh MO_F32_Cos             = putByte bh 16
  put_ bh MO_F32_Tan             = putByte bh 17
  put_ bh MO_F32_Sinh            = putByte bh 18
  put_ bh MO_F32_Cosh            = putByte bh 19
  put_ bh MO_F32_Tanh            = putByte bh 20
  put_ bh MO_F32_Asin            = putByte bh 21
  put_ bh MO_F32_Acos            = putByte bh 22
  put_ bh MO_F32_Atan            = putByte bh 23
  put_ bh MO_F32_Log             = putByte bh 24
  put_ bh MO_F32_Exp             = putByte bh 25
  put_ bh MO_F32_Fabs            = putByte bh 26
  put_ bh MO_F32_Sqrt            = putByte bh 27
  put_ bh (MO_UF_Conv       a)   = putByte bh 28 >> put_ bh a
  put_ bh (MO_S_QuotRem     a)   = putByte bh 29 >> put_ bh a
  put_ bh (MO_U_QuotRem     a)   = putByte bh 30 >> put_ bh a
  put_ bh (MO_U_QuotRem2    a)   = putByte bh 31 >> put_ bh a
  put_ bh (MO_Add2          a)   = putByte bh 32 >> put_ bh a
  put_ bh (MO_AddWordC      a)   = putByte bh 33 >> put_ bh a
  put_ bh (MO_SubWordC      a)   = putByte bh 34 >> put_ bh a
  put_ bh (MO_AddIntC       a)   = putByte bh 35 >> put_ bh a
  put_ bh (MO_SubIntC       a)   = putByte bh 36 >> put_ bh a
  put_ bh (MO_U_Mul2        a)   = putByte bh 37 >> put_ bh a
  put_ bh MO_WriteBarrier        = putByte bh 38
  put_ bh MO_Touch               = putByte bh 39
  put_ bh (MO_Prefetch_Data a)   = putByte bh 40 >> put_ bh a
  put_ bh (MO_Memcpy        a)   = putByte bh 41 >> put_ bh a
  put_ bh (MO_Memset        a)   = putByte bh 42 >> put_ bh a
  put_ bh (MO_Memmove       a)   = putByte bh 43 >> put_ bh a
  put_ bh (MO_Memcmp        a)   = putByte bh 44 >> put_ bh a
  put_ bh (MO_PopCnt        a)   = putByte bh 45 >> put_ bh a
  put_ bh (MO_Pdep          a)   = putByte bh 46 >> put_ bh a
  put_ bh (MO_Pext          a)   = putByte bh 47 >> put_ bh a
  put_ bh (MO_Clz           a)   = putByte bh 48 >> put_ bh a
  put_ bh (MO_Ctz           a)   = putByte bh 49 >> put_ bh a
  put_ bh (MO_BSwap         a)   = putByte bh 50 >> put_ bh a
  put_ bh (MO_AtomicRMW     a b) = putByte bh 51 >> put_ bh a >> put_ bh b
  put_ bh (MO_AtomicRead    a)   = putByte bh 52 >> put_ bh a
  put_ bh (MO_AtomicWrite   a)   = putByte bh 53 >> put_ bh a
  put_ bh (MO_Cmpxchg       a)   = putByte bh 54 >> put_ bh a

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure MO_F64_Pwr
      1 -> pure MO_F64_Sin
      2 -> pure MO_F64_Cos
      3 -> pure MO_F64_Tan
      4 -> pure MO_F64_Sinh
      5 -> pure MO_F64_Cosh
      6 -> pure MO_F64_Tanh
      7 -> pure MO_F64_Asin
      8 -> pure MO_F64_Acos
      9 -> pure MO_F64_Atan
      10 -> pure MO_F64_Log
      11 -> pure MO_F64_Exp
      12 -> pure MO_F64_Fabs
      13 -> pure MO_F64_Sqrt
      14 -> pure MO_F32_Pwr
      15 -> pure MO_F32_Sin
      16 -> pure MO_F32_Cos
      17 -> pure MO_F32_Tan
      18 -> pure MO_F32_Sinh
      19 -> pure MO_F32_Cosh
      20 -> pure MO_F32_Tanh
      21 -> pure MO_F32_Asin
      22 -> pure MO_F32_Acos
      23 -> pure MO_F32_Atan
      24 -> pure MO_F32_Log
      25 -> pure MO_F32_Exp
      26 -> pure MO_F32_Fabs
      27 -> pure MO_F32_Sqrt
      28 -> MO_UF_Conv       <$> get bh
      29 -> MO_S_QuotRem     <$> get bh
      30 -> MO_U_QuotRem     <$> get bh
      31 -> MO_U_QuotRem2    <$> get bh
      32 -> MO_Add2          <$> get bh
      33 -> MO_AddWordC      <$> get bh
      34 -> MO_SubWordC      <$> get bh
      35 -> MO_AddIntC       <$> get bh
      36 -> MO_SubIntC       <$> get bh
      37 -> MO_U_Mul2        <$> get bh
      38 -> pure MO_WriteBarrier
      39 -> pure MO_Touch
      40 -> MO_Prefetch_Data <$> get bh
      41 -> MO_Memcpy        <$> get bh
      42 -> MO_Memset        <$> get bh
      43 -> MO_Memmove       <$> get bh
      44 -> MO_Memcmp        <$> get bh
      45 -> MO_PopCnt        <$> get bh
      46 -> MO_Pdep          <$> get bh
      47 -> MO_Pext          <$> get bh
      48 -> MO_Clz           <$> get bh
      49 -> MO_Ctz           <$> get bh
      50 -> MO_BSwap         <$> get bh
      51 -> MO_AtomicRMW     <$> get bh <*> get bh
      52 -> MO_AtomicRead    <$> get bh
      53 -> MO_AtomicWrite   <$> get bh
      54 -> MO_Cmpxchg       <$> get bh
      _  -> fail "Binary.putCallishMachOp: invalid tag"


-- | The operation to perform atomically.
data AtomicMachOp =
      AMO_Add
    | AMO_Sub
    | AMO_And
    | AMO_Nand
    | AMO_Or
    | AMO_Xor
      deriving (Eq, Show, Enum)

instance Binary AtomicMachOp where
  put_ bh = putByte bh . fromIntegral . fromEnum
  get bh = toEnum . fromIntegral <$> getByte bh
 

pprCallishMachOp :: CallishMachOp -> SDoc
pprCallishMachOp mo = text (show mo)

callishMachOpHints :: CallishMachOp -> ([ForeignHint], [ForeignHint])
callishMachOpHints op = case op of
  MO_Memcpy _  -> ([], [AddrHint,AddrHint,NoHint])
  MO_Memset _  -> ([], [AddrHint,NoHint,NoHint])
  MO_Memmove _ -> ([], [AddrHint,AddrHint,NoHint])
  MO_Memcmp _  -> ([], [AddrHint, AddrHint, NoHint])
  _            -> ([],[])
  -- empty lists indicate NoHint

-- | The alignment of a 'memcpy'-ish operation.
machOpMemcpyishAlign :: CallishMachOp -> Maybe Int
machOpMemcpyishAlign op = case op of
  MO_Memcpy  align -> Just align
  MO_Memset  align -> Just align
  MO_Memmove align -> Just align
  MO_Memcmp  align -> Just align
  _                -> Nothing
