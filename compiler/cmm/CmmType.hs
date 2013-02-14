
module CmmType
    ( CmmType   -- Abstract
    , b8, b16, b32, b64, b128, b256, f32, f64, bWord, bHalfWord, gcWord
    , cInt, cLong
    , cmmBits, cmmFloat
    , typeWidth, cmmEqType, cmmEqType_ignoring_ptrhood
    , isFloatType, isGcPtrType, isWord32, isWord64, isFloat64, isFloat32

    , Width(..)
    , widthInBits, widthInBytes, widthInLog, widthFromBytes
    , wordWidth, halfWordWidth, cIntWidth, cLongWidth
    , halfWordMask
    , narrowU, narrowS
    , rEP_CostCentreStack_mem_alloc
    , rEP_CostCentreStack_scc_count
    , rEP_StgEntCounter_allocs
    , rEP_StgEntCounter_allocd

    , ForeignHint(..)

    , Length
    , vec, vec2, vec4, vec8, vec16
    , vec2f64, vec2b64, vec4f32, vec4b32, vec8b16, vec16b8
    , cmmVec
    , vecLength, vecElemType
    , isVecType
   )
where

#include "HsVersions.h"

import DynFlags
import FastString
import Outputable

import Data.Word
import Data.Int

-----------------------------------------------------------------------------
--              CmmType
-----------------------------------------------------------------------------

  -- NOTE: CmmType is an abstract type, not exported from this
  --       module so you can easily change its representation
  --
  -- However Width is exported in a concrete way,
  -- and is used extensively in pattern-matching

data CmmType    -- The important one!
  = CmmType CmmCat Width

data CmmCat                -- "Category" (not exported)
   = GcPtrCat              -- GC pointer
   | BitsCat               -- Non-pointer
   | FloatCat              -- Float
   | VecCat Length CmmCat  -- Vector
   deriving( Eq )
        -- See Note [Signed vs unsigned] at the end

instance Outputable CmmType where
  ppr (CmmType cat wid) = ppr cat <> ppr (widthInBits wid)

instance Outputable CmmCat where
  ppr FloatCat       = ptext $ sLit("F")
  ppr GcPtrCat       = ptext $ sLit("P")
  ppr BitsCat        = ptext $ sLit("I")
  ppr (VecCat n cat) = ppr cat <> text "x" <> ppr n <> text "V"

-- Why is CmmType stratified?  For native code generation,
-- most of the time you just want to know what sort of register
-- to put the thing in, and for this you need to know how
-- many bits thing has and whether it goes in a floating-point
-- register.  By contrast, the distinction between GcPtr and
-- GcNonPtr is of interest to only a few parts of the code generator.

-------- Equality on CmmType --------------
-- CmmType is *not* an instance of Eq; sometimes we care about the
-- Gc/NonGc distinction, and sometimes we don't
-- So we use an explicit function to force you to think about it
cmmEqType :: CmmType -> CmmType -> Bool -- Exact equality
cmmEqType (CmmType c1 w1) (CmmType c2 w2) = c1==c2 && w1==w2

cmmEqType_ignoring_ptrhood :: CmmType -> CmmType -> Bool
  -- This equality is temporary; used in CmmLint
  -- but the RTS files are not yet well-typed wrt pointers
cmmEqType_ignoring_ptrhood (CmmType c1 w1) (CmmType c2 w2)
   = c1 `weak_eq` c2 && w1==w2
   where
     weak_eq :: CmmCat -> CmmCat -> Bool
     FloatCat         `weak_eq` FloatCat         = True
     FloatCat         `weak_eq` _other           = False
     _other           `weak_eq` FloatCat         = False
     (VecCat l1 cat1) `weak_eq` (VecCat l2 cat2) = l1 == l2
                                                   && cat1 `weak_eq` cat2
     (VecCat {})      `weak_eq` _other           = False
     _other           `weak_eq` (VecCat {})      = False
     _word1           `weak_eq` _word2           = True        -- Ignores GcPtr

--- Simple operations on CmmType -----
typeWidth :: CmmType -> Width
typeWidth (CmmType _ w) = w

cmmBits, cmmFloat :: Width -> CmmType
cmmBits  = CmmType BitsCat
cmmFloat = CmmType FloatCat

-------- Common CmmTypes ------------
-- Floats and words of specific widths
b8, b16, b32, b64, b128, b256, f32, f64 :: CmmType
b8     = cmmBits W8
b16    = cmmBits W16
b32    = cmmBits W32
b64    = cmmBits W64
b128   = cmmBits W128
b256   = cmmBits W256
f32    = cmmFloat W32
f64    = cmmFloat W64

-- CmmTypes of native word widths
bWord :: DynFlags -> CmmType
bWord dflags = cmmBits (wordWidth dflags)

bHalfWord :: DynFlags -> CmmType
bHalfWord dflags = cmmBits (halfWordWidth dflags)

gcWord :: DynFlags -> CmmType
gcWord dflags = CmmType GcPtrCat (wordWidth dflags)

cInt, cLong :: DynFlags -> CmmType
cInt  dflags = cmmBits (cIntWidth  dflags)
cLong dflags = cmmBits (cLongWidth dflags)


------------ Predicates ----------------
isFloatType, isGcPtrType :: CmmType -> Bool
isFloatType (CmmType FloatCat    _) = True
isFloatType _other                  = False

isGcPtrType (CmmType GcPtrCat _) = True
isGcPtrType _other               = False

isWord32, isWord64, isFloat32, isFloat64 :: CmmType -> Bool
-- isWord64 is true of 64-bit non-floats (both gc-ptrs and otherwise)
-- isFloat32 and 64 are obvious

isWord64 (CmmType BitsCat  W64) = True
isWord64 (CmmType GcPtrCat W64) = True
isWord64 _other                 = False

isWord32 (CmmType BitsCat  W32) = True
isWord32 (CmmType GcPtrCat W32) = True
isWord32 _other                 = False

isFloat32 (CmmType FloatCat W32) = True
isFloat32 _other                 = False

isFloat64 (CmmType FloatCat W64) = True
isFloat64 _other                 = False

-----------------------------------------------------------------------------
--              Width
-----------------------------------------------------------------------------

data Width   = W8 | W16 | W32 | W64
             | W80      -- Extended double-precision float,
                        -- used in x86 native codegen only.
                        -- (we use Ord, so it'd better be in this order)
             | W128
             | W256
             deriving (Eq, Ord, Show)

instance Outputable Width where
   ppr rep = ptext (mrStr rep)

mrStr :: Width -> LitString
mrStr W8   = sLit("W8")
mrStr W16  = sLit("W16")
mrStr W32  = sLit("W32")
mrStr W64  = sLit("W64")
mrStr W128 = sLit("W128")
mrStr W256 = sLit("W256")
mrStr W80  = sLit("W80")


-------- Common Widths  ------------
wordWidth :: DynFlags -> Width
wordWidth dflags
 | wORD_SIZE dflags == 4 = W32
 | wORD_SIZE dflags == 8 = W64
 | otherwise             = panic "MachOp.wordRep: Unknown word size"

halfWordWidth :: DynFlags -> Width
halfWordWidth dflags
 | wORD_SIZE dflags == 4 = W16
 | wORD_SIZE dflags == 8 = W32
 | otherwise             = panic "MachOp.halfWordRep: Unknown word size"

halfWordMask :: DynFlags -> Integer
halfWordMask dflags
 | wORD_SIZE dflags == 4 = 0xFFFF
 | wORD_SIZE dflags == 8 = 0xFFFFFFFF
 | otherwise             = panic "MachOp.halfWordMask: Unknown word size"

-- cIntRep is the Width for a C-language 'int'
cIntWidth, cLongWidth :: DynFlags -> Width
cIntWidth dflags = case cINT_SIZE dflags of
                   4 -> W32
                   8 -> W64
                   s -> panic ("cIntWidth: Unknown cINT_SIZE: " ++ show s)
cLongWidth dflags = case cLONG_SIZE dflags of
                    4 -> W32
                    8 -> W64
                    s -> panic ("cIntWidth: Unknown cLONG_SIZE: " ++ show s)

widthInBits :: Width -> Int
widthInBits W8   = 8
widthInBits W16  = 16
widthInBits W32  = 32
widthInBits W64  = 64
widthInBits W128 = 128
widthInBits W256 = 256
widthInBits W80  = 80

widthInBytes :: Width -> Int
widthInBytes W8   = 1
widthInBytes W16  = 2
widthInBytes W32  = 4
widthInBytes W64  = 8
widthInBytes W128 = 16
widthInBytes W256 = 32
widthInBytes W80  = 10

widthFromBytes :: Int -> Width
widthFromBytes 1  = W8
widthFromBytes 2  = W16
widthFromBytes 4  = W32
widthFromBytes 8  = W64
widthFromBytes 16 = W128
widthFromBytes 32 = W256
widthFromBytes 10 = W80
widthFromBytes n  = pprPanic "no width for given number of bytes" (ppr n)

-- log_2 of the width in bytes, useful for generating shifts.
widthInLog :: Width -> Int
widthInLog W8   = 0
widthInLog W16  = 1
widthInLog W32  = 2
widthInLog W64  = 3
widthInLog W128 = 4
widthInLog W256 = 5
widthInLog W80  = panic "widthInLog: F80"

-- widening / narrowing

narrowU :: Width -> Integer -> Integer
narrowU W8  x = fromIntegral (fromIntegral x :: Word8)
narrowU W16 x = fromIntegral (fromIntegral x :: Word16)
narrowU W32 x = fromIntegral (fromIntegral x :: Word32)
narrowU W64 x = fromIntegral (fromIntegral x :: Word64)
narrowU _ _ = panic "narrowTo"

narrowS :: Width -> Integer -> Integer
narrowS W8  x = fromIntegral (fromIntegral x :: Int8)
narrowS W16 x = fromIntegral (fromIntegral x :: Int16)
narrowS W32 x = fromIntegral (fromIntegral x :: Int32)
narrowS W64 x = fromIntegral (fromIntegral x :: Int64)
narrowS _ _ = panic "narrowTo"

-----------------------------------------------------------------------------
--              SIMD
-----------------------------------------------------------------------------

type Length = Int

vec :: Length -> CmmType -> CmmType
vec l (CmmType cat w) = CmmType (VecCat l cat) vecw
  where
    vecw :: Width
    vecw = widthFromBytes (l*widthInBytes w)

vec2, vec4, vec8, vec16 :: CmmType -> CmmType
vec2  = vec 2
vec4  = vec 4
vec8  = vec 8
vec16 = vec 16

vec2f64, vec2b64, vec4f32, vec4b32, vec8b16, vec16b8 :: CmmType
vec2f64 = vec 2 f64
vec2b64 = vec 2 b64
vec4f32 = vec 4 f32
vec4b32 = vec 4 b32
vec8b16 = vec 8 b16
vec16b8 = vec 16 b8

cmmVec :: Int -> CmmType -> CmmType
cmmVec n (CmmType cat w) =
    CmmType (VecCat n cat) (widthFromBytes (n*widthInBytes w))

vecLength :: CmmType -> Length
vecLength (CmmType (VecCat l _) _) = l
vecLength _                        = panic "vecLength: not a vector"

vecElemType :: CmmType -> CmmType
vecElemType (CmmType (VecCat l cat) w) = CmmType cat scalw
  where
    scalw :: Width
    scalw = widthFromBytes (widthInBytes w `div` l)
vecElemType _ = panic "vecElemType: not a vector"

isVecType :: CmmType -> Bool
isVecType (CmmType (VecCat {}) _) = True
isVecType _                       = False

-------------------------------------------------------------------------
-- Hints

-- Hints are extra type information we attach to the arguments and
-- results of a foreign call, where more type information is sometimes
-- needed by the ABI to make the correct kind of call.

data ForeignHint
  = NoHint | AddrHint | SignedHint
  deriving( Eq )
        -- Used to give extra per-argument or per-result
        -- information needed by foreign calling conventions

-------------------------------------------------------------------------

-- These don't really belong here, but I don't know where is best to
-- put them.

rEP_CostCentreStack_mem_alloc :: DynFlags -> CmmType
rEP_CostCentreStack_mem_alloc dflags
    = cmmBits (widthFromBytes (pc_REP_CostCentreStack_mem_alloc pc))
    where pc = sPlatformConstants (settings dflags)

rEP_CostCentreStack_scc_count :: DynFlags -> CmmType
rEP_CostCentreStack_scc_count dflags
    = cmmBits (widthFromBytes (pc_REP_CostCentreStack_scc_count pc))
    where pc = sPlatformConstants (settings dflags)

rEP_StgEntCounter_allocs :: DynFlags -> CmmType
rEP_StgEntCounter_allocs dflags
    = cmmBits (widthFromBytes (pc_REP_StgEntCounter_allocs pc))
    where pc = sPlatformConstants (settings dflags)

rEP_StgEntCounter_allocd :: DynFlags -> CmmType
rEP_StgEntCounter_allocd dflags
    = cmmBits (widthFromBytes (pc_REP_StgEntCounter_allocd pc))
    where pc = sPlatformConstants (settings dflags)

-------------------------------------------------------------------------
{-      Note [Signed vs unsigned]
        ~~~~~~~~~~~~~~~~~~~~~~~~~
Should a CmmType include a signed vs. unsigned distinction?

This is very much like a "hint" in C-- terminology: it isn't necessary
in order to generate correct code, but it might be useful in that the
compiler can generate better code if it has access to higher-level
hints about data.  This is important at call boundaries, because the
definition of a function is not visible at all of its call sites, so
the compiler cannot infer the hints.

Here in Cmm, we're taking a slightly different approach.  We include
the int vs. float hint in the CmmType, because (a) the majority of
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

If signed/unsigned hints are missing from CmmType, then the only
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

Pros for adding signed vs. unsigned to CmmType:

  - It would let us use convention (b) above, and get easier
    code generation for extending loads.

  - Less information required on foreign calls.

  - MachOp type would be simpler

Cons:

  - More complexity

  - What is the CmmType for a VanillaReg?  Currently it is
    always wordRep, but now we have to decide whether it is
    signed or unsigned.  The same VanillaReg can thus have
    different CmmType in different parts of the program.

  - Extra coercions cluttering up expressions.

Currently for GHC, the foreign call point is moot, because we do our
own promotion of sub-word-sized values to word-sized values.  The Int8
type is represnted by an Int# which is kept sign-extended at all times
(this is slightly naughty, because we're making assumptions about the
C calling convention rather early on in the compiler).  However, given
this, the cons outweigh the pros.

-}

