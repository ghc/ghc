-- | Formats on this architecture
--      A Format is a combination of width and class
--
--      TODO:   Signed vs unsigned?
--
--      TODO:   This module is currently shared by all architectures because
--              NCGMonad need to know about it to make a VReg. It would be better
--              to have architecture specific formats, and do the overloading
--              properly. eg SPARC doesn't care about FF80.
--
module GHC.CmmToAsm.Format (
    Format(..),
    ScalarFormat(..),
    intFormat,
    floatFormat,
    isIntFormat,
    isFloatFormat,
    isVecFormat,
    cmmTypeFormat,
    formatToWidth,
    formatInBytes
)

where

import GHC.Prelude

import GHC.Cmm
import GHC.Utils.Outputable
import GHC.Utils.Panic

{- Note [GHC's data format representations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has severals types that represent various aspects of data format.
These include:

 * 'CmmType.CmmType': The data classification used throughout the C--
   pipeline. This is a pair of a CmmCat and a Width.

 * 'CmmType.CmmCat': What the bits in a C-- value mean (e.g. a pointer, integer, or floating-point value)

 * 'CmmType.Width': The width of a C-- value.

 * 'CmmType.Length': The width (measured in number of scalars) of a vector value.

 * 'Format.Format': The data format representation used by much of the backend.

 * 'Format.ScalarFormat': The format of a 'Format.VecFormat'\'s scalar.

 * 'RegClass.RegClass': Whether a register is an integer, float-point, or vector register
-}

-- It looks very like the old MachRep, but it's now of purely local
-- significance, here in the native code generator.  You can change it
-- without global consequences.
--
-- A major use is as an opcode qualifier; thus the opcode
--      mov.l a b
-- might be encoded
--      MOV II32 a b
-- where the Format field encodes the ".l" part.

-- ToDo: it's not clear to me that we need separate signed-vs-unsigned formats
--        here.  I've removed them from the x86 version, we'll see what happens --SDM

-- ToDo: quite a few occurrences of Format could usefully be replaced by Width

data Format
        = II8
        | II16
        | II32
        | II64
        | FF32
        | FF64
        | VecFormat !Length !ScalarFormat !Width
        deriving (Show, Eq)

data ScalarFormat = FmtInt8
                  | FmtInt16
                  | FmtInt32
                  | FmtInt64
                  | FmtFloat
                  | FmtDouble
                  deriving (Show, Eq)

-- | Get the integer format of this width.
intFormat :: Width -> Format
intFormat width
 = case width of
        W8      -> II8
        W16     -> II16
        W32     -> II32
        W64     -> II64
        other   -> sorry $ "The native code generator cannot " ++
            "produce code for Format.intFormat " ++ show other
            ++ "\n\tConsider using the llvm backend with -fllvm"

-- | Check if a format represents a vector
isVecFormat :: Format -> Bool
isVecFormat (VecFormat {}) = True
isVecFormat _              = False

-- | Get the float format of this width.
floatFormat :: Width -> Format
floatFormat width
 = case width of
        W32     -> FF32
        W64     -> FF64

        other   -> pprPanic "Format.floatFormat" (ppr other)

-- | Check if a format represent an integer value.
isIntFormat :: Format -> Bool
isIntFormat = not . isFloatFormat

-- | Check if a format represents a floating point value.
isFloatFormat :: Format -> Bool
isFloatFormat format
 = case format of
        FF32    -> True
        FF64    -> True
        _       -> False


-- | Convert a Cmm type to a Format.
cmmTypeFormat :: CmmType -> Format
cmmTypeFormat ty
        | isFloatType ty        = floatFormat (typeWidth ty)
        | isVecType ty          = vecFormat ty
        | otherwise             = intFormat (typeWidth ty)

vecFormat :: CmmType -> Format
vecFormat ty =
  let l      = vecLength ty
      elemTy = vecElemType ty
   in if isFloatType elemTy
      then case typeWidth elemTy of
             W32 -> VecFormat l FmtFloat  W32
             W64 -> VecFormat l FmtDouble W64
             _   -> pprPanic "Incorrect vector element width" (ppr elemTy)
      else case typeWidth elemTy of
             W8  -> VecFormat l FmtInt8  W8
             W16 -> VecFormat l FmtInt16 W16
             W32 -> VecFormat l FmtInt32 W32
             W64 -> VecFormat l FmtInt64 W64
             _   -> pprPanic "Incorrect vector element width" (ppr elemTy)

-- | Get the Width of a Format.
formatToWidth :: Format -> Width
formatToWidth format
 = case format of
        II8             -> W8
        II16            -> W16
        II32            -> W32
        II64            -> W64
        FF32            -> W32
        FF64            -> W64
        VecFormat l _ w -> widthFromBytes (l*widthInBytes w)

formatInBytes :: Format -> Int
formatInBytes = widthInBytes . formatToWidth
