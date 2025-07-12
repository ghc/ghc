{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
    Format(.., IntegerFormat),
    ScalarFormat(..),
    intFormat,
    floatFormat,
    isIntFormat,
    isIntScalarFormat,
    intScalarFormat,
    isFloatFormat,
    vecFormat,
    floatVecFormat,
    intVecFormat,
    isVecFormat,
    cmmTypeFormat,
    formatToWidth,
    scalarWidth,
    formatInBytes,
    isFloatScalarFormat,
    isFloatOrFloatVecFormat,
    floatScalarFormat,
    scalarFormatFormat,
    VirtualRegWithFormat(..),
    RegWithFormat(..),
    takeVirtualRegs,
    takeRealRegs,
)

where

import GHC.Prelude

import GHC.Cmm
import GHC.Platform.Reg ( Reg(..), RealReg, VirtualReg )
import GHC.Types.Unique ( Uniquable(..) )
import GHC.Types.Unique.Set
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

 * 'RegClass.RegClass': Whether a register is an integer or a floating point/vector register.
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
        | VecFormat !Length       -- ^ number of elements (always at least 2)
                    !ScalarFormat -- ^ format of each element
        deriving (Show, Eq, Ord)

pattern IntegerFormat :: Format
pattern IntegerFormat <- ( isIntegerFormat -> True )
{-# COMPLETE IntegerFormat, FF32, FF64, VecFormat #-}

isIntegerFormat :: Format -> Bool
isIntegerFormat = \case
  II8  -> True
  II16 -> True
  II32 -> True
  II64 -> True
  _    -> False


instance Outputable Format where
  ppr fmt = text (show fmt)

data ScalarFormat
  = FmtInt8
  | FmtInt16
  | FmtInt32
  | FmtInt64
  | FmtFloat
  | FmtDouble
  deriving (Show, Eq, Ord)

scalarFormatFormat :: ScalarFormat -> Format
scalarFormatFormat = \case
  FmtInt8 -> II8
  FmtInt16 -> II16
  FmtInt32 -> II32
  FmtInt64 -> II64
  FmtFloat -> FF32
  FmtDouble -> FF64

isFloatScalarFormat :: ScalarFormat -> Bool
isFloatScalarFormat = \case
  FmtFloat -> True
  FmtDouble -> True
  _ -> False

isFloatOrFloatVecFormat :: Format -> Bool
isFloatOrFloatVecFormat = \case
  VecFormat _ sFmt -> isFloatScalarFormat sFmt
  fmt -> isFloatFormat fmt

floatScalarFormat :: Width -> ScalarFormat
floatScalarFormat W32 = FmtFloat
floatScalarFormat W64 = FmtDouble
floatScalarFormat w = pprPanic "floatScalarFormat" (ppr w)

isIntScalarFormat :: ScalarFormat -> Bool
isIntScalarFormat = not . isFloatScalarFormat

intScalarFormat :: Width -> ScalarFormat
intScalarFormat = \case
  W8  -> FmtInt8
  W16 -> FmtInt16
  W32 -> FmtInt32
  W64 -> FmtInt64
  w   -> pprPanic "intScalarFormat" (ppr w)

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

-- | Check if a format represent an integer value.
isIntFormat :: Format -> Bool
isIntFormat format =
  case format of
    II8  -> True
    II16 -> True
    II32 -> True
    II64 -> True
    _    -> False

-- | Get the float format of this width.
floatFormat :: Width -> Format
floatFormat width
 = case width of
        W32     -> FF32
        W64     -> FF64
        other   -> pprPanic "Format.floatFormat" (ppr other)

-- | Check if a format represents a floating point value.
isFloatFormat :: Format -> Bool
isFloatFormat format
 = case format of
        FF32    -> True
        FF64    -> True
        _       -> False

vecFormat :: CmmType -> Format
vecFormat ty =
  let l      = vecLength ty
      elemTy = vecElemType ty
   in if isFloatType elemTy
      then case typeWidth elemTy of
             W32 -> VecFormat l FmtFloat
             W64 -> VecFormat l FmtDouble
             _   -> pprPanic "Incorrect vector element width" (ppr elemTy)
      else case typeWidth elemTy of
             W8  -> VecFormat l FmtInt8
             W16 -> VecFormat l FmtInt16
             W32 -> VecFormat l FmtInt32
             W64 -> VecFormat l FmtInt64
             _   -> pprPanic "Incorrect vector element width" (ppr elemTy)

floatVecFormat :: Int -> Width -> Format
floatVecFormat length = vecFormat . cmmVec length . cmmFloat

intVecFormat :: Int -> Width -> Format
intVecFormat length = vecFormat . cmmVec length . cmmBits

-- | Check if a format represents a vector
isVecFormat :: Format -> Bool
isVecFormat (VecFormat {}) = True
isVecFormat _              = False


-- | Convert a Cmm type to a Format.
cmmTypeFormat :: CmmType -> Format
cmmTypeFormat ty
        | isFloatType ty        = floatFormat (typeWidth ty)
        | isVecType ty          = vecFormat ty
        | otherwise             = intFormat (typeWidth ty)


-- | Get the Width of a Format.
formatToWidth :: Format -> Width
formatToWidth format
 = case format of
        II8  -> W8
        II16 -> W16
        II32 -> W32
        II64 -> W64
        FF32 -> W32
        FF64 -> W64
        VecFormat l s ->
          widthFromBytes (l * widthInBytes (scalarWidth s))

scalarWidth :: ScalarFormat -> Width
scalarWidth = \case
  FmtInt8   -> W8
  FmtInt16  -> W16
  FmtInt32  -> W32
  FmtInt64  -> W64
  FmtFloat  -> W32
  FmtDouble -> W64

formatInBytes :: Format -> Int
formatInBytes = widthInBytes . formatToWidth

--------------------------------------------------------------------------------

-- | A typed virtual register: a virtual register, together with the specific
-- format we are using it at.
data VirtualRegWithFormat
    = VirtualRegWithFormat
    { virtualRegWithFormat_reg :: {-# UNPACK #-} !VirtualReg
    , virtualRegWithFormat_format :: !Format
    }

-- | A typed register: a register, together with the specific format we
-- are using it at.
data RegWithFormat
    = RegWithFormat
    { regWithFormat_reg :: {-# UNPACK #-} !Reg
    , regWithFormat_format :: !Format
    }

instance Show RegWithFormat where
  show (RegWithFormat reg fmt) = show reg ++ "::" ++ show fmt

instance Uniquable RegWithFormat where
  getUnique = getUnique . regWithFormat_reg

instance Outputable VirtualRegWithFormat where
  ppr (VirtualRegWithFormat reg fmt) = ppr reg <+> dcolon <+> ppr fmt

instance Outputable RegWithFormat where
  ppr (RegWithFormat reg fmt) = ppr reg <+> dcolon <+> ppr fmt

-- | Take all the virtual registers from this set.
takeVirtualRegs :: UniqSet RegWithFormat -> UniqSet VirtualReg
takeVirtualRegs = mapMaybeUniqSet_sameUnique $
  \ case { RegWithFormat { regWithFormat_reg = RegVirtual vr } -> Just vr; _ -> Nothing }
  -- See Note [Unique Determinism and code generation]

-- | Take all the real registers from this set.
takeRealRegs :: UniqSet RegWithFormat -> UniqSet RealReg
takeRealRegs = mapMaybeUniqSet_sameUnique $
  \ case { RegWithFormat { regWithFormat_reg = RegReal rr } -> Just rr; _ -> Nothing }
  -- See Note [Unique Determinism and code generation]
