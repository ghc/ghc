-- | Formats on this architecture
--      A Format is a combination of width and class
--
--      TODO:   Signed vs unsigned?
--
--      TODO:   This module is currenly shared by all architectures because
--              NCGMonad need to know about it to make a VReg. It would be better
--              to have architecture specific formats, and do the overloading
--              properly. eg SPARC doesn't care about FF80.
--
module Format (
    Format(..),
    intFormat,
    floatFormat,
    isFloatFormat,
    cmmTypeFormat,
    formatToWidth,
    formatInBytes
)

where

import GhcPrelude

import Cmm
import Outputable

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
        | FF80
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


-- | Get the float format of this width.
floatFormat :: Width -> Format
floatFormat width
 = case width of
        W32     -> FF32
        W64     -> FF64
        W80     -> FF80
        other   -> pprPanic "Format.floatFormat" (ppr other)


-- | Check if a format represents a floating point value.
isFloatFormat :: Format -> Bool
isFloatFormat format
 = case format of
        FF32    -> True
        FF64    -> True
        FF80    -> True
        _       -> False


-- | Convert a Cmm type to a Format.
cmmTypeFormat :: CmmType -> Format
cmmTypeFormat ty
        | isFloatType ty        = floatFormat (typeWidth ty)
        | otherwise             = intFormat (typeWidth ty)


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
        FF80            -> W80

formatInBytes :: Format -> Int
formatInBytes = widthInBytes . formatToWidth
