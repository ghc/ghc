{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Sizes on this architecture
-- 	A Size is a combination of width and class
-- 
-- 	TODO: 	Rename this to "Format" instead of "Size" to reflect
--		the fact that it represents floating point vs integer.
--
--	TODO: 	Signed vs unsigned?
--
--	TODO:	This module is currenly shared by all architectures because
--		NCGMonad need to know about it to make a VReg. It would be better
--		to have architecture specific formats, and do the overloading
--		properly. eg SPARC doesn't care about FF80.
--
module Size (
    Size(..),
    intSize,
    floatSize,
    isFloatSize,
    cmmTypeSize,
    sizeToWidth,
    sizeInBytes
)

where

import Cmm
import Outputable

-- It looks very like the old MachRep, but it's now of purely local
-- significance, here in the native code generator.  You can change it
-- without global consequences.
--
-- A major use is as an opcode qualifier; thus the opcode 
--	mov.l a b
-- might be encoded 
-- 	MOV II32 a b
-- where the Size field encodes the ".l" part.

-- ToDo: it's not clear to me that we need separate signed-vs-unsigned sizes
-- 	  here.  I've removed them from the x86 version, we'll see what happens --SDM

-- ToDo: quite a few occurrences of Size could usefully be replaced by Width

data Size
	= II8 
	| II16 
	| II32 
	| II64 
	| FF32 
	| FF64 
	| FF80
	deriving (Show, Eq)


-- | Get the integer size of this width.
intSize :: Width -> Size
intSize width
 = case width of
 	W8 	-> II8
	W16	-> II16
	W32	-> II32
	W64 	-> II64
	other	-> pprPanic "Size.intSize" (ppr other)


-- | Get the float size of this width.
floatSize :: Width -> Size
floatSize width
 = case width of
 	W32	-> FF32
	W64	-> FF64
	other 	-> pprPanic "Size.floatSize" (ppr other)


-- | Check if a size represents a floating point value.
isFloatSize :: Size -> Bool
isFloatSize size
 = case size of
 	FF32	-> True
	FF64	-> True
	FF80	-> True
	_	-> False


-- | Convert a Cmm type to a Size.
cmmTypeSize :: CmmType -> Size
cmmTypeSize ty 
	| isFloatType ty	= floatSize (typeWidth ty)
	| otherwise		= intSize (typeWidth ty)


-- | Get the Width of a Size.
sizeToWidth :: Size -> Width
sizeToWidth size
 = case size of
 	II8		-> W8
	II16		-> W16
	II32		-> W32
	II64		-> W64
	FF32		-> W32
	FF64		-> W64
	FF80		-> W80

sizeInBytes :: Size -> Int
sizeInBytes = widthInBytes . sizeToWidth
