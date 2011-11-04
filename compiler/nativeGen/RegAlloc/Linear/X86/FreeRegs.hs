
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Free regs map for i386 and x86_64
module RegAlloc.Linear.X86.FreeRegs
where

import X86.Regs
import RegClass
import Reg
import Panic

import Data.Word
import Data.Bits

type FreeRegs 
#ifdef i386_TARGET_ARCH
	= Word32
#else
	= Word64
#endif

noFreeRegs :: FreeRegs
noFreeRegs = 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) f 
	= f .|. (1 `shiftL` n)

releaseReg _ _	
	= panic "RegAlloc.Linear.X86.FreeRegs.realeaseReg: no reg"

initFreeRegs :: FreeRegs
initFreeRegs 
	= foldr releaseReg noFreeRegs allocatableRegs

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]	-- lazilly
getFreeRegs cls f = go f 0

  where go 0 _ = []
        go n m 
	  | n .&. 1 /= 0 && classOfRealReg (RealRegSingle m) == cls
	  = RealRegSingle m : (go (n `shiftR` 1) $! (m+1))

	  | otherwise
	  = go (n `shiftR` 1) $! (m+1)
	-- ToDo: there's no point looking through all the integer registers
	-- in order to find a floating-point one.

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) f 
        = f .&. complement (1 `shiftL` r)

allocateReg _ _
	= panic "RegAlloc.Linear.X86.FreeRegs.allocateReg: no reg"


