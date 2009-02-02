
-- | Free regs map for i386 and x86_64
module RegAlloc.Linear.X86.FreeRegs
where

import MachRegs

import Data.Word
import Data.Bits
import Data.List

type FreeRegs 
	= Word32

noFreeRegs :: FreeRegs
noFreeRegs = 0

releaseReg :: RegNo -> FreeRegs -> FreeRegs
releaseReg n f = f .|. (1 `shiftL` n)

initFreeRegs :: FreeRegs
initFreeRegs = foldr releaseReg noFreeRegs allocatableRegs

getFreeRegs :: RegClass -> FreeRegs -> [RegNo]	-- lazilly
getFreeRegs cls f = go f 0

  where go 0 _ = []
        go n m 
	  | n .&. 1 /= 0 && regClass (RealReg m) == cls
	  = m : (go (n `shiftR` 1) $! (m+1))

	  | otherwise
	  = go (n `shiftR` 1) $! (m+1)
	-- ToDo: there's no point looking through all the integer registers
	-- in order to find a floating-point one.

allocateReg :: RegNo -> FreeRegs -> FreeRegs
allocateReg r f = f .&. complement (1 `shiftL` fromIntegral r)


