module GHC.CmmToAsm.RISCV64.Regs where
import GHC.Platform.Reg
import Prelude
import GHC.CmmToAsm.Format
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Platform
import GHC.Platform.Regs

allMachRegNos   :: [RegNo]
allMachRegNos   = [1..31] ++ [32..63]

-- argRegs is the set of regs which are read for an n-argument call to C.
allGpArgRegs :: [Reg]
allGpArgRegs = map regSingle [10..17] -- a0..a7
allFpArgRegs :: [Reg]
allFpArgRegs = map regSingle [42..49] -- fa0..fa7

-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: Platform -> [RealReg]
allocatableRegs platform
   = let isFree i = freeReg platform i
     in  map RealRegSingle $ filter isFree allMachRegNos

mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   | not (isFloatFormat format) = VirtualRegI u
   | otherwise
   = case format of
        FF32    -> VirtualRegD u
        FF64    -> VirtualRegD u
        _       -> panic "RISCV64.mkVirtualReg"
