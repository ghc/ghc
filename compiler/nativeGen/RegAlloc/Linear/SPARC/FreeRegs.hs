
-- | Free regs map for SPARC
module RegAlloc.Linear.SPARC.FreeRegs
where

import SPARC.Regs
import RegClass
import Reg

import CodeGen.Platform
import Outputable
import Platform
import FastBool

import Data.Word
import Data.Bits
-- import Data.List


--------------------------------------------------------------------------------
-- SPARC is like PPC, except for twinning of floating point regs.
--      When we allocate a double reg we must take an even numbered
--      float reg, as well as the one after it.


-- Holds bitmaps showing what registers are currently allocated.
--      The float and double reg bitmaps overlap, but we only alloc
--      float regs into the float map, and double regs into the double map.
--
--      Free regs have a bit set in the corresponding bitmap.
--
data FreeRegs 
        = FreeRegs 
                !Word32         -- int    reg bitmap    regs  0..31
                !Word32         -- float  reg bitmap    regs 32..63
                !Word32         -- double reg bitmap    regs 32..63

instance Show FreeRegs where
        show = showFreeRegs

-- | A reg map where no regs are free to be allocated.
noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0 0


-- | The initial set of free regs.
initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
 =      foldr (releaseReg platform) noFreeRegs allocatableRegs

                        
-- | Get all the free registers of this class.
getFreeRegs :: RegClass -> FreeRegs -> [RealReg]        -- lazily
getFreeRegs cls (FreeRegs g f d)
        | RcInteger <- cls = map RealRegSingle                  $ go 1 g 1 0  
        | RcFloat   <- cls = map RealRegSingle                  $ go 1 f 1 32 
        | RcDouble  <- cls = map (\i -> RealRegPair i (i+1))    $ go 2 d 1 32 
        | otherwise = pprPanic "RegAllocLinear.getFreeRegs: Bad register class " (ppr cls)
        where
                go _    _      0    _
                        = []

                go step bitmap mask ix 
                        | bitmap .&. mask /= 0 
                        = ix : (go step bitmap (mask `shiftL` step) $! ix + step) 

                        | otherwise    
                        = go step bitmap (mask `shiftL` step) $! ix + step


-- | Grab a register.
allocateReg :: Platform -> RealReg -> FreeRegs -> FreeRegs
allocateReg platform
         reg@(RealRegSingle r)
             (FreeRegs g f d)

        -- can't allocate free regs
        | not $ isFastTrue (freeReg platform r)
        = pprPanic "SPARC.FreeRegs.allocateReg: not allocating pinned reg" (ppr reg)
        
        -- a general purpose reg
        | r <= 31
        = let   mask    = complement (bitMask r)
          in    FreeRegs 
                        (g .&. mask) 
                        f 
                        d

        -- a float reg
        | r >= 32, r <= 63
        = let   mask    = complement (bitMask (r - 32))
        
                -- the mask of the double this FP reg aliases
                maskLow = if r `mod` 2 == 0
                                then complement (bitMask (r - 32))
                                else complement (bitMask (r - 32 - 1))
          in    FreeRegs
                        g
                        (f .&. mask)
                        (d .&. maskLow)

        | otherwise
        = pprPanic "SPARC.FreeRegs.releaseReg: not allocating bad reg" (ppr reg)
                        
allocateReg _
         reg@(RealRegPair r1 r2)
             (FreeRegs g f d)
        
        | r1 >= 32, r1 <= 63, r1 `mod` 2 == 0
        , r2 >= 32, r2 <= 63
        = let   mask1   = complement (bitMask (r1 - 32))
                mask2   = complement (bitMask (r2 - 32))
          in
                FreeRegs
                        g
                        ((f .&. mask1) .&. mask2)
                        (d .&. mask1)
                        
        | otherwise
        = pprPanic "SPARC.FreeRegs.releaseReg: not allocating bad reg" (ppr reg)
 


-- | Release a register from allocation.
--      The register liveness information says that most regs die after a C call, 
--      but we still don't want to allocate to some of them.
--
releaseReg :: Platform -> RealReg -> FreeRegs -> FreeRegs
releaseReg platform
         reg@(RealRegSingle r) 
        regs@(FreeRegs g f d)

        -- don't release pinned reg
        | not $ isFastTrue (freeReg platform r)
        = regs

        -- a general purpose reg
        | r <= 31       
        = let   mask    = bitMask r
          in    FreeRegs (g .|. mask) f d

        -- a float reg
        | r >= 32, r <= 63
        = let   mask    = bitMask (r - 32)
                
                -- the mask of the double this FP reg aliases
                maskLow = if r `mod` 2 == 0
                                then bitMask (r - 32)
                                else bitMask (r - 32 - 1)
          in    FreeRegs 
                        g 
                        (f .|. mask)
                        (d .|. maskLow)

        | otherwise
        = pprPanic "SPARC.FreeRegs.releaseReg: not releasing bad reg" (ppr reg)
        
releaseReg _
         reg@(RealRegPair r1 r2) 
             (FreeRegs g f d)

        | r1 >= 32, r1 <= 63, r1 `mod` 2 == 0
        , r2 >= 32, r2 <= 63
        = let   mask1   = bitMask (r1 - 32)
                mask2   = bitMask (r2 - 32)
          in
                FreeRegs
                        g
                        ((f .|. mask1) .|. mask2)
                        (d .|. mask1)
                        
        | otherwise
        = pprPanic "SPARC.FreeRegs.releaseReg: not releasing bad reg" (ppr reg)
           


bitMask :: Int -> Word32
bitMask n       = 1 `shiftL` n


showFreeRegs :: FreeRegs -> String
showFreeRegs regs
        =  "FreeRegs\n"
        ++ "    integer: " ++ (show $ getFreeRegs RcInteger regs)       ++ "\n"
        ++ "      float: " ++ (show $ getFreeRegs RcFloat   regs)       ++ "\n"
        ++ "     double: " ++ (show $ getFreeRegs RcDouble  regs)       ++ "\n"

