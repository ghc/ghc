{-# LANGUAGE CPP #-}
module X86.RegInfo (
        mkVirtualReg,
        regDotColor
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import GhcPrelude

import Format
import Reg

import Outputable
import Platform
import Unique

import UniqFM
import X86.Regs


mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   = case format of
        FF32    -> VirtualRegD u
        -- for scalar F32, we use the same xmm as F64!
        -- this is a hack that needs some improvement.
        FF64    -> VirtualRegD u
        _other  -> VirtualRegI   u

regDotColor :: Platform -> RealReg -> SDoc
regDotColor platform reg
 = let Just str = lookupUFM (regColors platform) reg
   in text str

regColors :: Platform -> UniqFM [Char]
regColors platform = listToUFM (normalRegColors platform ++ fpRegColors)

normalRegColors :: Platform -> [(Reg,String)]
normalRegColors platform
 | target32Bit platform = [ (eax, "#00ff00")
                          , (ebx, "#0000ff")
                          , (ecx, "#00ffff")
                          , (edx, "#0080ff") ]
 | otherwise            = [ (rax, "#00ff00"), (eax, "#00ff00")
                          , (rbx, "#0000ff"), (ebx, "#0000ff")
                          , (rcx, "#00ffff"), (ecx, "#00ffff")
                          , (rdx, "#0080ff"), (edx, "#00ffff")
                          , (r8,  "#00ff80")
                          , (r9,  "#008080")
                          , (r10, "#0040ff")
                          , (r11, "#00ff40")
                          , (r12, "#008040")
                          , (r13, "#004080")
                          , (r14, "#004040")
                          , (r15, "#002080") ]

fpRegColors :: [(Reg,String)]
fpRegColors =zip (map regSingle [16..31])  (repeat "red")
