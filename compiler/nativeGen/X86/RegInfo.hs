
module X86.RegInfo (
	mkVirtualReg,
	regDotColor
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import Size
import Reg

import Outputable
import Platform
import Unique

import UniqFM
import X86.Regs


mkVirtualReg :: Unique -> Size -> VirtualReg
mkVirtualReg u size
   = case size of
        FF32	-> VirtualRegSSE u
        FF64	-> VirtualRegSSE u
        FF80	-> VirtualRegD   u
        _other  -> VirtualRegI   u

regDotColor :: Platform -> RealReg -> SDoc
regDotColor platform reg
 = let Just str = lookupUFM (regColors platform) reg
   in text str

regColors :: Platform -> UniqFM [Char]
regColors platform = listToUFM (normalRegColors platform ++ fpRegColors)

normalRegColors :: Platform -> [(Reg,String)]
normalRegColors platform
                = case platformArch platform of
                  ArchX86 -> [ (eax, "#00ff00")
                             , (ebx, "#0000ff")
                             , (ecx, "#00ffff")
                             , (edx, "#0080ff") ]
                  ArchX86_64 -> [ (rax, "#00ff00"), (eax, "#00ff00")
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
                  ArchPPC     -> panic "X86 normalRegColors ArchPPC"
                  ArchPPC_64  -> panic "X86 normalRegColors ArchPPC_64"
                  ArchSPARC   -> panic "X86 normalRegColors ArchSPARC"
                  ArchARM _ _ -> panic "X86 normalRegColors ArchARM"
                  ArchUnknown -> panic "X86 normalRegColors ArchUnknown"

fpRegColors :: [(Reg,String)]
fpRegColors =
        [ (fake0, "#ff00ff")
	, (fake1, "#ff00aa")
	, (fake2, "#aa00ff")
	, (fake3, "#aa00aa")
	, (fake4, "#ff0055")
	, (fake5, "#5500ff") ]

	++ zip (map regSingle [24..39]) (repeat "red")

