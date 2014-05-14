{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
fpRegColors =
        [ (fake0, "#ff00ff")
	, (fake1, "#ff00aa")
	, (fake2, "#aa00ff")
	, (fake3, "#aa00aa")
	, (fake4, "#ff0055")
	, (fake5, "#5500ff") ]

	++ zip (map regSingle [24..39]) (repeat "red")

