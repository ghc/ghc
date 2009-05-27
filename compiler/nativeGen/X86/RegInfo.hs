
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
import Unique

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
import UniqFM
import X86.Regs
#endif


mkVirtualReg :: Unique -> Size -> VirtualReg
mkVirtualReg u size
   | not (isFloatSize size) = VirtualRegI u
   | otherwise
   = case size of
        FF32	-> VirtualRegD u
        FF64	-> VirtualRegD u
	_	-> panic "mkVirtualReg"


-- reg colors for x86
#if i386_TARGET_ARCH
regDotColor :: RealReg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors :: UniqFM [Char]
regColors
 = listToUFM
 $  	[ (eax,	"#00ff00")
	, (ebx,	"#0000ff")
	, (ecx,	"#00ffff")
	, (edx,	"#0080ff")

	, (fake0, "#ff00ff")
	, (fake1, "#ff00aa")
	, (fake2, "#aa00ff")
	, (fake3, "#aa00aa")
	, (fake4, "#ff0055")
	, (fake5, "#5500ff") ]


-- reg colors for x86_64
#elif x86_64_TARGET_ARCH
regDotColor :: RealReg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors :: UniqFM [Char]
regColors
 = listToUFM
 $	[ (rax, "#00ff00"), (eax, "#00ff00")
	, (rbx,	"#0000ff"), (ebx, "#0000ff")
	, (rcx,	"#00ffff"), (ecx, "#00ffff")
	, (rdx,	"#0080ff"), (edx, "#00ffff")
	, (r8,  "#00ff80")
	, (r9,  "#008080")
	, (r10, "#0040ff")
	, (r11, "#00ff40")
	, (r12, "#008040")
	, (r13, "#004080")
	, (r14, "#004040")
	, (r15, "#002080") ]

	++ zip (map regSingle [16..31]) (repeat "red")
#else
regDotColor :: Reg -> SDoc
regDotColor	= panic "not defined"
#endif
