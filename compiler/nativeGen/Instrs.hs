{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "nativeGen/NCG.h"


module Instrs (
	NatCmm,
	NatCmmTop,
	NatBasicBlock,
	condUnsigned,
	condToSigned,
	condToUnsigned,

#if   alpha_TARGET_ARCH
	module Alpha.Instr
#elif powerpc_TARGET_ARCH
	module PPC.Instr
#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
	module X86.Instr
#elif sparc_TARGET_ARCH
	module SPARC.Instr
#else
#error "Instrs: not defined for this architecture"
#endif
)

where

#include "HsVersions.h"

import BlockId
import Regs
import Cmm
import CLabel           ( CLabel, pprCLabel )
import Panic		( panic )
import Outputable
import FastString
import Constants       ( wORD_SIZE )

import GHC.Exts

#if   alpha_TARGET_ARCH
import Alpha.Instr
#elif powerpc_TARGET_ARCH
import PPC.Instr
#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
import X86.Instr
#elif sparc_TARGET_ARCH
import SPARC.Instr
#else
#error "Instrs: not defined for this architecture"
#endif


-- Our flavours of the Cmm types
-- Type synonyms for Cmm populated with native code

type NatCmm        = GenCmm CmmStatic [CmmStatic] (ListGraph Instr)
type NatCmmTop     = GenCmmTop CmmStatic [CmmStatic] (ListGraph Instr)
type NatBasicBlock = GenBasicBlock Instr


-- Condition utils
condUnsigned GU  = True
condUnsigned LU  = True
condUnsigned GEU = True
condUnsigned LEU = True
condUnsigned _   = False

condToSigned GU  = GTT
condToSigned LU  = LTT
condToSigned GEU = GE
condToSigned LEU = LE
condToSigned x   = x

condToUnsigned GTT = GU
condToUnsigned LTT = LU
condToUnsigned GE  = GEU
condToUnsigned LE  = LEU
condToUnsigned x   = x




