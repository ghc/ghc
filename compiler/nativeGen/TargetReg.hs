
-- | Hard wired things related to registers.
--	This is module is preventing the native code generator being able to 
--	emit code for non-host architectures.
--
--	TODO: Do a better job of the overloading, and eliminate this module.
--	We'd probably do better with a Register type class, and hook this to 
--	Instruction somehow.
--
--	TODO: We should also make arch specific versions of RegAlloc.Graph.TrivColorable

module TargetReg (
	targetVirtualRegSqueeze,
	targetRealRegSqueeze,
	targetClassOfRealReg,
	targetMkVirtualReg,
	targetWordSize,
	targetRegDotColor,
	targetClassOfReg
)

where

#include "HsVersions.h"

import Reg
import RegClass
import Size

import CmmType	(wordWidth)
import Outputable
import Unique
import FastTypes
import Platform

import qualified X86.Regs       as X86
import qualified X86.RegInfo    as X86

import qualified PPC.Regs       as PPC

import qualified SPARC.Regs     as SPARC

-- TODO: We shouldn't be using defaultTargetPlatform here.
--       We should be passing DynFlags in instead, and looking at
--       its targetPlatform.

targetVirtualRegSqueeze :: RegClass -> VirtualReg -> FastInt
targetVirtualRegSqueeze
    = case platformArch defaultTargetPlatform of
      ArchX86    -> X86.virtualRegSqueeze
      ArchX86_64 -> X86.virtualRegSqueeze
      ArchPPC    -> PPC.virtualRegSqueeze
      ArchSPARC  -> SPARC.virtualRegSqueeze
      ArchPPC_64 -> panic "targetVirtualRegSqueeze ArchPPC_64"

targetRealRegSqueeze :: RegClass -> RealReg -> FastInt
targetRealRegSqueeze
    = case platformArch defaultTargetPlatform of
      ArchX86    -> X86.realRegSqueeze
      ArchX86_64 -> X86.realRegSqueeze
      ArchPPC    -> PPC.realRegSqueeze
      ArchSPARC  -> SPARC.realRegSqueeze
      ArchPPC_64 -> panic "targetRealRegSqueeze ArchPPC_64"

targetClassOfRealReg :: RealReg -> RegClass
targetClassOfRealReg
    = case platformArch defaultTargetPlatform of
      ArchX86    -> X86.classOfRealReg
      ArchX86_64 -> X86.classOfRealReg
      ArchPPC    -> PPC.classOfRealReg
      ArchSPARC  -> SPARC.classOfRealReg
      ArchPPC_64 -> panic "targetClassOfRealReg ArchPPC_64"

-- TODO: This should look at targetPlatform too
targetWordSize :: Size
targetWordSize = intSize wordWidth

targetMkVirtualReg :: Unique -> Size -> VirtualReg
targetMkVirtualReg
    = case platformArch defaultTargetPlatform of
      ArchX86    -> X86.mkVirtualReg
      ArchX86_64 -> X86.mkVirtualReg
      ArchPPC    -> PPC.mkVirtualReg
      ArchSPARC  -> SPARC.mkVirtualReg
      ArchPPC_64 -> panic "targetMkVirtualReg ArchPPC_64"

targetRegDotColor :: RealReg -> SDoc
targetRegDotColor
    = case platformArch defaultTargetPlatform of
      ArchX86    -> X86.regDotColor
      ArchX86_64 -> X86.regDotColor
      ArchPPC    -> PPC.regDotColor
      ArchSPARC  -> SPARC.regDotColor
      ArchPPC_64 -> panic "targetRegDotColor ArchPPC_64"


targetClassOfReg :: Reg -> RegClass
targetClassOfReg reg
 = case reg of
 	RegVirtual vr	-> classOfVirtualReg vr
	RegReal rr	-> targetClassOfRealReg rr


