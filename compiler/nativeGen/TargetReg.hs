{-# LANGUAGE CPP #-}
-- | Hard wired things related to registers.
--      This is module is preventing the native code generator being able to
--      emit code for non-host architectures.
--
--      TODO: Do a better job of the overloading, and eliminate this module.
--      We'd probably do better with a Register type class, and hook this to
--      Instruction somehow.
--
--      TODO: We should also make arch specific versions of RegAlloc.Graph.TrivColorable
module TargetReg (
        targetVirtualRegSqueeze,
        targetRealRegSqueeze,
        targetClassOfRealReg,
        targetMkVirtualReg,
        targetRegDotColor,
        targetClassOfReg
)

where

#include "HsVersions.h"

import GhcPrelude

import Reg
import RegClass
import Format

import Outputable
import Unique
import Platform

import qualified X86.Regs       as X86
import qualified X86.RegInfo    as X86

import qualified PPC.Regs       as PPC

import qualified SPARC.Regs     as SPARC

targetVirtualRegSqueeze :: Platform -> RegClass -> VirtualReg -> Int
targetVirtualRegSqueeze platform
    = case platformArch platform of
      ArchX86       -> X86.virtualRegSqueeze
      ArchX86_64    -> X86.virtualRegSqueeze
      ArchPPC       -> PPC.virtualRegSqueeze
      ArchSPARC     -> SPARC.virtualRegSqueeze
      ArchSPARC64   -> panic "targetVirtualRegSqueeze ArchSPARC64"
      ArchPPC_64 _  -> PPC.virtualRegSqueeze
      ArchARM _ _ _ -> panic "targetVirtualRegSqueeze ArchARM"
      ArchARM64     -> panic "targetVirtualRegSqueeze ArchARM64"
      ArchAlpha     -> panic "targetVirtualRegSqueeze ArchAlpha"
      ArchMipseb    -> panic "targetVirtualRegSqueeze ArchMipseb"
      ArchMipsel    -> panic "targetVirtualRegSqueeze ArchMipsel"
      ArchJavaScript-> panic "targetVirtualRegSqueeze ArchJavaScript"
      ArchUnknown   -> panic "targetVirtualRegSqueeze ArchUnknown"


targetRealRegSqueeze :: Platform -> RegClass -> RealReg -> Int
targetRealRegSqueeze platform
    = case platformArch platform of
      ArchX86       -> X86.realRegSqueeze
      ArchX86_64    -> X86.realRegSqueeze
      ArchPPC       -> PPC.realRegSqueeze
      ArchSPARC     -> SPARC.realRegSqueeze
      ArchSPARC64   -> panic "targetRealRegSqueeze ArchSPARC64"
      ArchPPC_64 _  -> PPC.realRegSqueeze
      ArchARM _ _ _ -> panic "targetRealRegSqueeze ArchARM"
      ArchARM64     -> panic "targetRealRegSqueeze ArchARM64"
      ArchAlpha     -> panic "targetRealRegSqueeze ArchAlpha"
      ArchMipseb    -> panic "targetRealRegSqueeze ArchMipseb"
      ArchMipsel    -> panic "targetRealRegSqueeze ArchMipsel"
      ArchJavaScript-> panic "targetRealRegSqueeze ArchJavaScript"
      ArchUnknown   -> panic "targetRealRegSqueeze ArchUnknown"

targetClassOfRealReg :: Platform -> RealReg -> RegClass
targetClassOfRealReg platform
    = case platformArch platform of
      ArchX86       -> X86.classOfRealReg platform
      ArchX86_64    -> X86.classOfRealReg platform
      ArchPPC       -> PPC.classOfRealReg
      ArchSPARC     -> SPARC.classOfRealReg
      ArchSPARC64   -> panic "targetClassOfRealReg ArchSPARC64"
      ArchPPC_64 _  -> PPC.classOfRealReg
      ArchARM _ _ _ -> panic "targetClassOfRealReg ArchARM"
      ArchARM64     -> panic "targetClassOfRealReg ArchARM64"
      ArchAlpha     -> panic "targetClassOfRealReg ArchAlpha"
      ArchMipseb    -> panic "targetClassOfRealReg ArchMipseb"
      ArchMipsel    -> panic "targetClassOfRealReg ArchMipsel"
      ArchJavaScript-> panic "targetClassOfRealReg ArchJavaScript"
      ArchUnknown   -> panic "targetClassOfRealReg ArchUnknown"

targetMkVirtualReg :: Platform -> Unique -> Format -> VirtualReg
targetMkVirtualReg platform
    = case platformArch platform of
      ArchX86       -> X86.mkVirtualReg
      ArchX86_64    -> X86.mkVirtualReg
      ArchPPC       -> PPC.mkVirtualReg
      ArchSPARC     -> SPARC.mkVirtualReg
      ArchSPARC64   -> panic "targetMkVirtualReg ArchSPARC64"
      ArchPPC_64 _  -> PPC.mkVirtualReg
      ArchARM _ _ _ -> panic "targetMkVirtualReg ArchARM"
      ArchARM64     -> panic "targetMkVirtualReg ArchARM64"
      ArchAlpha     -> panic "targetMkVirtualReg ArchAlpha"
      ArchMipseb    -> panic "targetMkVirtualReg ArchMipseb"
      ArchMipsel    -> panic "targetMkVirtualReg ArchMipsel"
      ArchJavaScript-> panic "targetMkVirtualReg ArchJavaScript"
      ArchUnknown   -> panic "targetMkVirtualReg ArchUnknown"

targetRegDotColor :: Platform -> RealReg -> SDoc
targetRegDotColor platform
    = case platformArch platform of
      ArchX86       -> X86.regDotColor platform
      ArchX86_64    -> X86.regDotColor platform
      ArchPPC       -> PPC.regDotColor
      ArchSPARC     -> SPARC.regDotColor
      ArchSPARC64   -> panic "targetRegDotColor ArchSPARC64"
      ArchPPC_64 _  -> PPC.regDotColor
      ArchARM _ _ _ -> panic "targetRegDotColor ArchARM"
      ArchARM64     -> panic "targetRegDotColor ArchARM64"
      ArchAlpha     -> panic "targetRegDotColor ArchAlpha"
      ArchMipseb    -> panic "targetRegDotColor ArchMipseb"
      ArchMipsel    -> panic "targetRegDotColor ArchMipsel"
      ArchJavaScript-> panic "targetRegDotColor ArchJavaScript"
      ArchUnknown   -> panic "targetRegDotColor ArchUnknown"


targetClassOfReg :: Platform -> Reg -> RegClass
targetClassOfReg platform reg
 = case reg of
   RegVirtual vr -> classOfVirtualReg vr
   RegReal rr -> targetClassOfRealReg platform rr
