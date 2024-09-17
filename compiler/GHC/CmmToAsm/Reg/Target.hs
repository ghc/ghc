
-- | Hard wired things related to registers.
--      This is module is preventing the native code generator being able to
--      emit code for non-host architectures.
--
--      TODO: Do a better job of the overloading, and eliminate this module.
--      We'd probably do better with a Register type class, and hook this to
--      Instruction somehow.
--
--      TODO: We should also make arch specific versions of RegAlloc.Graph.TrivColorable
module GHC.CmmToAsm.Reg.Target (
        targetVirtualRegSqueeze,
        targetRealRegSqueeze,
        targetClassOfRealReg,
        targetMkVirtualReg,
        targetRegDotColor,
        targetClassOfReg,
        mapRegFormatSet,
)

where

import GHC.Prelude

import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Format

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Platform

import qualified GHC.CmmToAsm.X86.Regs       as X86
import qualified GHC.CmmToAsm.X86.RegInfo    as X86
import qualified GHC.CmmToAsm.PPC.Regs       as PPC
import qualified GHC.CmmToAsm.AArch64.Regs   as AArch64
import qualified GHC.CmmToAsm.RV64.Regs   as RV64

targetVirtualRegSqueeze :: Platform -> RegClass -> VirtualReg -> Int
targetVirtualRegSqueeze platform
    = case platformArch platform of
      ArchX86       -> X86.virtualRegSqueeze
      ArchX86_64    -> X86.virtualRegSqueeze
      ArchPPC       -> PPC.virtualRegSqueeze
      ArchS390X     -> panic "targetVirtualRegSqueeze ArchS390X"
      ArchPPC_64 _  -> PPC.virtualRegSqueeze
      ArchARM _ _ _ -> panic "targetVirtualRegSqueeze ArchARM"
      ArchAArch64   -> AArch64.virtualRegSqueeze
      ArchAlpha     -> panic "targetVirtualRegSqueeze ArchAlpha"
      ArchMipseb    -> panic "targetVirtualRegSqueeze ArchMipseb"
      ArchMipsel    -> panic "targetVirtualRegSqueeze ArchMipsel"
      ArchRISCV64   -> RV64.virtualRegSqueeze
      ArchLoongArch64->panic "targetVirtualRegSqueeze ArchLoongArch64"
      ArchJavaScript-> panic "targetVirtualRegSqueeze ArchJavaScript"
      ArchWasm32    -> panic "targetVirtualRegSqueeze ArchWasm32"
      ArchUnknown   -> panic "targetVirtualRegSqueeze ArchUnknown"


targetRealRegSqueeze :: Platform -> RegClass -> RealReg -> Int
targetRealRegSqueeze platform
    = case platformArch platform of
      ArchX86       -> X86.realRegSqueeze
      ArchX86_64    -> X86.realRegSqueeze
      ArchPPC       -> PPC.realRegSqueeze
      ArchS390X     -> panic "targetRealRegSqueeze ArchS390X"
      ArchPPC_64 _  -> PPC.realRegSqueeze
      ArchARM _ _ _ -> panic "targetRealRegSqueeze ArchARM"
      ArchAArch64   -> AArch64.realRegSqueeze
      ArchAlpha     -> panic "targetRealRegSqueeze ArchAlpha"
      ArchMipseb    -> panic "targetRealRegSqueeze ArchMipseb"
      ArchMipsel    -> panic "targetRealRegSqueeze ArchMipsel"
      ArchRISCV64   -> RV64.realRegSqueeze
      ArchLoongArch64->panic "targetRealRegSqueeze ArchLoongArch64"
      ArchJavaScript-> panic "targetRealRegSqueeze ArchJavaScript"
      ArchWasm32    -> panic "targetRealRegSqueeze ArchWasm32"
      ArchUnknown   -> panic "targetRealRegSqueeze ArchUnknown"

targetClassOfRealReg :: Platform -> RealReg -> RegClass
targetClassOfRealReg platform
    = case platformArch platform of
      ArchX86       -> X86.classOfRealReg platform
      ArchX86_64    -> X86.classOfRealReg platform
      ArchPPC       -> PPC.classOfRealReg
      ArchS390X     -> panic "targetClassOfRealReg ArchS390X"
      ArchPPC_64 _  -> PPC.classOfRealReg
      ArchARM _ _ _ -> panic "targetClassOfRealReg ArchARM"
      ArchAArch64   -> AArch64.classOfRealReg
      ArchAlpha     -> panic "targetClassOfRealReg ArchAlpha"
      ArchMipseb    -> panic "targetClassOfRealReg ArchMipseb"
      ArchMipsel    -> panic "targetClassOfRealReg ArchMipsel"
      ArchRISCV64   -> RV64.classOfRealReg
      ArchLoongArch64->panic "targetClassOfRealReg ArchLoongArch64"
      ArchJavaScript-> panic "targetClassOfRealReg ArchJavaScript"
      ArchWasm32    -> panic "targetClassOfRealReg ArchWasm32"
      ArchUnknown   -> panic "targetClassOfRealReg ArchUnknown"

targetMkVirtualReg :: Platform -> Unique -> Format -> VirtualReg
targetMkVirtualReg platform
    = case platformArch platform of
      ArchX86       -> X86.mkVirtualReg
      ArchX86_64    -> X86.mkVirtualReg
      ArchPPC       -> PPC.mkVirtualReg
      ArchS390X     -> panic "targetMkVirtualReg ArchS390X"
      ArchPPC_64 _  -> PPC.mkVirtualReg
      ArchARM _ _ _ -> panic "targetMkVirtualReg ArchARM"
      ArchAArch64   -> AArch64.mkVirtualReg
      ArchAlpha     -> panic "targetMkVirtualReg ArchAlpha"
      ArchMipseb    -> panic "targetMkVirtualReg ArchMipseb"
      ArchMipsel    -> panic "targetMkVirtualReg ArchMipsel"
      ArchRISCV64   -> RV64.mkVirtualReg
      ArchLoongArch64->panic "targetMkVirtualReg ArchLoongArch64"
      ArchJavaScript-> panic "targetMkVirtualReg ArchJavaScript"
      ArchWasm32    -> panic "targetMkVirtualReg ArchWasm32"
      ArchUnknown   -> panic "targetMkVirtualReg ArchUnknown"

targetRegDotColor :: Platform -> RealReg -> SDoc
targetRegDotColor platform
    = case platformArch platform of
      ArchX86       -> X86.regDotColor platform
      ArchX86_64    -> X86.regDotColor platform
      ArchPPC       -> PPC.regDotColor
      ArchS390X     -> panic "targetRegDotColor ArchS390X"
      ArchPPC_64 _  -> PPC.regDotColor
      ArchARM _ _ _ -> panic "targetRegDotColor ArchARM"
      ArchAArch64   -> AArch64.regDotColor
      ArchAlpha     -> panic "targetRegDotColor ArchAlpha"
      ArchMipseb    -> panic "targetRegDotColor ArchMipseb"
      ArchMipsel    -> panic "targetRegDotColor ArchMipsel"
      ArchRISCV64   -> RV64.regDotColor
      ArchLoongArch64->panic "targetRegDotColor ArchLoongArch64"
      ArchJavaScript-> panic "targetRegDotColor ArchJavaScript"
      ArchWasm32    -> panic "targetRegDotColor ArchWasm32"
      ArchUnknown   -> panic "targetRegDotColor ArchUnknown"


targetClassOfReg :: Platform -> Reg -> RegClass
targetClassOfReg platform reg
 = case reg of
   RegVirtual vr -> classOfVirtualReg (platformArch platform) vr
   RegReal rr -> targetClassOfRealReg platform rr

mapRegFormatSet :: HasDebugCallStack => (Reg -> Reg) -> UniqSet RegWithFormat -> UniqSet RegWithFormat
mapRegFormatSet f = mapUniqSet (\ ( RegWithFormat r fmt ) -> RegWithFormat ( f r ) fmt)
