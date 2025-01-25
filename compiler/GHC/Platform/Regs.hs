module GHC.Platform.Regs
       (callerSaves, activeStgRegs, haveRegBase, globalRegMaybe, freeReg)
       where

import GHC.Prelude

import GHC.Cmm.Expr
import GHC.Platform
import GHC.Platform.Reg

import qualified GHC.Platform.ARM        as ARM
import qualified GHC.Platform.AArch64    as AArch64
import qualified GHC.Platform.PPC        as PPC
import qualified GHC.Platform.S390X      as S390X
import qualified GHC.Platform.X86        as X86
import qualified GHC.Platform.X86_64     as X86_64
import qualified GHC.Platform.RISCV64    as RISCV64
import qualified GHC.Platform.Wasm32     as Wasm32
import qualified GHC.Platform.LoongArch64 as LoongArch64
import qualified GHC.Platform.NoRegs     as NoRegs

-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: Platform -> GlobalReg -> Bool
callerSaves platform
 | platformUnregisterised platform = NoRegs.callerSaves
 | otherwise
 = case platformArch platform of
   ArchX86     -> X86.callerSaves
   ArchX86_64  -> X86_64.callerSaves
   ArchS390X   -> S390X.callerSaves
   ArchARM {}  -> ARM.callerSaves
   ArchAArch64 -> AArch64.callerSaves
   ArchRISCV64 -> RISCV64.callerSaves
   ArchWasm32  -> Wasm32.callerSaves
   ArchLoongArch64 -> LoongArch64.callerSaves
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.callerSaves

    | otherwise -> NoRegs.callerSaves

-- | Here is where the STG register map is defined for each target arch.
-- The order matters (for the llvm backend anyway)! We must make sure to
-- maintain the order here with the order used in the LLVM calling conventions.
-- Note that also, this isn't all registers, just the ones that are currently
-- possibly mapped to real registers.
activeStgRegs :: Platform -> [GlobalReg]
activeStgRegs platform
 | platformUnregisterised platform = NoRegs.activeStgRegs
 | otherwise
 = case platformArch platform of
   ArchX86     -> X86.activeStgRegs
   ArchX86_64  -> X86_64.activeStgRegs
   ArchS390X   -> S390X.activeStgRegs
   ArchARM {}  -> ARM.activeStgRegs
   ArchAArch64 -> AArch64.activeStgRegs
   ArchRISCV64 -> RISCV64.activeStgRegs
   ArchWasm32  -> Wasm32.activeStgRegs
   ArchLoongArch64 -> LoongArch64.activeStgRegs
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.activeStgRegs

    | otherwise -> NoRegs.activeStgRegs

haveRegBase :: Platform -> Bool
haveRegBase platform
 | platformUnregisterised platform = NoRegs.haveRegBase
 | otherwise
 = case platformArch platform of
   ArchX86     -> X86.haveRegBase
   ArchX86_64  -> X86_64.haveRegBase
   ArchS390X   -> S390X.haveRegBase
   ArchARM {}  -> ARM.haveRegBase
   ArchAArch64 -> AArch64.haveRegBase
   ArchRISCV64 -> RISCV64.haveRegBase
   ArchWasm32  -> Wasm32.haveRegBase
   ArchLoongArch64 -> LoongArch64.haveRegBase
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.haveRegBase

    | otherwise -> NoRegs.haveRegBase

globalRegMaybe :: Platform -> GlobalReg -> Maybe RealReg
globalRegMaybe platform
 | platformUnregisterised platform = NoRegs.globalRegMaybe
 | otherwise
 = case platformArch platform of
   ArchX86     -> X86.globalRegMaybe
   ArchX86_64  -> X86_64.globalRegMaybe
   ArchS390X   -> S390X.globalRegMaybe
   ArchARM {}  -> ARM.globalRegMaybe
   ArchAArch64 -> AArch64.globalRegMaybe
   ArchRISCV64 -> RISCV64.globalRegMaybe
   ArchWasm32  -> Wasm32.globalRegMaybe
   ArchLoongArch64 -> LoongArch64.globalRegMaybe
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.globalRegMaybe

    | otherwise -> NoRegs.globalRegMaybe

freeReg :: Platform -> RegNo -> Bool
freeReg platform
 | platformUnregisterised platform = NoRegs.freeReg
 | otherwise
 = case platformArch platform of
   ArchX86     -> X86.freeReg
   ArchX86_64  -> X86_64.freeReg
   ArchS390X   -> S390X.freeReg
   ArchARM {}  -> ARM.freeReg
   ArchAArch64 ->
    -- See Note [Aarch64 Register x18 at Darwin and Windows].
    -- It already has `freeReg 18 = False` but that line does not work for cross-compile when
    -- we use host not from the list (darwin_HOST_OS, ios_HOST_OS, mingw32_HOST_OS) i.e. Linux
    if platformOS platform == OSMinGW32 || platformOS platform == OSDarwin
        then
            let
                x18Check :: RegNo -> Bool
                x18Check 18 = False
                x18Check a = AArch64.freeReg a
            in x18Check
        else AArch64.freeReg
   ArchRISCV64 -> RISCV64.freeReg
   ArchWasm32  -> Wasm32.freeReg
   ArchLoongArch64 -> LoongArch64.freeReg
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.freeReg

    | otherwise -> NoRegs.freeReg
