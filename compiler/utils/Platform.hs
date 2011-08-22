
-- | A description of the platform we're compiling for.
--      In the future, this module should be the only one that references
--      the evil #defines for each TARGET_ARCH and TARGET_OS
--
module Platform (
        Platform(..),
        Arch(..),
        OS(..),
        ArmISA(..),
        ArmISAExt(..),

        defaultTargetPlatform,
        target32Bit,
        osElfTarget
)

where

import Panic

#include "HsVersions.h"


-- | Contains enough information for the native code generator to emit
--      code for this platform.
data Platform
        = Platform
        { platformArch  :: Arch
        , platformOS    :: OS }


-- | Architectures that the native code generator knows about.
--      TODO: It might be nice to extend these constructors with information
--      about what instruction set extensions an architecture might support.
--
data Arch
        = ArchUnknown
        | ArchX86
        | ArchX86_64
        | ArchPPC
        | ArchPPC_64
        | ArchSPARC
        | ArchARM
          { armISA    :: ArmISA
          , armISAExt :: [ArmISAExt] }
        deriving (Show, Eq)


-- | Operating systems that the native code generator knows about.
--      Having OSUnknown should produce a sensible default, but no promises.
data OS
        = OSUnknown
        | OSLinux
        | OSDarwin
        | OSSolaris2
        | OSMinGW32
        | OSFreeBSD
        | OSOpenBSD
        deriving (Show, Eq)

-- | ARM Instruction Set Architecture and Extensions
--
data ArmISA
    = ARMv5
    | ARMv6
    | ARMv7
    deriving (Show, Eq)

data ArmISAExt
    = VFPv2
    | VFPv3
    | VFPv3D16
    | NEON
    | IWMMX2
    deriving (Show, Eq)


target32Bit :: Platform -> Bool
target32Bit p = case platformArch p of
                ArchUnknown -> panic "Don't know if ArchUnknown is 32bit"
                ArchX86     -> True
                ArchX86_64  -> False
                ArchPPC     -> True
                ArchPPC_64  -> False
                ArchSPARC   -> True
                ArchARM _ _ -> True


-- | This predicates tells us whether the OS supports ELF-like shared libraries.
osElfTarget :: OS -> Bool
osElfTarget OSLinux    = True
osElfTarget OSFreeBSD  = True
osElfTarget OSOpenBSD  = True
osElfTarget OSSolaris2 = True
osElfTarget OSDarwin   = False
osElfTarget OSMinGW32  = False
osElfTarget OSUnknown  = panic "Don't know if OSUnknown is elf"


-- | This is the target platform as far as the #ifdefs are concerned.
--      These are set in includes/ghcplatform.h by the autoconf scripts
defaultTargetPlatform :: Platform
defaultTargetPlatform
        = Platform defaultTargetArch defaultTargetOS


-- | Move the evil TARGET_ARCH #ifdefs into Haskell land.
defaultTargetArch :: Arch
#if i386_TARGET_ARCH
defaultTargetArch       = ArchX86
#elif x86_64_TARGET_ARCH
defaultTargetArch       = ArchX86_64
#elif powerpc_TARGET_ARCH
defaultTargetArch       = ArchPPC
#elif powerpc64_TARGET_ARCH
defaultTargetArch       = ArchPPC_64
#elif sparc_TARGET_ARCH
defaultTargetArch       = ArchSPARC
#elif arm_TARGET_ARCH
defaultTargetArch       = ArchARM defaultTargetArmISA defaultTargetArmISAExt
#else
defaultTargetArch       = ArchUnknown
#endif


-- | Move the evil TARGET_OS #ifdefs into Haskell land.
defaultTargetOS :: OS
#if   linux_TARGET_OS
defaultTargetOS = OSLinux
#elif darwin_TARGET_OS
defaultTargetOS = OSDarwin
#elif solaris2_TARGET_OS
defaultTargetOS = OSSolaris2
#elif mingw32_TARGET_OS
defaultTargetOS = OSMinGW32
#elif freebsd_TARGET_OS
defaultTargetOS = OSFreeBSD
#elif kfreebsdgnu_TARGET_OS
defaultTargetOS = OSFreeBSD
#elif openbsd_TARGET_OS
defaultTargetOS = OSOpenBSD
#else
defaultTargetOS = OSUnknown
#endif

#if arm_TARGET_ARCH
defaultTargetArmISA :: ArmISA
#if defined(arm_HOST_ARCH_PRE_ARMv6)
defaultTargetArmISA = ARMv5
#elif defined(arm_HOST_ARCH_PRE_ARMv7)
defaultTargetArmISA = ARMv6
#else
defaultTargetArmISA = ARMv7
#endif

defaultTargetArmISAExt :: [ArmISAExt]
#if defined(arm_TARGET_ARCH) && !defined(arm_HOST_ARCH_PRE_ARMv7)
/* wild guess really, in case of ARMv7 we assume both VFPv3 and NEON presented
   however this is not true for SoCs like NVidia Tegra2 and Marvell Dove */
defaultTargetArmISAExt = [VFPv3, NEON]
#else
defaultTargetArmISAExt = []
#endif
#endif /* arm_TARGET_ARCH */
