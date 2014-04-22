
-- | A description of the platform we're compiling for.
--
module Platform (
        Platform(..),
        Arch(..),
        OS(..),
        ArmISA(..),
        ArmISAExt(..),
        ArmABI(..),

        target32Bit,
        isARM,
        osElfTarget,
        osMachOTarget,
        platformUsesFrameworks,
        platformBinariesAreStaticLibs,

        CPUDesc(..), isx86Desc,
        IntelCPU(..),
        IntelFeature(..),
        descToCPU,
        intelCPUFeatures
)

where

-- | Contains enough information for the native code generator to emit
--      code for this platform.
data Platform
        = Platform {
              platformArch                     :: Arch,
              platformOS                       :: OS,
              -- Word size in bytes (i.e. normally 4 or 8,
              -- for 32bit and 64bit platforms respectively)
              platformWordSize                 :: {-# UNPACK #-} !Int,
              platformUnregisterised           :: Bool,
              platformHasGnuNonexecStack       :: Bool,
              platformHasIdentDirective        :: Bool,
              platformHasSubsectionsViaSymbols :: Bool
          }
        deriving (Read, Show, Eq)


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
          , armISAExt :: [ArmISAExt]
          , armABI    :: ArmABI
          }
        | ArchARM64
        | ArchAlpha
        | ArchMipseb
        | ArchMipsel
        | ArchJavaScript
        deriving (Read, Show, Eq)

isARM :: Arch -> Bool
isARM (ArchARM {}) = True
isARM _ = False

-- | Operating systems that the native code generator knows about.
--      Having OSUnknown should produce a sensible default, but no promises.
data OS
        = OSUnknown
        | OSLinux
        | OSDarwin
        | OSiOS
        | OSSolaris2
        | OSMinGW32
        | OSFreeBSD
        | OSDragonFly
        | OSOpenBSD
        | OSNetBSD
        | OSKFreeBSD
        | OSHaiku
        | OSOsf3
        | OSQNXNTO
        | OSAndroid
        deriving (Read, Show, Eq)

-- | ARM Instruction Set Architecture, Extensions and ABI
--
data ArmISA
    = ARMv5
    | ARMv6
    | ARMv7
    deriving (Read, Show, Eq)

data ArmISAExt
    = VFPv2
    | VFPv3
    | VFPv3D16
    | NEON
    | IWMMX2
    deriving (Read, Show, Eq)

data ArmABI
    = SOFT
    | SOFTFP
    | HARD
    deriving (Read, Show, Eq)

target32Bit :: Platform -> Bool
target32Bit p = platformWordSize p == 4

-- | This predicates tells us whether the OS supports ELF-like shared libraries.
osElfTarget :: OS -> Bool
osElfTarget OSLinux     = True
osElfTarget OSFreeBSD   = True
osElfTarget OSDragonFly = True
osElfTarget OSOpenBSD   = True
osElfTarget OSNetBSD    = True
osElfTarget OSSolaris2  = True
osElfTarget OSDarwin    = False
osElfTarget OSiOS       = False
osElfTarget OSMinGW32   = False
osElfTarget OSKFreeBSD  = True
osElfTarget OSHaiku     = True
osElfTarget OSOsf3      = False -- I don't know if this is right, but as
                                -- per comment below it's safe
osElfTarget OSQNXNTO    = False
osElfTarget OSAndroid   = True
osElfTarget OSUnknown   = False
 -- Defaulting to False is safe; it means don't rely on any
 -- ELF-specific functionality.  It is important to have a default for
 -- portability, otherwise we have to answer this question for every
 -- new platform we compile on (even unreg).

-- | This predicate tells us whether the OS support Mach-O shared libraries.
osMachOTarget :: OS -> Bool
osMachOTarget OSDarwin = True
osMachOTarget _ = False

osUsesFrameworks :: OS -> Bool
osUsesFrameworks OSDarwin = True
osUsesFrameworks OSiOS    = True
osUsesFrameworks _        = False

platformUsesFrameworks :: Platform -> Bool
platformUsesFrameworks = osUsesFrameworks . platformOS

osBinariesAreStaticLibs :: OS -> Bool
osBinariesAreStaticLibs OSiOS = True
osBinariesAreStaticLibs _     = False

platformBinariesAreStaticLibs :: Platform -> Bool
platformBinariesAreStaticLibs = osBinariesAreStaticLibs . platformOS

-- -----------------------------------------------------------------------------
-- Platform-specific micro architectures.

-- CPU descriptions that may be fed to -mcpu or -march
data CPUDesc
  = Generic
  | Native
  | Intel IntelCPU

isx86Desc :: CPUDesc -> Bool
isx86Desc (Intel _) = True
isx86Desc _         = False

-- -----------------------------------------------------------------------------
-- Intel

-- Description of all Intel CPUs. Order isn't necessarily important -
-- we'll discriminate on feature set later anyway.
data IntelCPU
  = I386CPU
  | I486CPU
  | I586CPU
  | PentiumMMX
  | PentiumPro
  | I686CPU
  | Pentium2
  | Pentium3
  | Pentium3M
  | PentiumM
  | Pentium4
  | Pentium4M
  | Prescott
  | NoCona
  | Core2
  | Nehalem
  | Westmere
  | Sandybridge
  | Ivybridge
  | Haswell
  | Bonnell
  | Silvermont
  | Broadwell

data IntelFeature
  = MMX
  | SSE
  | SSE2
  | SSE3
  | SSSE3
  | SSE4
  | SSE41
  | SSE42
  | AVX1
  | ERMSB -- "Extended rep-movsb"
  | AVX2

descToCPU :: String -> Maybe CPUDesc
descToCPU "generic"     = Just Generic
descToCPU "native"      = Just Native
descToCPU "i386"        = Just $ Intel $ I386CPU
descToCPU "i486"        = Just $ Intel $ I486CPU
descToCPU "i586"        = Just $ Intel $ I586CPU
descToCPU "pentium"     = Just $ Intel $ I586CPU
descToCPU "pentium-mmx" = Just $ Intel $ PentiumMMX
descToCPU "pentiumpro"  = Just $ Intel $ PentiumPro
descToCPU "i686"        = Just $ Intel $ PentiumPro
descToCPU "pentium2"    = Just $ Intel $ Pentium2
descToCPU "pentium3"    = Just $ Intel $ Pentium3
descToCPU "pentium3m"   = Just $ Intel $ Pentium3M
descToCPU "pentium-m"   = Just $ Intel $ PentiumM
descToCPU "pentium4"    = Just $ Intel $ Pentium4
descToCPU "pentium4m"   = Just $ Intel $ Pentium4M
descToCPU "prescott"    = Just $ Intel $ Prescott
descToCPU "nocona"      = Just $ Intel $ NoCona
descToCPU "core2"       = Just $ Intel $ Core2
descToCPU "nehalem"     = Just $ Intel $ Nehalem
descToCPU "westmere"    = Just $ Intel $ Westmere
descToCPU "sandybridge" = Just $ Intel $ Sandybridge
descToCPU "ivybridge"   = Just $ Intel $ Ivybridge
descToCPU "haswell"     = Just $ Intel $ Haswell
descToCPU "bonnell"     = Just $ Intel $ Bonnell
descToCPU "silvermont"  = Just $ Intel $ Silvermont
descToCPU "broadwell"   = Just $ Intel $ Broadwell
descToCPU _             = Nothing

intelCPUFeatures :: IntelCPU -> [IntelFeature]
intelCPUFeatures I386CPU         = []
intelCPUFeatures I486CPU         = []
intelCPUFeatures I586CPU         = []
intelCPUFeatures PentiumMMX      = [MMX]
intelCPUFeatures PentiumPro      = [MMX]
intelCPUFeatures I686CPU         = [MMX]
intelCPUFeatures Pentium2        = [MMX]
intelCPUFeatures Pentium3        = [MMX, SSE]
intelCPUFeatures Pentium3M       = [MMX, SSE]
intelCPUFeatures PentiumM        = [MMX, SSE, SSE2]
intelCPUFeatures Pentium4        = [MMX, SSE, SSE2]
intelCPUFeatures Pentium4M       = [MMX, SSE, SSE2]
intelCPUFeatures Prescott        = [MMX, SSE, SSE2, SSE3]
intelCPUFeatures NoCona          = [MMX, SSE, SSE2, SSE3]
intelCPUFeatures Core2           = [MMX, SSE, SSE2, SSE3, SSSE3]
intelCPUFeatures Nehalem         = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42]
intelCPUFeatures Westmere        = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42]
intelCPUFeatures Sandybridge     = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1]
intelCPUFeatures Ivybridge       = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1, ERMSB]
intelCPUFeatures Haswell         = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1, ERMSB, AVX2]
intelCPUFeatures Bonnell         = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1, ERMSB, AVX2]
intelCPUFeatures Silvermont      = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1, ERMSB, AVX2]
intelCPUFeatures Broadwell       = [MMX, SSE, SSE2, SSE3, SSSE3, SSE4, SSE41, SSE42, AVX1, ERMSB, AVX2]
