{-# LANGUAGE LambdaCase #-}

-- | A description of the platform we're compiling for.
--
module Platform (
        Platform(..),
        Arch(..),
        OS(..),
        ArmISA(..),
        ArmISAExt(..),
        ArmABI(..),
        PPC_64ABI(..),

        target32Bit,
        isARM,
        osElfTarget,
        osMachOTarget,
        osSubsectionsViaSymbols,
        platformUsesFrameworks,
        stringifyArch,
        stringifyOS,
)

where

import GhcPrelude

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
              platformHasSubsectionsViaSymbols :: Bool,
              platformIsCrossCompiling         :: Bool
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
          { ppc_64ABI :: PPC_64ABI
          }
        | ArchSPARC
        | ArchSPARC64
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

stringifyArch :: Arch -> String
stringifyArch = \case
  ArchUnknown -> "unknown"
  ArchX86 -> "i386"
  ArchX86_64 -> "x86_64"
  ArchPPC -> "powerpc"
  ArchPPC_64 { ppc_64ABI = abi } -> case abi of
    ELF_V1 -> "powerpc64"
    ELF_V2 -> "powerpc64le"
  ArchSPARC -> "sparc"
  ArchSPARC64 -> "sparc64"
  ArchARM { armISA = isa, armISAExt = _, armABI = _ } -> "arm" ++ vsuf
    where
      vsuf = case isa of
        ARMv5 -> "v5"
        ARMv6 -> "v6"
        ARMv7 -> "v7"
  ArchARM64 -> "aarch64"
  ArchAlpha -> "alpha"
  ArchMipseb -> "mipseb"
  ArchMipsel -> "mipsel"
  ArchJavaScript -> "js"

isARM :: Arch -> Bool
isARM (ArchARM {}) = True
isARM ArchARM64    = True
isARM _ = False

-- | Operating systems that the native code generator knows about.
--      Having OSUnknown should produce a sensible default, but no promises.
data OS
        = OSUnknown
        | OSLinux
        | OSDarwin
        | OSSolaris2
        | OSMinGW32
        | OSFreeBSD
        | OSDragonFly
        | OSOpenBSD
        | OSNetBSD
        | OSKFreeBSD
        | OSHaiku
        | OSQNXNTO
        | OSAIX
        | OSHurd
        deriving (Read, Show, Eq)

stringifyOS :: OS -> String
stringifyOS = \case
  OSUnknown -> "unknown"
  OSLinux -> "linux"
  OSDarwin -> "darwin"
  OSSolaris2 -> "solaris2"
  OSMinGW32 -> "mingw32"
  OSFreeBSD -> "freebsd"
  OSDragonFly -> "dragonfly"
  OSOpenBSD -> "openbsd"
  OSNetBSD -> "netbsd"
  OSKFreeBSD -> "kfreebsdgnu"
  OSHaiku -> "haiku"
  OSQNXNTO -> "nto-qnx"
  OSAIX -> "aix"
  OSHurd -> "hurd"

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

-- | PowerPC 64-bit ABI
--
data PPC_64ABI
    = ELF_V1
    | ELF_V2
    deriving (Read, Show, Eq)

-- | This predicate tells us whether the platform is 32-bit.
target32Bit :: Platform -> Bool
target32Bit p = platformWordSize p == 4

-- | This predicate tells us whether the OS supports ELF-like shared libraries.
osElfTarget :: OS -> Bool
osElfTarget OSLinux     = True
osElfTarget OSFreeBSD   = True
osElfTarget OSDragonFly = True
osElfTarget OSOpenBSD   = True
osElfTarget OSNetBSD    = True
osElfTarget OSSolaris2  = True
osElfTarget OSDarwin    = False
osElfTarget OSMinGW32   = False
osElfTarget OSKFreeBSD  = True
osElfTarget OSHaiku     = True
osElfTarget OSQNXNTO    = False
osElfTarget OSAIX       = False
osElfTarget OSHurd      = True
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
osUsesFrameworks _        = False

platformUsesFrameworks :: Platform -> Bool
platformUsesFrameworks = osUsesFrameworks . platformOS

osSubsectionsViaSymbols :: OS -> Bool
osSubsectionsViaSymbols OSDarwin = True
osSubsectionsViaSymbols _        = False

