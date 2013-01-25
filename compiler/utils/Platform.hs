
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
        osElfTarget
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
        | ArchAlpha
        | ArchMipseb
        | ArchMipsel
        deriving (Read, Show, Eq)


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

