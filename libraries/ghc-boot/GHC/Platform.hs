{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

-- | A description of the platform we're compiling for.
--
module GHC.Platform
   ( PlatformMini(..)
   , PlatformWordSize(..)
   , Platform(..)
   , platformArch
   , platformOS
   , Arch(..)
   , OS(..)
   , ArmISA(..)
   , ArmISAExt(..)
   , ArmABI(..)
   , PPC_64ABI(..)
   , ByteOrder(..)
   , target32Bit
   , isARM
   , osElfTarget
   , osMachOTarget
   , osSubsectionsViaSymbols
   , platformUsesFrameworks
   , platformWordSizeInBytes
   , platformWordSizeInBits
   , platformMinInt
   , platformMaxInt
   , platformMaxWord
   , platformInIntRange
   , platformInWordRange
   , platformCConvNeedsExtension
   , PlatformMisc(..)
   , stringEncodeArch
   , stringEncodeOS
   , SseVersion (..)
   , BmiVersion (..)
   )
where

import Prelude -- See Note [Why do we import Prelude here?]
import GHC.Read
import GHC.ByteOrder (ByteOrder(..))
import Data.Word
import Data.Int

-- | Contains the bare-bones arch and os information. This isn't enough for
-- code gen, but useful for tasks where we can fall back upon the host
-- platform, as this is all we know about the host platform.
data PlatformMini
  = PlatformMini
    { platformMini_arch :: Arch
    , platformMini_os :: OS
    }
    deriving (Read, Show, Eq)

-- | Contains enough information for the native code generator to emit
-- code for this platform.
data Platform = Platform
   { platformMini                     :: !PlatformMini
   , platformWordSize                 :: !PlatformWordSize -- ^ Word size
   , platformByteOrder                :: !ByteOrder        -- ^ Byte order (endianness)
   , platformUnregisterised           :: !Bool
   , platformHasGnuNonexecStack       :: !Bool
   , platformHasIdentDirective        :: !Bool
   , platformHasSubsectionsViaSymbols :: !Bool
   , platformIsCrossCompiling         :: !Bool
   , platformLeadingUnderscore        :: !Bool             -- ^ Symbols need underscore prefix
   , platformTablesNextToCode         :: !Bool
      -- ^ Determines whether we will be compiling info tables that reside just
      --   before the entry code, or with an indirection to the entry code. See
      --   TABLES_NEXT_TO_CODE in includes/rts/storage/InfoTables.h.
   }
   deriving (Read, Show, Eq)

data PlatformWordSize
  = PW4 -- ^ A 32-bit platform
  | PW8 -- ^ A 64-bit platform
  deriving (Eq)

instance Show PlatformWordSize where
  show PW4 = "4"
  show PW8 = "8"

instance Read PlatformWordSize where
  readPrec = do
    i :: Int <- readPrec
    case i of
      4 -> return PW4
      8 -> return PW8
      other -> fail ("Invalid PlatformWordSize: " ++ show other)

platformWordSizeInBytes :: Platform -> Int
platformWordSizeInBytes p =
    case platformWordSize p of
      PW4 -> 4
      PW8 -> 8

platformWordSizeInBits :: Platform -> Int
platformWordSizeInBits p = platformWordSizeInBytes p * 8

-- | Legacy accessor
platformArch :: Platform -> Arch
platformArch = platformMini_arch . platformMini

-- | Legacy accessor
platformOS :: Platform -> OS
platformOS = platformMini_os . platformMini

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
        | ArchS390X
        | ArchSPARC
        | ArchSPARC64
        | ArchARM
          { armISA    :: ArmISA
          , armISAExt :: [ArmISAExt]
          , armABI    :: ArmABI
          }
        | ArchAArch64
        | ArchAlpha
        | ArchMipseb
        | ArchMipsel
        | ArchJavaScript
        deriving (Read, Show, Eq)

-- Note [Platform Syntax]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- There is a very loose encoding of platforms shared by many tools we are
-- encoding to here. GNU Config (http://git.savannah.gnu.org/cgit/config.git),
-- and LLVM's http://llvm.org/doxygen/classllvm_1_1Triple.html are perhaps the
-- most definitional parsers. The basic syntax is a list of '-'-separated
-- components. The Unix 'uname' command syntax is related but briefer.
--
-- Those two parsers are quite forgiving, and even the 'config.sub'
-- normalization is forgiving too. The "best" way to encode a platform is
-- therefore somewhat a matter of taste.
--
-- The 'stringEncode*' functions here convert each part of GHC's structured
-- notion of a platform into one dash-separated component.

-- | See Note [Platform Syntax].
stringEncodeArch :: Arch -> String
stringEncodeArch = \case
  ArchUnknown -> "unknown"
  ArchX86 -> "i386"
  ArchX86_64 -> "x86_64"
  ArchPPC -> "powerpc"
  ArchPPC_64 { ppc_64ABI = abi } -> case abi of
    ELF_V1 -> "powerpc64"
    ELF_V2 -> "powerpc64le"
  ArchS390X -> "s390x"
  ArchSPARC -> "sparc"
  ArchSPARC64 -> "sparc64"
  ArchARM { armISA = isa, armISAExt = _, armABI = _ } -> "arm" ++ vsuf
    where
      vsuf = case isa of
        ARMv5 -> "v5"
        ARMv6 -> "v6"
        ARMv7 -> "v7"
  ArchAArch64 -> "aarch64"
  ArchAlpha -> "alpha"
  ArchMipseb -> "mipseb"
  ArchMipsel -> "mipsel"
  ArchJavaScript -> "js"

isARM :: Arch -> Bool
isARM (ArchARM {}) = True
isARM ArchAArch64  = True
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

-- | See Note [Platform Syntax].
stringEncodeOS :: OS -> String
stringEncodeOS = \case
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
target32Bit p =
    case platformWordSize p of
      PW4 -> True
      PW8 -> False

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

-- | Platform-specific settings formerly hard-coded in Config.hs.
--
-- These should probably be all be triaged whether they can be computed from
-- other settings or belong in another another place (like 'Platform' above).
data PlatformMisc = PlatformMisc
  { -- TODO Recalculate string from richer info?
    platformMisc_targetPlatformString :: String
  , platformMisc_ghcWithInterpreter   :: Bool
  , platformMisc_ghcWithSMP           :: Bool
  , platformMisc_ghcRTSWays           :: String
  , platformMisc_libFFI               :: Bool
  , platformMisc_ghcThreaded          :: Bool
  , platformMisc_ghcDebugged          :: Bool
  , platformMisc_ghcRtsWithLibdw      :: Bool
  , platformMisc_llvmTarget           :: String
  }

-- | Minimum representable Int value for the given platform
platformMinInt :: Platform -> Integer
platformMinInt p = case platformWordSize p of
   PW4 -> toInteger (minBound :: Int32)
   PW8 -> toInteger (minBound :: Int64)

-- | Maximum representable Int value for the given platform
platformMaxInt :: Platform -> Integer
platformMaxInt p = case platformWordSize p of
   PW4 -> toInteger (maxBound :: Int32)
   PW8 -> toInteger (maxBound :: Int64)

-- | Maximum representable Word value for the given platform
platformMaxWord :: Platform -> Integer
platformMaxWord p = case platformWordSize p of
   PW4 -> toInteger (maxBound :: Word32)
   PW8 -> toInteger (maxBound :: Word64)

-- | Test if the given Integer is representable with a platform Int
platformInIntRange :: Platform -> Integer -> Bool
platformInIntRange platform x = x >= platformMinInt platform && x <= platformMaxInt platform

-- | Test if the given Integer is representable with a platform Word
platformInWordRange :: Platform -> Integer -> Bool
platformInWordRange platform x = x >= 0 && x <= platformMaxWord platform

-- | For some architectures the C calling convention is that any
-- integer shorter than 64 bits is replaced by its 64 bits
-- representation using sign or zero extension.
platformCConvNeedsExtension :: Platform -> Bool
platformCConvNeedsExtension platform = case platformArch platform of
  ArchPPC_64 _ -> True
  ArchS390X    -> True
  _            -> False


--------------------------------------------------
-- Instruction sets
--------------------------------------------------

-- | x86 SSE instructions
data SseVersion
   = SSE1
   | SSE2
   | SSE3
   | SSE4
   | SSE42
   deriving (Eq, Ord)

-- | x86 BMI (bit manipulation) instructions
data BmiVersion
   = BMI1
   | BMI2
   deriving (Eq, Ord)

