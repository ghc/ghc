{-# LANGUAGE ScopedTypeVariables #-}

-- | Platform description
module GHC.Platform
   ( Platform (..)
   , PlatformWordSize(..)
   , PlatformConstants(..)
   , platformArch
   , platformOS
   , ArchOS(..)
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
   , SseVersion (..)
   , BmiVersion (..)
   -- * Shared libraries
   , platformSOName
   , platformHsSOName
   , platformSOExt
   , genericPlatform
   )
where

import Prelude -- See Note [Why do we import Prelude here?]

import GHC.Read
import GHC.ByteOrder (ByteOrder(..))
import GHC.Platform.Constants
import GHC.Platform.ArchOS

import Data.Word
import Data.Int
import System.FilePath

-- | Platform description
--
-- This is used to describe platforms so that we can generate code for them.
data Platform = Platform
   { platformArchOS                   :: !ArchOS           -- ^ Architecture and OS
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
   , platformConstants                :: PlatformConstants
      -- ^ Constants such as structure offsets, type sizes, etc.
   }
   deriving (Read, Show, Eq)

genericPlatform :: Platform
genericPlatform = Platform
   { platformArchOS                  = ArchOS ArchX86_64 OSLinux
   , platformWordSize                = PW8
   , platformByteOrder               = LittleEndian
   , platformUnregisterised          = False
   , platformHasGnuNonexecStack      = False
   , platformHasIdentDirective       = False
   , platformHasSubsectionsViaSymbols= False
   , platformIsCrossCompiling        = False
   , platformLeadingUnderscore       = False
   , platformTablesNextToCode        = True
   , platformConstants               = error "No PlatformConstants"
   }

data PlatformWordSize
  = PW4 -- ^ A 32-bit platform
  | PW8 -- ^ A 64-bit platform
  deriving (Eq, Ord)

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

-- | Platform architecture
platformArch :: Platform -> Arch
platformArch platform = case platformArchOS platform of
   ArchOS arch _ -> arch

-- | Platform OS
platformOS :: Platform -> OS
platformOS platform = case platformArchOS platform of
   ArchOS _ os -> os

isARM :: Arch -> Bool
isARM (ArchARM {}) = True
isARM ArchAArch64  = True
isARM _ = False

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
  , platformMisc_ghcRtsWithLibdw      :: Bool
  , platformMisc_llvmTarget           :: String
  }

platformSOName :: Platform -> FilePath -> FilePath
platformSOName platform root = case platformOS platform of
   OSMinGW32 ->           root  <.> platformSOExt platform
   _         -> ("lib" ++ root) <.> platformSOExt platform

platformHsSOName :: Platform -> FilePath -> FilePath
platformHsSOName platform root = ("lib" ++ root) <.> platformSOExt platform

platformSOExt :: Platform -> FilePath
platformSOExt platform
    = case platformOS platform of
      OSDarwin  -> "dylib"
      OSMinGW32 -> "dll"
      _         -> "so"
