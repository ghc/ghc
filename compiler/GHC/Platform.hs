{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Platform description
module GHC.Platform
   ( Platform (..)
   , PlatformWordSize(..)
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
   , platformHasRTSLinker
   , PlatformMisc(..)
   , SseVersion (..)
   , BmiVersion (..)
   , wordAlignment
   -- * SSE and AVX
   , isSseEnabled
   , isSse2Enabled
   -- * Platform constants
   , PlatformConstants(..)
   , lookupPlatformConstants
   , platformConstants
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
import GHC.Types.Basic (Alignment, alignmentOf)
import GHC.Utils.Panic.Plain

import Data.Word
import Data.Int
import System.FilePath
import System.Directory

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
      --   TABLES_NEXT_TO_CODE in @rts/include/rts/storage/InfoTables.h@.
   , platformHasLibm                  :: !Bool
      -- ^ Some platforms require that we explicitly link against @libm@ if any
      -- math-y things are used (which we assume to include all programs). See
      -- #14022.

   , platform_constants               :: !(Maybe PlatformConstants)
      -- ^ Constants such as structure offsets, type sizes, etc.
   }
   deriving (Read, Show, Eq, Ord)

wordAlignment :: Platform -> Alignment
wordAlignment platform = alignmentOf (platformWordSizeInBytes platform)

-- -----------------------------------------------------------------------------
-- SSE and AVX

-- TODO: Instead of using a separate predicate (i.e. isSse2Enabled) to
-- check if SSE is enabled, we might have x86-64 imply the -msse2
-- flag.

isSseEnabled :: Platform -> Bool
isSseEnabled platform = case platformArch platform of
    ArchX86_64 -> True
    ArchX86    -> True
    _          -> False

isSse2Enabled :: Platform -> Bool
isSse2Enabled platform = case platformArch platform of
  -- We assume  SSE1 and SSE2 operations are available on both
  -- x86 and x86_64. Historically we didn't default to SSE2 and
  -- SSE1 on x86, which results in defacto nondeterminism for how
  -- rounding behaves in the associated x87 floating point instructions
  -- because variations in the spill/fpu stack placement of arguments for
  -- operations would change the precision and final result of what
  -- would otherwise be the same expressions with respect to single or
  -- double precision IEEE floating point computations.
    ArchX86_64 -> True
    ArchX86    -> True
    _          -> False

-- -----------------------------------------------------------------------------
-- Platform Constants

platformConstants :: Platform -> PlatformConstants
platformConstants platform = case platform_constants platform of
  Nothing -> panic "Platform constants not available!"
  Just c  -> c

genericPlatform :: Platform
genericPlatform = Platform
   { platformArchOS                  = ArchOS ArchX86_64 OSLinux
   , platformWordSize                = PW8
   , platformByteOrder               = LittleEndian
   , platformUnregisterised          = False
   , platformHasGnuNonexecStack      = False
   , platformHasIdentDirective       = False
   , platformHasSubsectionsViaSymbols= False
   , platformHasLibm                 = False
   , platformIsCrossCompiling        = False
   , platformLeadingUnderscore       = False
   , platformTablesNextToCode        = True
   , platform_constants               = Nothing
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

-- | This predicate tells us whether the platform is 32-bit.
target32Bit :: Platform -> Bool
target32Bit p =
    case platformWordSize p of
      PW4 -> True
      PW8 -> False

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
  ArchRISCV64  -> True
  ArchLoongArch64 -> True
  ArchAArch64
      -- Apple's AArch64 ABI requires that the caller sign-extend
      -- small integer arguments. See
      -- https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms
    | OSDarwin <- platformOS platform -> True
  _            -> False

-- | Does this platform have an RTS linker?
platformHasRTSLinker :: Platform -> Bool
-- Note that we've inlined this logic in hadrian's
-- Settings.Builders.RunTest.inTreeCompilerArgs.
-- If you change this, be sure to change it too
platformHasRTSLinker p = case archOS_arch (platformArchOS p) of
  ArchPPC           -> False -- powerpc
  ArchPPC_64 ELF_V1 -> False -- powerpc64
  ArchPPC_64 ELF_V2 -> False -- powerpc64le
  ArchS390X         -> False
  ArchRISCV64       -> False
  ArchLoongArch64   -> False
  ArchJavaScript    -> False
  ArchWasm32        -> False
  _                 -> True



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
  , platformMisc_libFFI               :: Bool
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

-- Note [Platform constants]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The RTS is partly written in C, hence we use an external C compiler to build
-- it. Thus GHC must somehow retrieve some information about the produced code
-- (sizes of types, offsets of struct fields, etc.) to produce compatible code.
--
-- This is the role of utils/deriveConstants utility: it produces a C
-- source, compiles it with the same toolchain that will be used to build the
-- RTS, and finally retrieves the constants from the built artefact. We can't
-- directly run the produced program because we may be cross-compiling.
--
-- These constants are then stored in DerivedConstants.h header file that is
-- bundled with the RTS unit. This file is directly imported by Cmm codes and it
-- is also read by GHC. deriveConstants also produces the Haskell definition of
-- the PlatformConstants datatype and the Haskell parser for the
-- DerivedConstants.h file.
--
-- For quite some time, constants used by GHC were globally installed in
-- ${libdir}/platformConstants but now GHC reads the DerivedConstants.h header
-- bundled with the RTS unit. GHC detects when it builds the RTS unit itself and
-- in this case it loads the header from the include-dirs passed on the
-- command-line.
--
-- Note that GHC doesn't parse every "#define SOME_CONSTANT 123" individually.
-- Instead there is a single #define that contains all the constants useful to
-- GHC in a comma separated list:
--
--    #define HS_CONSTANTS "123,45,..."
--
-- Note that GHC mustn't directly import DerivedConstants.h as these constants
-- are only valid for a specific target platform and we want GHC to be target
-- agnostic.
--


-- | Try to locate "DerivedConstants.h" file in the given dirs and to parse the
-- PlatformConstants from it.
--
-- See Note [Platform constants]
lookupPlatformConstants :: [FilePath] -> IO (Maybe PlatformConstants)
lookupPlatformConstants include_dirs = find_constants include_dirs
  where
    try_parse d = do
        let p = d </> "DerivedConstants.h"
        doesFileExist p >>= \case
          True  -> Just <$> parseConstantsHeader p
          False -> return Nothing

    find_constants []     = return Nothing
    find_constants (x:xs) = try_parse x >>= \case
        Nothing -> find_constants xs
        Just c  -> return (Just c)
