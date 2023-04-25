module GHC.Toolchain.Target where

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Cxx
import GHC.Toolchain.Tools.Cpp
import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Ranlib
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Nm
import GHC.Toolchain.Tools.MergeObjs

data WordSize = WS4 | WS8
    deriving (Show, Read)

data Endianness = LittleEndian | BigEndian
    deriving (Show, Read)

-- | A 'Target' consists of:
--
-- * a target architecture and operating system
-- * various bits of information about the platform
-- * various toolchain components targetting that platform
--
data Target = Target
    { -- Platform
      tgtArchOs :: ArchOS
    -- , tgtCrossCompiling :: Bool -- TODO: Rename hostCanExecute?
    , tgtSupportsGnuNonexecStack :: Bool
    , tgtSupportsSubsectionsViaSymbols :: Bool
    , tgtSupportsIdentDirective :: Bool
    , tgtWordSize :: WordSize
    , tgtEndianness :: Endianness
    , tgtSymbolsHaveLeadingUnderscore :: Bool
    , tgtLlvmTarget :: String

      -- GHC capabilities
    , tgtUnregisterised :: Bool
    , tgtTablesNextToCode :: Bool
    -- , tgtHasRtsLinker :: Bool -- Hmm?
    -- , tgtHasThreadedRts :: Bool
    -- , tgtUseLibffi :: Bool

      -- C toolchain
    , tgtCCompiler :: Cc
    , tgtCxxCompiler :: Cxx
    , tgtCPreprocessor :: Cpp
    , tgtCCompilerLink :: CcLink
    -- , tgtLd :: Program -- needed?
    -- , tgtLdSupportsCompactUnwind :: Bool
    -- , tgtLdSupportsFilelist :: Bool
    -- , tgtLdIsGnuLd :: Bool -- needed?
    , tgtAr :: Ar
    , tgtRanlib :: Maybe Ranlib
    , tgtNm :: Nm
    , tgtMergeObjs :: Maybe MergeObjs

      -- Windows-specific tools
    , tgtDllwrap :: Maybe Program
    , tgtWindres :: Maybe Program
    }
    deriving (Show, Read)
