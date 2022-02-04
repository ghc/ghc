{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

-- | Platform architecture and OS
--
-- We need it in ghc-boot because ghc-pkg needs it.
module GHC.Platform.ArchOS
   ( ArchOS(..)
   , Arch(..)
   , OS(..)
   , ArmISA(..)
   , ArmISAExt(..)
   , ArmABI(..)
   , PPC_64ABI(..)
   , stringEncodeArch
   , stringEncodeOS
   )
where

import Prelude -- See Note [Why do we import Prelude here?]

-- | Platform architecture and OS.
data ArchOS
   = ArchOS
      { archOS_arch :: Arch
      , archOS_OS   :: OS
      }
   deriving (Read, Show, Eq, Ord)

-- | Architectures
--
-- TODO: It might be nice to extend these constructors with information about
-- what instruction set extensions an architecture might support.
--
data Arch
   = ArchUnknown
   | ArchX86
   | ArchX86_64
   | ArchPPC
   | ArchPPC_64 PPC_64ABI
   | ArchS390X
   | ArchARM ArmISA [ArmISAExt] ArmABI
   | ArchAArch64
   | ArchAlpha
   | ArchMipseb
   | ArchMipsel
   | ArchRISCV64
   | ArchJavaScript
   deriving (Read, Show, Eq, Ord)

-- | ARM Instruction Set Architecture
data ArmISA
   = ARMv5
   | ARMv6
   | ARMv7
   deriving (Read, Show, Eq, Ord)

-- | ARM extensions
data ArmISAExt
   = VFPv2
   | VFPv3
   | VFPv3D16
   | NEON
   | IWMMX2
   deriving (Read, Show, Eq, Ord)

-- | ARM ABI
data ArmABI
   = SOFT
   | SOFTFP
   | HARD
   deriving (Read, Show, Eq, Ord)

-- | PowerPC 64-bit ABI
data PPC_64ABI
   = ELF_V1 -- ^ PowerPC64
   | ELF_V2 -- ^ PowerPC64 LE
   deriving (Read, Show, Eq, Ord)

-- | Operating systems.
--
-- Using OSUnknown to generate code should produce a sensible default, but no
-- promises.
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
   deriving (Read, Show, Eq, Ord)


-- Note [Platform Syntax]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
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
  ArchUnknown       -> "unknown"
  ArchX86           -> "i386"
  ArchX86_64        -> "x86_64"
  ArchPPC           -> "powerpc"
  ArchPPC_64 ELF_V1 -> "powerpc64"
  ArchPPC_64 ELF_V2 -> "powerpc64le"
  ArchS390X         -> "s390x"
  ArchARM ARMv5 _ _ -> "armv5"
  ArchARM ARMv6 _ _ -> "armv6"
  ArchARM ARMv7 _ _ -> "armv7"
  ArchAArch64       -> "aarch64"
  ArchAlpha         -> "alpha"
  ArchMipseb        -> "mipseb"
  ArchMipsel        -> "mipsel"
  ArchRISCV64       -> "riscv64"
  ArchJavaScript    -> "js"

-- | See Note [Platform Syntax].
stringEncodeOS :: OS -> String
stringEncodeOS = \case
  OSUnknown   -> "unknown"
  OSLinux     -> "linux"
  OSDarwin    -> "darwin"
  OSSolaris2  -> "solaris2"
  OSMinGW32   -> "mingw32"
  OSFreeBSD   -> "freebsd"
  OSDragonFly -> "dragonfly"
  OSOpenBSD   -> "openbsd"
  OSNetBSD    -> "netbsd"
  OSKFreeBSD  -> "kfreebsdgnu"
  OSHaiku     -> "haiku"
  OSQNXNTO    -> "nto-qnx"
  OSAIX       -> "aix"
  OSHurd      -> "hurd"
