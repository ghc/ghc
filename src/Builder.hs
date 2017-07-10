{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Builder (
    CcMode (..), GhcMode (..), GhcPkgMode (..), Builder (..),
    trackedArgument, isOptional
    ) where

import Data.Char
import GHC.Generics

import Base
import Stage

-- | A compiler can typically be used in different modes:
-- * Compiling or preprocessing a source file;
-- * Extracting source dependencies, e.g. by passing @-M@ command line argument;
-- * Linking object files & static libraries into an executable.
-- We have CcMode for C compiler and GhcMode for GHC.
data CcMode  = CompileC  | FindCDependencies deriving (Eq, Generic, Show)
data GhcMode = CompileHs | FindHsDependencies | LinkHs
    deriving (Eq, Generic, Show)

-- | GhcPkg can initialise a package database and register packages in it.
data GhcPkgMode = Init | Update deriving (Eq, Generic, Show)

-- | A 'Builder' is an external command invoked in a separate process via 'cmd'.
-- @Ghc Stage0@ is the bootstrapping compiler.
-- @Ghc StageN@, N > 0, is the one built in stage (N - 1).
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@.
-- @GhcPkg Stage1@ is the one built in Stage0.
data Builder = Alex
             | Ar Stage
             | DeriveConstants
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcCabal
             | GhcCabalHsColour   -- synonym for 'GhcCabal hscolour'
             | GhcPkg GhcPkgMode Stage
             | Haddock
             | Happy
             | Hpc
             | HsColour
             | HsCpp
             | Hsc2Hs
             | Ld
             | Make FilePath
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Tar
             | Unlit
             deriving (Eq, Generic, Show)

-- TODO: Some builders are required only on certain platforms. For example,
-- Objdump is only required on OpenBSD and AIX, as mentioned in #211. Add
-- support for platform-specific optional builders as soon as we can reliably
-- test this feature.
isOptional :: Builder -> Bool
isOptional = \case
    HsColour -> True
    Objdump  -> True
    _        -> False

-- | Some arguments do not affect build results and therefore do not need to be
-- tracked by the build system. A notable example is "-jN" that controls Make's
-- parallelism. Given a 'Builder' and an argument, this function should return
-- 'True' only if the argument needs to be tracked.
trackedArgument :: Builder -> String -> Bool
trackedArgument (Make _) = not . threadArg
trackedArgument _        = const True

threadArg :: String -> Bool
threadArg s = dropWhileEnd isDigit s `elem` ["-j", "MAKEFLAGS=-j", "THREADS="]

instance Binary Builder
instance Hashable Builder
instance NFData Builder

instance Binary CcMode
instance Hashable CcMode
instance NFData CcMode

instance Binary GhcMode
instance Hashable GhcMode
instance NFData GhcMode

instance Binary GhcPkgMode
instance Hashable GhcPkgMode
instance NFData GhcPkgMode
