{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This should be a much more sophisticated abstraction than it is. Currently
-- it's just a bit of data about the compiler, like it's flavour and name and
-- version. The reason it's just data is because currently it has to be in
-- 'Read' and 'Show' so it can be saved along with the 'LocalBuildInfo'. The
-- only interesting bit of info it contains is a mapping between language
-- extensions and compiler command line flags. This module also defines a
-- 'PackageDB' type which is used to refer to package databases. Most compilers
-- only know about a single global package collection but GHC has a global and
-- per-user one and it lets you create arbitrary other package databases. We do
-- not yet fully support this latter feature.

module Distribution.Simple.Compiler (
        -- * Haskell implementations
        module Distribution.Compiler,
        Compiler(..),
        showCompilerId, showCompilerIdWithAbi,
        compilerFlavor, compilerVersion,
        compilerCompatFlavor,
        compilerCompatVersion,
        compilerInfo,

        -- * Support for package databases
        PackageDB(..),
        PackageDBStack,
        registrationPackageDB,
        absolutePackageDBPaths,
        absolutePackageDBPath,

        -- * Support for optimisation levels
        OptimisationLevel(..),
        flagToOptimisationLevel,

        -- * Support for debug info levels
        DebugInfoLevel(..),
        flagToDebugInfoLevel,

        -- * Support for language extensions
        Flag,
        languageToFlags,
        unsupportedLanguages,
        extensionsToFlags,
        unsupportedExtensions,
        parmakeSupported,
        reexportedModulesSupported,
        renamingPackageFlagsSupported,
        unifiedIPIDRequired,
        packageKeySupported,
        unitIdSupported,
        coverageSupported,
        profilingSupported,
        backpackSupported,
        arResponseFilesSupported,
        libraryDynDirSupported,

        -- * Support for profiling detail levels
        ProfDetailLevel(..),
        knownProfDetailLevels,
        flagToProfDetailLevel,
        showProfDetailLevel,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compiler
import Distribution.Version
import Distribution.Text
import Language.Haskell.Extension
import Distribution.Simple.Utils

import qualified Data.Map as Map (lookup)
import System.Directory (canonicalizePath)

data Compiler = Compiler {
        compilerId              :: CompilerId,
        -- ^ Compiler flavour and version.
        compilerAbiTag          :: AbiTag,
        -- ^ Tag for distinguishing incompatible ABI's on the same
        -- architecture/os.
        compilerCompat          :: [CompilerId],
        -- ^ Other implementations that this compiler claims to be
        -- compatible with.
        compilerLanguages       :: [(Language, Flag)],
        -- ^ Supported language standards.
        compilerExtensions      :: [(Extension, Flag)],
        -- ^ Supported extensions.
        compilerProperties      :: Map String String
        -- ^ A key-value map for properties not covered by the above fields.
    }
    deriving (Eq, Generic, Typeable, Show, Read)

instance Binary Compiler

showCompilerId :: Compiler -> String
showCompilerId = display . compilerId

showCompilerIdWithAbi :: Compiler -> String
showCompilerIdWithAbi comp =
  display (compilerId comp) ++
  case compilerAbiTag comp of
    NoAbiTag  -> []
    AbiTag xs -> '-':xs

compilerFlavor ::  Compiler -> CompilerFlavor
compilerFlavor = (\(CompilerId f _) -> f) . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = (\(CompilerId _ v) -> v) . compilerId


-- | Is this compiler compatible with the compiler flavour we're interested in?
--
-- For example this checks if the compiler is actually GHC or is another
-- compiler that claims to be compatible with some version of GHC, e.g. GHCJS.
--
-- > if compilerCompatFlavor GHC compiler then ... else ...
--
compilerCompatFlavor :: CompilerFlavor -> Compiler -> Bool
compilerCompatFlavor flavor comp =
    flavor == compilerFlavor comp
 || flavor `elem` [ flavor' | CompilerId flavor' _ <- compilerCompat comp ]


-- | Is this compiler compatible with the compiler flavour we're interested in,
-- and if so what version does it claim to be compatible with.
--
-- For example this checks if the compiler is actually GHC-7.x or is another
-- compiler that claims to be compatible with some GHC-7.x version.
--
-- > case compilerCompatVersion GHC compiler of
-- >   Just (Version (7:_)) -> ...
-- >   _                    -> ...
--
compilerCompatVersion :: CompilerFlavor -> Compiler -> Maybe Version
compilerCompatVersion flavor comp
  | compilerFlavor comp == flavor = Just (compilerVersion comp)
  | otherwise    =
      listToMaybe [ v | CompilerId fl v <- compilerCompat comp, fl == flavor ]

compilerInfo :: Compiler -> CompilerInfo
compilerInfo c = CompilerInfo (compilerId c)
                              (compilerAbiTag c)
                              (Just . compilerCompat $ c)
                              (Just . map fst . compilerLanguages $ c)
                              (Just . map fst . compilerExtensions $ c)

-- ------------------------------------------------------------
-- * Package databases
-- ------------------------------------------------------------

-- |Some compilers have a notion of a database of available packages.
-- For some there is just one global db of packages, other compilers
-- support a per-user or an arbitrary db specified at some location in
-- the file system. This can be used to build isloated environments of
-- packages, for example to build a collection of related packages
-- without installing them globally.
--
data PackageDB = GlobalPackageDB
               | UserPackageDB
               | SpecificPackageDB FilePath
    deriving (Eq, Generic, Ord, Show, Read)

instance Binary PackageDB

-- | We typically get packages from several databases, and stack them
-- together. This type lets us be explicit about that stacking. For example
-- typical stacks include:
--
-- > [GlobalPackageDB]
-- > [GlobalPackageDB, UserPackageDB]
-- > [GlobalPackageDB, SpecificPackageDB "package.conf.inplace"]
--
-- Note that the 'GlobalPackageDB' is invariably at the bottom since it
-- contains the rts, base and other special compiler-specific packages.
--
-- We are not restricted to using just the above combinations. In particular
-- we can use several custom package dbs and the user package db together.
--
-- When it comes to writing, the top most (last) package is used.
--
type PackageDBStack = [PackageDB]

-- | Return the package that we should register into. This is the package db at
-- the top of the stack.
--
registrationPackageDB :: PackageDBStack -> PackageDB
registrationPackageDB []  = error "internal error: empty package db set"
registrationPackageDB dbs = last dbs

-- | Make package paths absolute


absolutePackageDBPaths :: PackageDBStack -> NoCallStackIO PackageDBStack
absolutePackageDBPaths = traverse absolutePackageDBPath

absolutePackageDBPath :: PackageDB -> NoCallStackIO PackageDB
absolutePackageDBPath GlobalPackageDB        = return GlobalPackageDB
absolutePackageDBPath UserPackageDB          = return UserPackageDB
absolutePackageDBPath (SpecificPackageDB db) =
  SpecificPackageDB `liftM` canonicalizePath db

-- ------------------------------------------------------------
-- * Optimisation levels
-- ------------------------------------------------------------

-- | Some compilers support optimising. Some have different levels.
-- For compilers that do not the level is just capped to the level
-- they do support.
--
data OptimisationLevel = NoOptimisation
                       | NormalOptimisation
                       | MaximumOptimisation
    deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary OptimisationLevel

flagToOptimisationLevel :: Maybe String -> OptimisationLevel
flagToOptimisationLevel Nothing  = NormalOptimisation
flagToOptimisationLevel (Just s) = case reads s of
  [(i, "")]
    | i >= fromEnum (minBound :: OptimisationLevel)
   && i <= fromEnum (maxBound :: OptimisationLevel)
                -> toEnum i
    | otherwise -> error $ "Bad optimisation level: " ++ show i
                        ++ ". Valid values are 0..2"
  _             -> error $ "Can't parse optimisation level " ++ s

-- ------------------------------------------------------------
-- * Debug info levels
-- ------------------------------------------------------------

-- | Some compilers support emitting debug info. Some have different
-- levels.  For compilers that do not the level is just capped to the
-- level they do support.
--
data DebugInfoLevel = NoDebugInfo
                    | MinimalDebugInfo
                    | NormalDebugInfo
                    | MaximalDebugInfo
    deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary DebugInfoLevel

flagToDebugInfoLevel :: Maybe String -> DebugInfoLevel
flagToDebugInfoLevel Nothing  = NormalDebugInfo
flagToDebugInfoLevel (Just s) = case reads s of
  [(i, "")]
    | i >= fromEnum (minBound :: DebugInfoLevel)
   && i <= fromEnum (maxBound :: DebugInfoLevel)
                -> toEnum i
    | otherwise -> error $ "Bad debug info level: " ++ show i
                        ++ ". Valid values are 0..3"
  _             -> error $ "Can't parse debug info level " ++ s

-- ------------------------------------------------------------
-- * Languages and Extensions
-- ------------------------------------------------------------

unsupportedLanguages :: Compiler -> [Language] -> [Language]
unsupportedLanguages comp langs =
  [ lang | lang <- langs
         , isNothing (languageToFlag comp lang) ]

languageToFlags :: Compiler -> Maybe Language -> [Flag]
languageToFlags comp = filter (not . null)
                     . catMaybes . map (languageToFlag comp)
                     . maybe [Haskell98] (\x->[x])

languageToFlag :: Compiler -> Language -> Maybe Flag
languageToFlag comp ext = lookup ext (compilerLanguages comp)


-- |For the given compiler, return the extensions it does not support.
unsupportedExtensions :: Compiler -> [Extension] -> [Extension]
unsupportedExtensions comp exts =
  [ ext | ext <- exts
        , isNothing (extensionToFlag comp ext) ]

type Flag = String

-- |For the given compiler, return the flags for the supported extensions.
extensionsToFlags :: Compiler -> [Extension] -> [Flag]
extensionsToFlags comp = nub . filter (not . null)
                       . catMaybes . map (extensionToFlag comp)

extensionToFlag :: Compiler -> Extension -> Maybe Flag
extensionToFlag comp ext = lookup ext (compilerExtensions comp)

-- | Does this compiler support parallel --make mode?
parmakeSupported :: Compiler -> Bool
parmakeSupported = ghcSupported "Support parallel --make"

-- | Does this compiler support reexported-modules?
reexportedModulesSupported :: Compiler -> Bool
reexportedModulesSupported = ghcSupported "Support reexported-modules"

-- | Does this compiler support thinning/renaming on package flags?
renamingPackageFlagsSupported :: Compiler -> Bool
renamingPackageFlagsSupported = ghcSupported
  "Support thinning and renaming package flags"

-- | Does this compiler have unified IPIDs (so no package keys)
unifiedIPIDRequired :: Compiler -> Bool
unifiedIPIDRequired = ghcSupported "Requires unified installed package IDs"

-- | Does this compiler support package keys?
packageKeySupported :: Compiler -> Bool
packageKeySupported = ghcSupported "Uses package keys"

-- | Does this compiler support unit IDs?
unitIdSupported :: Compiler -> Bool
unitIdSupported = ghcSupported "Uses unit IDs"

-- | Does this compiler support Backpack?
backpackSupported :: Compiler -> Bool
backpackSupported = ghcSupported "Support Backpack"

-- | Does this compiler support a package database entry with:
-- "dynamic-library-dirs"?
libraryDynDirSupported :: Compiler -> Bool
libraryDynDirSupported comp = case compilerFlavor comp of
  GHC ->
      -- Not just v >= mkVersion [8,0,1,20161022], as there
      -- are many GHC 8.1 nightlies which don't support this.
    ((v >= mkVersion [8,0,1,20161022] && v < mkVersion [8,1]) ||
      v >= mkVersion [8,1,20161021])
  _   -> False
 where
  v = compilerVersion comp

-- | Does this compiler's "ar" command supports response file
-- arguments (i.e. @file-style arguments).
arResponseFilesSupported :: Compiler -> Bool
arResponseFilesSupported = ghcSupported "ar supports at file"

-- | Does this compiler support Haskell program coverage?
coverageSupported :: Compiler -> Bool
coverageSupported comp =
  case compilerFlavor comp of
    GHC   -> True
    GHCJS -> True
    _     -> False

-- | Does this compiler support profiling?
profilingSupported :: Compiler -> Bool
profilingSupported comp =
  case compilerFlavor comp of
    GHC   -> True
    GHCJS -> True
    LHC   -> True
    _     -> False

-- | Utility function for GHC only features
ghcSupported :: String -> Compiler -> Bool
ghcSupported key comp =
  case compilerFlavor comp of
    GHC   -> checkProp
    GHCJS -> checkProp
    _     -> False
  where checkProp =
          case Map.lookup key (compilerProperties comp) of
            Just "YES" -> True
            _          -> False

-- ------------------------------------------------------------
-- * Profiling detail level
-- ------------------------------------------------------------

-- | Some compilers (notably GHC) support profiling and can instrument
-- programs so the system can account costs to different functions. There are
-- different levels of detail that can be used for this accounting.
-- For compilers that do not support this notion or the particular detail
-- levels, this is either ignored or just capped to some similar level
-- they do support.
--
data ProfDetailLevel = ProfDetailNone
                     | ProfDetailDefault
                     | ProfDetailExportedFunctions
                     | ProfDetailToplevelFunctions
                     | ProfDetailAllFunctions
                     | ProfDetailOther String
    deriving (Eq, Generic, Read, Show)

instance Binary ProfDetailLevel

flagToProfDetailLevel :: String -> ProfDetailLevel
flagToProfDetailLevel "" = ProfDetailDefault
flagToProfDetailLevel s  =
    case lookup (lowercase s)
                [ (name, value)
                | (primary, aliases, value) <- knownProfDetailLevels
                , name <- primary : aliases ]
      of Just value -> value
         Nothing    -> ProfDetailOther s

knownProfDetailLevels :: [(String, [String], ProfDetailLevel)]
knownProfDetailLevels =
  [ ("default",            [],                  ProfDetailDefault)
  , ("none",               [],                  ProfDetailNone)
  , ("exported-functions", ["exported"],        ProfDetailExportedFunctions)
  , ("toplevel-functions", ["toplevel", "top"], ProfDetailToplevelFunctions)
  , ("all-functions",      ["all"],             ProfDetailAllFunctions)
  ]

showProfDetailLevel :: ProfDetailLevel -> String
showProfDetailLevel dl = case dl of
    ProfDetailNone              -> "none"
    ProfDetailDefault           -> "default"
    ProfDetailExportedFunctions -> "exported-functions"
    ProfDetailToplevelFunctions -> "toplevel-functions"
    ProfDetailAllFunctions      -> "all-functions"
    ProfDetailOther other       -> other

