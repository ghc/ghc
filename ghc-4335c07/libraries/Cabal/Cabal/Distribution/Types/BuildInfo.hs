{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BuildInfo (
    BuildInfo(..),

    emptyBuildInfo,
    allLanguages,
    allExtensions,
    usedExtensions,
    usesTemplateHaskellOrQQ,

    hcOptions,
    hcProfOptions,
    hcSharedOptions,
    hcStaticOptions,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Mixin
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PkgconfigDependency

import Distribution.ModuleName
import Distribution.Compiler
import Language.Haskell.Extension

-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        -- | component is buildable here
        buildable         :: Bool,
        -- | Tools needed to build this bit.
        --
        -- This is a legacy field that 'buildToolDepends' larely supersedes.
        --
        -- Unless use are very sure what you are doing, use the functions in
        -- "Distribution.Simple.BuildToolDepends" rather than accessing this
        -- field directly.
        buildTools        :: [LegacyExeDependency],
        -- | Haskell tools needed to build this bit
        --
        -- This field is better than 'buildTools' because it allows one to
        -- precisely specify an executable in a package.
        --
        -- Unless use are very sure what you are doing, use the functions in
        -- "Distribution.Simple.BuildToolDepends" rather than accessing this
        -- field directly.
        buildToolDepends  :: [ExeDependency],
        cppOptions        :: [String],  -- ^ options for pre-processing Haskell code
        asmOptions        :: [String],  -- ^ options for assmebler
        cmmOptions        :: [String],  -- ^ options for C-- compiler
        ccOptions         :: [String],  -- ^ options for C compiler
        cxxOptions        :: [String],  -- ^ options for C++ compiler
        ldOptions         :: [String],  -- ^ options for linker
        pkgconfigDepends  :: [PkgconfigDependency], -- ^ pkg-config packages that are used
        frameworks        :: [String], -- ^support frameworks for Mac OS X
        extraFrameworkDirs:: [String], -- ^ extra locations to find frameworks.
        asmSources        :: [FilePath], -- ^ Assembly files.
        cmmSources        :: [FilePath], -- ^ C-- files.
        cSources          :: [FilePath],
        cxxSources        :: [FilePath],
        jsSources         :: [FilePath],
        hsSourceDirs      :: [FilePath], -- ^ where to look for the Haskell module hierarchy
        otherModules      :: [ModuleName], -- ^ non-exposed or non-main modules
        virtualModules    :: [ModuleName], -- ^ exposed modules that do not have a source file (e.g. @GHC.Prim@ from @ghc-prim@ package)
        autogenModules    :: [ModuleName], -- ^ not present on sdist, Paths_* or user-generated with a custom Setup.hs

        defaultLanguage   :: Maybe Language,-- ^ language used when not explicitly specified
        otherLanguages    :: [Language],    -- ^ other languages used within the package
        defaultExtensions :: [Extension],   -- ^ language extensions used by all modules
        otherExtensions   :: [Extension],   -- ^ other language extensions used within the package
        oldExtensions     :: [Extension],   -- ^ the old extensions field, treated same as 'defaultExtensions'

        extraLibs         :: [String], -- ^ what libraries to link with when compiling a program that uses your package
        extraGHCiLibs     :: [String], -- ^ if present, overrides extraLibs when package is loaded with GHCi.
        extraBundledLibs  :: [String], -- ^ if present, adds libs to hs-lirbaries, which become part of the package.
                                       --   Example: the Cffi library shipping with the rts, alognside the HSrts-1.0.a,.o,...
                                       --   Example 2: a library that is being built by a foreing tool (e.g. rust)
                                       --              and copied and registered together with this library.  The
                                       --              logic on how this library is built will have to be encoded in a
                                       --              custom Setup for now.  Oherwise cabal would need to lear how to
                                       --              call arbitary lirbary builders.
        extraLibFlavours  :: [String], -- ^ Hidden Flag.  This set of strings, will be appended to all lirbaries when
                                       --   copying. E.g. [libHS<name>_<flavour> | flavour <- extraLibFlavours]. This
                                       --   should only be needed in very specific cases, e.g. the `rts` package, where
                                       --   there are multiple copies of slightly differently built libs.
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath], -- ^directories to find .h files
        includes          :: [FilePath], -- ^ The .h files to be found in includeDirs
        installIncludes   :: [FilePath], -- ^ .h files to install with the package
        options           :: [(CompilerFlavor,[String])],
        profOptions       :: [(CompilerFlavor,[String])],
        sharedOptions     :: [(CompilerFlavor,[String])],
        staticOptions     :: [(CompilerFlavor,[String])],
        customFieldsBI    :: [(String,String)], -- ^Custom fields starting
                                                -- with x-, stored in a
                                                -- simple assoc-list.
        targetBuildDepends :: [Dependency], -- ^ Dependencies specific to a library or executable target
        mixins :: [Mixin]
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BuildInfo

instance Monoid BuildInfo where
  mempty = BuildInfo {
    buildable           = True,
    buildTools          = [],
    buildToolDepends    = [],
    cppOptions          = [],
    asmOptions          = [],
    cmmOptions          = [],
    ccOptions           = [],
    cxxOptions          = [],
    ldOptions           = [],
    pkgconfigDepends    = [],
    frameworks          = [],
    extraFrameworkDirs  = [],
    asmSources          = [],
    cmmSources          = [],
    cSources            = [],
    cxxSources          = [],
    jsSources           = [],
    hsSourceDirs        = [],
    otherModules        = [],
    virtualModules      = [],
    autogenModules      = [],
    defaultLanguage     = Nothing,
    otherLanguages      = [],
    defaultExtensions   = [],
    otherExtensions     = [],
    oldExtensions       = [],
    extraLibs           = [],
    extraGHCiLibs       = [],
    extraBundledLibs    = [],
    extraLibFlavours    = [],
    extraLibDirs        = [],
    includeDirs         = [],
    includes            = [],
    installIncludes     = [],
    options             = [],
    profOptions         = [],
    sharedOptions       = [],
    staticOptions       = [],
    customFieldsBI      = [],
    targetBuildDepends  = [],
    mixins    = []
  }
  mappend = (<>)

instance Semigroup BuildInfo where
  a <> b = BuildInfo {
    buildable           = buildable a && buildable b,
    buildTools          = combine    buildTools,
    buildToolDepends    = combine    buildToolDepends,
    cppOptions          = combine    cppOptions,
    asmOptions          = combine    asmOptions,
    cmmOptions          = combine    cmmOptions,
    ccOptions           = combine    ccOptions,
    cxxOptions          = combine    cxxOptions,
    ldOptions           = combine    ldOptions,
    pkgconfigDepends    = combine    pkgconfigDepends,
    frameworks          = combineNub frameworks,
    extraFrameworkDirs  = combineNub extraFrameworkDirs,
    asmSources          = combineNub asmSources,
    cmmSources          = combineNub cmmSources,
    cSources            = combineNub cSources,
    cxxSources          = combineNub cxxSources,
    jsSources           = combineNub jsSources,
    hsSourceDirs        = combineNub hsSourceDirs,
    otherModules        = combineNub otherModules,
    virtualModules      = combineNub virtualModules,
    autogenModules      = combineNub autogenModules,
    defaultLanguage     = combineMby defaultLanguage,
    otherLanguages      = combineNub otherLanguages,
    defaultExtensions   = combineNub defaultExtensions,
    otherExtensions     = combineNub otherExtensions,
    oldExtensions       = combineNub oldExtensions,
    extraLibs           = combine    extraLibs,
    extraGHCiLibs       = combine    extraGHCiLibs,
    extraBundledLibs    = combine    extraBundledLibs,
    extraLibFlavours    = combine    extraLibFlavours,
    extraLibDirs        = combineNub extraLibDirs,
    includeDirs         = combineNub includeDirs,
    includes            = combineNub includes,
    installIncludes     = combineNub installIncludes,
    options             = combine    options,
    profOptions         = combine    profOptions,
    sharedOptions       = combine    sharedOptions,
    staticOptions       = combine    staticOptions,
    customFieldsBI      = combine    customFieldsBI,
    targetBuildDepends  = combineNub targetBuildDepends,
    mixins    = combine mixins
  }
    where
      combine    field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a

emptyBuildInfo :: BuildInfo
emptyBuildInfo = mempty

-- | The 'Language's used by this component
--
allLanguages :: BuildInfo -> [Language]
allLanguages bi = maybeToList (defaultLanguage bi)
               ++ otherLanguages bi

-- | The 'Extension's that are used somewhere by this component
--
allExtensions :: BuildInfo -> [Extension]
allExtensions bi = usedExtensions bi
                ++ otherExtensions bi

-- | The 'Extensions' that are used by all modules in this component
--
usedExtensions :: BuildInfo -> [Extension]
usedExtensions bi = oldExtensions bi
                 ++ defaultExtensions bi

-- | Whether any modules in this component use Template Haskell or
-- Quasi Quotes
usesTemplateHaskellOrQQ :: BuildInfo -> Bool
usesTemplateHaskellOrQQ bi = any p (allExtensions bi)
  where
    p ex = ex `elem`
      [EnableExtension TemplateHaskell, EnableExtension QuasiQuotes]

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> BuildInfo -> [String]
hcOptions = lookupHcOptions options

hcProfOptions :: CompilerFlavor -> BuildInfo -> [String]
hcProfOptions = lookupHcOptions profOptions

hcSharedOptions :: CompilerFlavor -> BuildInfo -> [String]
hcSharedOptions = lookupHcOptions sharedOptions

hcStaticOptions :: CompilerFlavor -> BuildInfo -> [String]
hcStaticOptions = lookupHcOptions staticOptions

lookupHcOptions :: (BuildInfo -> [(CompilerFlavor,[String])])
                -> CompilerFlavor -> BuildInfo -> [String]
lookupHcOptions f hc bi = [ opt | (hc',opts) <- f bi
                          , hc' == hc
                          , opt <- opts ]
