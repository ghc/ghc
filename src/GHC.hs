{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    -- * GHC packages
    array, base, binary, bytestring, cabal, checkApiAnnotations, compareSizes,
    compiler, containers, deepseq, deriveConstants, directory, dllSplit, filepath,
    genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghcCompact, ghci,
    ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock, haskeline, hsc2hs, hp2ps,
    hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi, mtl, parsec,
    parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell,
    terminfo, text, time, touchy, transformers, unlit, unix, win32, xhtml,
    defaultKnownPackages,

    -- * Package information
    builderProvenance, programName, nonCabalContext, nonHsMainPackage, autogenPath,

    -- * Miscellaneous
    systemBuilderPath, ghcSplitPath, stripCmdPath, inplaceInstallPath, buildDll0
    ) where

import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile

import Base
import Context
import Oracles.Setting

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows. "Settings.Default" defines default
-- conditions for building each package, which can be overridden in
-- @hadrian/src/UserSettings.hs@.
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, checkApiAnnotations, compareSizes
    , compiler, containers, deepseq, deriveConstants, directory, dllSplit
    , filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal
    , ghcCompact, ghci, ghcPkg, ghcPrim, ghcTags, haddock, haskeline, hsc2hs
    , hp2ps, hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi
    , mtl, parsec, parallel, pretty, primitive, process, rts, runGhc, stm
    , templateHaskell, terminfo, text, time, touchy, transformers, unlit, unix
    , win32, xhtml ]

-- | Package definitions, see 'Package'.
array               = lib  "array"
base                = lib  "base"
binary              = lib  "binary"
bytestring          = lib  "bytestring"
cabal               = lib  "Cabal"           `setPath` "libraries/Cabal/Cabal"
checkApiAnnotations = util "check-api-annotations"
compareSizes        = util "compareSizes"    `setPath` "utils/compare_sizes"
compiler            = top  "ghc"             `setPath` "compiler"
containers          = lib  "containers"
deepseq             = lib  "deepseq"
deriveConstants     = util "deriveConstants"
directory           = lib  "directory"
dllSplit            = util "dll-split"
filepath            = lib  "filepath"
genapply            = util "genapply"
genprimopcode       = util "genprimopcode"
ghc                 = prg  "ghc-bin"         `setPath` "ghc"
ghcBoot             = lib  "ghc-boot"
ghcBootTh           = lib  "ghc-boot-th"
ghcCabal            = util "ghc-cabal"
ghcCompact          = lib  "ghc-compact"
ghci                = lib  "ghci"
ghcPkg              = util "ghc-pkg"
ghcPrim             = lib  "ghc-prim"
ghcTags             = util "ghctags"
ghcSplit            = util "ghc-split"
haddock             = util "haddock"
haskeline           = lib  "haskeline"
hsc2hs              = util "hsc2hs"
hp2ps               = util "hp2ps"
hpc                 = lib  "hpc"
hpcBin              = util "hpc-bin"         `setPath` "utils/hpc"
integerGmp          = lib  "integer-gmp"
integerSimple       = lib  "integer-simple"
iservBin            = prg  "iserv-bin"       `setPath` "iserv"
libffi              = top  "libffi"
mtl                 = lib  "mtl"
parsec              = lib  "parsec"
parallel            = lib  "parallel"
pretty              = lib  "pretty"
primitive           = lib  "primitive"
process             = lib  "process"
rts                 = top  "rts"
runGhc              = util "runghc"
stm                 = lib  "stm"
templateHaskell     = lib  "template-haskell"
terminfo            = lib  "terminfo"
text                = lib  "text"
time                = lib  "time"
touchy              = util "touchy"
transformers        = lib  "transformers"
unlit               = util "unlit"
unix                = lib  "unix"
win32               = lib  "Win32"
xhtml               = lib  "xhtml"

-- | Construct a library package, e.g. @array@.
lib :: PackageName -> Package
lib name = library name ("libraries" -/- name)

-- | Construct a top-level library package, e.g. @compiler@.
top :: PackageName -> Package
top name = library name name

-- | Construct a top-level program package, e.g. @ghc@.
prg :: PackageName -> Package
prg name = program name name

-- | Construct a utility package, e.g. @haddock@.
util :: PackageName -> Package
util name = program name ("utils" -/- name)

-- | Amend a package path if it doesn't conform to a typical pattern.
setPath :: Package -> FilePath -> Package
setPath pkg path | isLibrary pkg = library (pkgName pkg) path
                 | otherwise     = program (pkgName pkg) path

-- | Some builders are built by this very build system, in which case
-- 'builderProvenance' returns the corresponding build 'Context' (which includes
-- 'Stage' and GHC 'Package').
builderProvenance :: Builder -> Maybe Context
builderProvenance = \case
    DeriveConstants  -> context Stage0 deriveConstants
    GenApply         -> context Stage0 genapply
    GenPrimopCode    -> context Stage0 genprimopcode
    Ghc _ Stage0     -> Nothing
    Ghc _ stage      -> context (pred stage) ghc
    GhcCabal         -> context Stage0 ghcCabal
    GhcCabalHsColour -> builderProvenance $ GhcCabal
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ _       -> context Stage0 ghcPkg
    Haddock          -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar Stage0       -> fromKey "system-ar"
    Ar _            -> fromKey "ar"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "sh configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsColour        -> fromKey "hscolour"
    HsCpp           -> fromKey "hs-cpp"
    Ld              -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Ranlib          -> fromKey "ranlib"
    Tar             -> fromKey "tar"
    _               -> error $ "No entry for " ++ show builder ++ inCfg
  where
    inCfg = " in " ++ quote configFile ++ " file."
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ inCfg ++ " Did you skip configure?"
        path <- unpack <$> lookupValue configFile key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified" ++ inCfg
            return "" -- TODO: Use a safe interface.
        else fixAbsolutePathOnWindows =<< lookupInPath path

-- | Given a 'Context', compute the name of the program that is built in it
-- assuming that the corresponding package's type is 'Program'. For example, GHC
-- built in 'Stage0' is called @ghc-stage1@. If the given package is a
-- 'Library', the function simply returns its name.
programName :: Context -> String
programName Context {..}
    | package == ghc      = "ghc-stage" ++ show (fromEnum stage + 1)
    | package == hpcBin   = "hpc"
    | package == runGhc   = "runhaskell"
    | package == iservBin = "ghc-iserv"
    | otherwise           = pkgName package

-- | Some contexts are special: their packages do not have @.cabal@ metadata or
-- we cannot run @ghc-cabal@ on them, e.g. because the latter hasn't been built
-- yet (this is the case with the 'ghcCabal' package in 'Stage0').
nonCabalContext :: Context -> Bool
nonCabalContext Context {..} = (package `elem` [hp2ps, rts, touchy, unlit])
    || package == ghcCabal && stage == Stage0

-- | Some program packages should not be linked with Haskell main function.
nonHsMainPackage :: Package -> Bool
nonHsMainPackage = (`elem` [ghc, hp2ps, iservBin, touchy, unlit])

-- | Path to the autogen directory generated by @ghc-cabal@ of a given 'Context'.
autogenPath :: Context -> Action FilePath
autogenPath context@Context {..}
    | isLibrary package   = autogen "build"
    | package == ghc      = autogen "build/ghc"
    | package == hpcBin   = autogen "build/hpc"
    | package == iservBin = autogen "build/iserv"
    | otherwise           = autogen $ "build" -/- pkgName package
  where
    autogen dir = buildPath context <&> (-/- dir -/- "autogen")

-- | Given a 'Package', return the path where the corresponding program is
-- installed. Most programs are installed in 'programInplacePath'.
inplaceInstallPath :: Package -> FilePath
inplaceInstallPath pkg
    | pkg == touchy   = inplaceLibBinPath
    | pkg == unlit    = inplaceLibBinPath
    | pkg == iservBin = inplaceLibBinPath
    | otherwise       = inplaceBinPath

-- | @ghc-split@ is a Perl script used by GHC with @-split-objs@ flag. It is
-- generated in "Rules.Generators.GhcSplit".
ghcSplitPath :: FilePath
ghcSplitPath = inplaceLibBinPath -/- "ghc-split"

-- ref: mk/config.mk
-- | Command line tool for stripping.
stripCmdPath :: Action FilePath
stripCmdPath = do
    targetPlatform <- setting TargetPlatform
    top <- topDirectory
    case targetPlatform of
        "x86_64-unknown-mingw32" ->
             return (top -/- "inplace/mingw/bin/strip.exe")
        "arm-unknown-linux" ->
             return ":" -- HACK: from the make-based system, see the ref above
        _ -> return "strip"

buildDll0 :: Context -> Action Bool
buildDll0 Context {..} = do
    windows <- windowsHost
    return $ windows && stage == Stage1 && package == compiler
