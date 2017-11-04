{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    -- * GHC packages
    array, base, binary, bytestring, cabal, compareSizes, compiler, containers,
    deepseq, deriveConstants, directory, filepath, genapply, genprimopcode, ghc,
    ghcBoot, ghcBootTh, ghcCabal, ghcCompact, ghci, ghcPkg, ghcPrim, ghcTags,
    ghcSplit, haddock, haskeline, hsc2hs, hp2ps, hpc, hpcBin, integerGmp,
    integerSimple, iservBin, libffi, mtl, parsec, parallel, pretty, primitive,
    process, rts, runGhc, stm, templateHaskell, terminfo, text, time, touchy,
    transformers, unlit, unix, win32, xhtml, ghcPackages, isGhcPackage,
    defaultPackages,

    -- * Package information
    programName, nonCabalContext, nonHsMainPackage, autogenPath, installStage,

    -- * Miscellaneous
    programPath, ghcSplitPath, stripCmdPath, buildDll0
    ) where

import Base
import CommandLine
import Context
import Oracles.Flag
import Oracles.Setting

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows. 'defaultPackages' defines default
-- conditions for building each package. Users can add their own packages and
-- modify build default build conditions in "UserSettings".
ghcPackages :: [Package]
ghcPackages =
    [ array, base, binary, bytestring, cabal, compareSizes, compiler, containers
    , deepseq, deriveConstants, directory, filepath, genapply, genprimopcode
    , ghc, ghcBoot, ghcBootTh, ghcCabal, ghcCompact, ghci, ghcPkg, ghcPrim
    , ghcTags, haddock, haskeline, hsc2hs, hp2ps, hpc, hpcBin, integerGmp
    , integerSimple, iservBin, libffi, mtl, parsec, parallel, pretty, primitive
    , process, rts, runGhc, stm, templateHaskell, terminfo, text, time, touchy
    , transformers, unlit, unix, win32, xhtml ]

-- TODO: Optimise by switching to sets of packages.
isGhcPackage :: Package -> Bool
isGhcPackage = (`elem` ghcPackages)

-- | Package definitions, see 'Package'.
array               = hsLib  "array"
base                = hsLib  "base"
binary              = hsLib  "binary"
bytestring          = hsLib  "bytestring"
cabal               = hsLib  "Cabal"           `setPath` "libraries/Cabal/Cabal"
compareSizes        = hsUtil "compareSizes"    `setPath` "utils/compare_sizes"
compiler            = hsTop  "ghc"             `setPath` "compiler"
containers          = hsLib  "containers"
deepseq             = hsLib  "deepseq"
deriveConstants     = hsUtil "deriveConstants"
directory           = hsLib  "directory"
filepath            = hsLib  "filepath"
genapply            = hsUtil "genapply"
genprimopcode       = hsUtil "genprimopcode"
ghc                 = hsPrg  "ghc-bin"         `setPath` "ghc"
ghcBoot             = hsLib  "ghc-boot"
ghcBootTh           = hsLib  "ghc-boot-th"
ghcCabal            = hsUtil "ghc-cabal"
ghcCompact          = hsLib  "ghc-compact"
ghci                = hsLib  "ghci"
ghcPkg              = hsUtil "ghc-pkg"
ghcPrim             = hsLib  "ghc-prim"
ghcTags             = hsUtil "ghctags"
ghcSplit            = hsUtil "ghc-split"
haddock             = hsUtil "haddock"
haskeline           = hsLib  "haskeline"
hsc2hs              = hsUtil "hsc2hs"
hp2ps               = cUtil  "hp2ps"
hpc                 = hsLib  "hpc"
hpcBin              = hsUtil "hpc-bin"         `setPath` "utils/hpc"
integerGmp          = hsLib  "integer-gmp"
integerSimple       = hsLib  "integer-simple"
iservBin            = hsPrg  "iserv-bin"       `setPath` "iserv"
libffi              = cTop   "libffi"
mtl                 = hsLib  "mtl"
parsec              = hsLib  "parsec"
parallel            = hsLib  "parallel"
pretty              = hsLib  "pretty"
primitive           = hsLib  "primitive"
process             = hsLib  "process"
rts                 = cTop   "rts"
runGhc              = hsUtil "runghc"
stm                 = hsLib  "stm"
templateHaskell     = hsLib  "template-haskell"
terminfo            = hsLib  "terminfo"
text                = hsLib  "text"
time                = hsLib  "time"
touchy              = cUtil  "touchy"
transformers        = hsLib  "transformers"
unlit               = cUtil  "unlit"
unix                = hsLib  "unix"
win32               = hsLib  "Win32"
xhtml               = hsLib  "xhtml"

-- | Construct a Haskell library package, e.g. @array@.
hsLib :: PackageName -> Package
hsLib name = hsLibrary name ("libraries" -/- name)

-- | Construct a top-level Haskell library package, e.g. @compiler@.
hsTop :: PackageName -> Package
hsTop name = hsLibrary name name

-- | Construct a top-level C library package, e.g. @rts@.
cTop :: PackageName -> Package
cTop name = cLibrary name name

-- | Construct a top-level Haskell program package, e.g. @ghc@.
hsPrg :: PackageName -> Package
hsPrg name = hsProgram name name

-- | Construct a Haskell utility package, e.g. @haddock@.
hsUtil :: PackageName -> Package
hsUtil name = hsProgram name ("utils" -/- name)

-- | Construct a C utility package, e.g. @haddock@.
cUtil :: PackageName -> Package
cUtil name = cProgram name ("utils" -/- name)

-- | Amend a package path if it doesn't conform to a typical pattern.
setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

-- | Packages that are built by default. You can change this in "UserSettings".
defaultPackages :: Stage -> Action [Package]
defaultPackages Stage0 = stage0Packages
defaultPackages Stage1 = stage1Packages
defaultPackages Stage2 = stage2Packages
defaultPackages Stage3 = return []

stage0Packages :: Action [Package]
stage0Packages = do
    win <- windowsHost
    ios <- iosHost
    cross <- crossCompiling
    return $ [ binary
             , cabal
             , compareSizes
             , compiler
             , deriveConstants
             , genapply
             , genprimopcode
             , ghc
             , ghcBoot
             , ghcBootTh
             , ghcCabal
             , ghci
             , ghcPkg
             , ghcTags
             , hsc2hs
             , hp2ps
             , hpc
             , mtl
             , parsec
             , templateHaskell
             , text
             , transformers
             , unlit                       ]
          ++ [ terminfo | not win, not ios, not cross ]
          ++ [ touchy   | win              ]

stage1Packages :: Action [Package]
stage1Packages = do
    win        <- windowsHost
    intSimple  <- cmdIntegerSimple
    libraries0 <- filter isLibrary <$> stage0Packages
    return $ libraries0 -- Build all Stage0 libraries in Stage1
          ++ [ array
             , base
             , bytestring
             , containers
             , deepseq
             , directory
             , filepath
             , ghc
             , ghcCabal
             , ghcCompact
             , ghcPrim
             , haskeline
             , hpcBin
             , hsc2hs
             , if intSimple then integerSimple else integerGmp
             , pretty
             , process
             , rts
             , runGhc
             , stm
             , time
             , xhtml              ]
          ++ [ iservBin | not win ]
          ++ [ unix     | not win ]
          ++ [ win32    | win     ]

stage2Packages :: Action [Package]
stage2Packages = return [haddock]

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

-- | The build stage whose results are used when installing a package, or
-- @Nothing@ if the package is not installed, e.g. because it is a user package.
-- The current implementation installs the /latest/ build stage of a package.
installStage :: Package -> Action (Maybe Stage)
installStage pkg
    | not (isGhcPackage pkg) = return Nothing -- Only GHC packages are installed
    | otherwise = do
        stages <- filterM (fmap (pkg `elem`) . defaultPackages) [Stage0 ..]
        return $ if null stages then Nothing else Just (maximum stages)

-- | Is the program corresponding to a given context built 'inplace', i.e. in
-- the @inplace/bin@ directory? For most programs, only their /latest/ build
-- stages are built 'inplace'. The only exception is the GHC itself, which is
-- built 'inplace' in all stages. The function returns @False@ for libraries and
-- all user packages.
isBuiltInplace :: Context -> Action Bool
isBuiltInplace Context {..}
    | isLibrary package          = return False
    | not (isGhcPackage package) = return False
    | package == ghc             = return True
    | otherwise                  = (Just stage ==) <$> installStage package

-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Action FilePath
programPath context@Context {..} = do
    path    <- buildPath context
    inplace <- isBuiltInplace context
    let contextPath = if inplace then inplacePath else path
    return $ contextPath -/- programName context <.> exe
  where
    inplacePath | package `elem` [touchy, unlit, iservBin] = inplaceLibBinPath
                | otherwise                                = inplaceBinPath

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
