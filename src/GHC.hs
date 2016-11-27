{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    array, base, binary, bytestring, cabal, checkApiAnnotations, compiler,
    containers, compareSizes, deepseq, deriveConstants, directory, dllSplit,
    filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghci,
    ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock, haskeline, hsc2hs, hoopl, hp2ps,
    hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi, mkUserGuidePart,
    parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell,
    terminfo, time, touchy, transformers, unlit, unix, win32, xhtml,

    defaultKnownPackages, builderProvenance, programName, nonCabalContext
    ) where

import Builder
import Context
import Package
import Stage

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- "Packages" defines default conditions for building each package, which can
-- be overridden in @hadrian/src/UserSettings.hs@.
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, checkApiAnnotations, compiler
    , containers, compareSizes, deepseq, deriveConstants, directory, dllSplit
    , filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghci
    , ghcPkg, ghcPrim, ghcTags, haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc
    , hpcBin, integerGmp, integerSimple, iservBin, libffi, mkUserGuidePart
    , parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell
    , terminfo, time, touchy, transformers, unlit, unix, win32, xhtml ]

-- | Package definitions, see 'Package'.
array               = library  "array"
base                = library  "base"
binary              = library  "binary"
bytestring          = library  "bytestring"
cabal               = library  "Cabal"        `setPath` "libraries/Cabal/Cabal"
checkApiAnnotations = utility  "check-api-annotations"
compiler            = topLevel "ghc"          `setPath` "compiler"
containers          = library  "containers"
compareSizes        = utility  "compareSizes" `setPath` "utils/compare_sizes"
deepseq             = library  "deepseq"
deriveConstants     = utility  "deriveConstants"
directory           = library  "directory"
dllSplit            = utility  "dll-split"
filepath            = library  "filepath"
genapply            = utility  "genapply"
genprimopcode       = utility  "genprimopcode"
ghc                 = topLevel "ghc-bin"      `setPath` "ghc"   `setType` Program
ghcBoot             = library  "ghc-boot"
ghcBootTh           = library  "ghc-boot-th"
ghcCabal            = utility  "ghc-cabal"
ghci                = library  "ghci"
ghcPkg              = utility  "ghc-pkg"
ghcPrim             = library  "ghc-prim"
ghcTags             = utility  "ghctags"
haddock             = utility  "haddock"
haskeline           = library  "haskeline"
hsc2hs              = utility  "hsc2hs"
hoopl               = library  "hoopl"
hp2ps               = utility  "hp2ps"
hpc                 = library  "hpc"
hpcBin              = utility  "hpc-bin"      `setPath` "utils/hpc"
integerGmp          = library  "integer-gmp"
integerSimple       = library  "integer-simple"
iservBin            = topLevel "iserv-bin"    `setPath` "iserv" `setType` Program
libffi              = topLevel "libffi"
mkUserGuidePart     = utility  "mkUserGuidePart"
parallel            = library  "parallel"
pretty              = library  "pretty"
primitive           = library  "primitive"
process             = library  "process"
rts                 = topLevel "rts"
runGhc              = utility  "runghc"
stm                 = library  "stm"
templateHaskell     = library  "template-haskell"
terminfo            = library  "terminfo"
time                = library  "time"
touchy              = utility  "touchy"
transformers        = library  "transformers"
unlit               = utility  "unlit"
unix                = library  "unix"
win32               = library  "Win32"
xhtml               = library  "xhtml"

-- | @ghc-split@ is a Perl script used by GHC with @-split-objs@ flag. It is
-- generated in "Rules.Generators.GhcSplit".
ghcSplit :: FilePath
ghcSplit = "inplace/lib/bin/ghc-split"

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
    GhcPkg Stage0    -> Nothing
    GhcPkg _         -> context Stage0 ghcPkg
    Haddock          -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

-- | Given a 'Context', compute the name of the program that is built in it
-- assuming that the corresponding package's type is 'Program'. For example, GHC
-- built in 'Stage0' is called @ghc-stage1@. If the given package is a
-- 'Library', the function simply returns its name.
programName :: Context -> String
programName Context {..}
    | package == ghc    = "ghc-stage" ++ show (fromEnum stage + 1)
    | package == hpcBin = "hpc"
    | package == runGhc = "runhaskell"
    | otherwise         = pkgNameString package

-- | Some contexts are special: their packages do have @.cabal@ metadata or
-- we cannot run @ghc-cabal@ on them, e.g. because the latter hasn't been built
-- yet (this is the case with the 'ghcCabal' package in 'Stage0').
nonCabalContext :: Context -> Bool
nonCabalContext Context {..} = (package `elem` [hp2ps, rts, touchy, unlit])
    || package == ghcCabal && stage == Stage0
