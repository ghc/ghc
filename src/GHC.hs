{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    -- * GHC packages
    array, base, binary, bytestring, cabal, checkApiAnnotations, checkPpr,
    compareSizes, compiler, containers, deepseq, deriveConstants, directory,
    filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal,
    ghcCompact, ghcHeap, ghci, ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock,
    haskeline, hsc2hs, hp2ps, hpc, hpcBin, integerGmp, integerSimple, iserv,
    libffi, libiserv, mtl, parsec, parallel, pretty, primitive, process, rts,
    runGhc, stm, templateHaskell, terminfo, text, time, touchy, transformers,
    unlit, unix, win32, xhtml, ghcPackages, isGhcPackage, defaultPackages,
    testsuitePackages,

    -- * Package information
    programName, nonCabalContext, nonHsMainPackage, autogenPath, installStage,

    -- * Miscellaneous
    programPath, buildDll0, rtsContext, rtsBuildPath, libffiContext,
    libffiBuildPath, libffiLibraryName
    ) where

import Base
import Context
import Flavour
import GHC.Packages
import Oracles.Flag
import Oracles.Setting
import Settings (flavour)

-- | Packages that are built by default. You can change this in "UserSettings".
defaultPackages :: Stage -> Action [Package]
defaultPackages Stage0 = stage0Packages
defaultPackages Stage1 = stage1Packages
defaultPackages Stage2 = stage2Packages
defaultPackages Stage3 = return []

stage0Packages :: Action [Package]
stage0Packages = do
    win <- windowsHost
    cross <- flag CrossCompiling
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
             , ghcHeap
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
             , unlit                         ]
          ++ [ terminfo | not win, not cross ]
          ++ [ touchy   | win                ]

stage1Packages :: Action [Package]
stage1Packages = do
    win        <- windowsHost
    intLib     <- integerLibrary =<< flavour
    libraries0 <- filter isLibrary <$> stage0Packages
    cross      <- flag CrossCompiling
    return $ libraries0 -- Build all Stage0 libraries in Stage1
          ++ [ array
             , base
             , bytestring
             , containers
             , deepseq
             , directory
             , filepath
             , ghc
             , ghcCompact
             , ghcPkg
             , ghcPrim
             , haskeline
             , hsc2hs
             , intLib
             , pretty
             , process
             , rts
             , stm
             , time
             , unlit
             , xhtml                         ]
          ++ [ haddock  | not cross          ]
          ++ [ runGhc   | not cross          ]
          ++ [ hpcBin   | not cross          ]
          ++ [ iserv    | not win, not cross ]
          ++ [ libiserv | not win, not cross ]
          ++ [ unix     | not win            ]
          ++ [ win32    | win                ]

stage2Packages :: Action [Package]
stage2Packages = return [haddock]

-- | Packages that are built only for the testsuite.
testsuitePackages :: Action [Package]
testsuitePackages = do
  win <- windowsHost
  return $
    [ checkApiAnnotations
    , checkPpr
    , ghci
    , ghcPkg
    , hp2ps
    , iserv
    , parallel
    , runGhc              ] ++
    [ timeout | win       ]

-- | Given a 'Context', compute the name of the program that is built in it
-- assuming that the corresponding package's type is 'Program'. For example, GHC
-- built in 'Stage0' is called @ghc-stage1@. If the given package is a
-- 'Library', the function simply returns its name.
programName :: Context -> Action String
programName Context {..} = do
    cross <- flag CrossCompiling
    targetPlatform <- setting TargetPlatformFull
    let prefix = if cross then targetPlatform ++ "-" else ""
      in return $ prefix ++ case package of
                              p | p == ghc    -> "ghc"
                                | p == hpcBin -> "hpc"
                                | p == runGhc -> "runhaskell"
                                | p == iserv  -> "ghc-iserv"
                              _               ->  pkgName package

-- | The build stage whose results are used when installing a package, or
-- @Nothing@ if the package is not installed, e.g. because it is a user package.
-- The current implementation installs the /latest/ build stage of a package.
installStage :: Package -> Action (Maybe Stage)
installStage pkg
    | not (isGhcPackage pkg) = return Nothing -- Only GHC packages are installed
    | otherwise = do
        stages <- filterM (fmap (pkg `elem`) . defaultPackages) [Stage0 ..]
        return $ if null stages then Nothing else Just (maximum stages)

-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Action FilePath
programPath context@Context {..} = do
    -- The @touchy@ utility lives in the @lib/bin@ directory instead of @bin@,
    -- which is likely just a historical accident that will hopefully be fixed.
    -- See: https://github.com/snowleopard/hadrian/issues/570
    -- Likewise for 'unlit'.
    path <- if package `elem` [touchy, unlit]
      then stageLibPath stage <&> (-/- "bin")
      else stageBinPath stage
    pgm  <- programName context
    return $ path -/- pgm <.> exe

-- | Some contexts are special: their packages do not have @.cabal@ metadata or
-- we cannot run @ghc-cabal@ on them, e.g. because the latter hasn't been built
-- yet (this is the case with the 'ghcCabal' package in 'Stage0').
nonCabalContext :: Context -> Bool
nonCabalContext Context {..} = (package `elem` [ hp2ps
                                               , touchy
                                               ])
    || package == ghcCabal && stage == Stage0

-- | Some program packages should not be linked with Haskell main function.
nonHsMainPackage :: Package -> Bool
nonHsMainPackage = (`elem` [ghc, hp2ps, iserv, touchy, unlit])

-- | Path to the autogen directory generated by @ghc-cabal@ of a given 'Context'.
autogenPath :: Context -> Action FilePath
autogenPath context@Context {..}
    | isLibrary package = autogen "build"
    | package == ghc    = autogen "build/ghc"
    | package == hpcBin = autogen "build/hpc"
    | otherwise         = autogen $ "build" -/- pkgName package
  where
    autogen dir = contextPath context <&> (-/- dir -/- "autogen")

buildDll0 :: Context -> Action Bool
buildDll0 Context {..} = do
    windows <- windowsHost
    return $ windows && stage == Stage1 && package == compiler

-- | RTS is considered a Stage1 package. This determines RTS build directory.
rtsContext :: Context
rtsContext = vanillaContext Stage1 rts

-- | Path to the RTS build directory.
rtsBuildPath :: Action FilePath
rtsBuildPath = buildPath rtsContext

-- | Libffi is considered a Stage1 package. This determines its build directory.
libffiContext :: Context
libffiContext = vanillaContext Stage1 libffi

-- | Build directory for in-tree Libffi library.
libffiBuildPath :: Action FilePath
libffiBuildPath = buildPath libffiContext

libffiLibraryName :: Action FilePath
libffiLibraryName = do
    useSystemFfi <- flag UseSystemFfi
    windows      <- windowsHost
    return $ case (useSystemFfi, windows) of
        (True , False) -> "ffi"
        (False, False) -> "Cffi"
        (_    , True ) -> "Cffi-6"
