module Settings.Packages (
    packages, knownPackages
    ) where

import Base
import Package
import Targets
import Switches
import Expression
import UserSettings

-- Combining default list of packages with user modifications
packages :: Packages
packages = defaultPackages <> userPackages

-- These are the packages we build by default
defaultPackages :: Packages
defaultPackages = mconcat
    [ stage0 ? packagesStage0
    , stage1 ? packagesStage1 ]

packagesStage0 :: Packages
packagesStage0 = mconcat
    [ append [binPackageDb, binary, cabal, compiler, hoopl, hpc, transformers]
    , notWindowsHost ? notTargetOs "ios" ? append [terminfo] ]

packagesStage1 :: Packages
packagesStage1 = mconcat
    [ append [ array, base, bytestring, containers, deepseq, directory
             , filepath, ghcPrim, haskeline, integerLibrary, parallel
             , pretty, primitive, process, stm, templateHaskell, time ]
    , windowsHost    ? append [win32]
    , notWindowsHost ? append [unix]
    , buildHaddock   ? append [xhtml] ]

-- These are all packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- Settings/Packages.hs defines default conditions for building each package,
-- which can be overridden in UserSettings.hs.
knownPackages :: [Package]
knownPackages = defaultKnownPackages ++ userKnownPackages

defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binPackageDb, binary, bytestring, cabal, compiler
    , containers, deepseq, directory, filepath, ghcPrim, haskeline
    , hoopl, hpc, integerLibrary, parallel, pretty, primitive, process
    , stm, templateHaskell, terminfo, time, transformers, unix, win32, xhtml ]
