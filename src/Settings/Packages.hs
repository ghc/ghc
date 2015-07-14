module Settings.Packages (
    module Settings.Default,
    packages, knownPackages
    ) where

import Base
import Package
import Switches
import Expression
import Settings.Default
import Settings.User

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
    [ packagesStage0
    , append [ array, base, bytestring, containers, deepseq, directory
             , filepath, ghcPrim, haskeline, integerLibrary, parallel
             , pretty, primitive, process, stm, templateHaskell, time ]
    , windowsHost    ? append [win32]
    , notWindowsHost ? append [unix]
    , buildHaddock   ? append [xhtml] ]

knownPackages :: [Package]
knownPackages = defaultKnownPackages ++ userKnownPackages
