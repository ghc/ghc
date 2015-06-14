module Settings.Packages (
    packages
    ) where

import Base
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
    [ stage Stage0 ? packagesStage0
    , stage Stage1 ? packagesStage1 ]

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
