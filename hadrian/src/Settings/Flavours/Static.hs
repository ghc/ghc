module Settings.Flavours.Static (staticFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

import Settings.Flavours.Performance (performanceArgs)

-- Please update doc/flavours.md when changing this file.

-- |Produce statically-linked executables.  Also compiles libraries
-- suitable for static linking.
staticFlavour :: Flavour
staticFlavour = defaultFlavour
    { name = "static"
    , args = defaultBuilderArgs <> performanceArgs <> defaultPackageArgs <> staticExec
    , dynamicGhcPrograms = return False }


staticExec :: Args
staticExec = not . wayUnit Dynamic <$> getWay ? mconcat
    {-
     - The final executables don't work unless the libraries linked into
     - it are compiled with "-fPIC."  The PI stands for "position
     - independent" and generates libraries that work when inlined into
     - an executable (where their position is not at the beginning of
     - the file).
     -}
    [ builder (Ghc CompileHs) ? pure [ "-fPIC", "-static" ]
    , builder (Ghc CompileCWithGhc) ? pure [ "-fPIC", "-optc", "-static"]
    , builder (Ghc LinkHs) ? pure [ "-optl", "-static" ]
    ]
