-- If you want to customise your build you should copy this file from
-- hadrian/src/UserSettings.hs to hadrian/UserSettings.hs and edit your copy.
-- If you don't copy the file your changes will be tracked by git and you can
-- accidentally commit them.
module UserSettings (
    userBuildRoot, userFlavours, userPackages, verboseCommands,
    buildProgressColour, successColour, stage1Only, crossCompiling
    ) where

import Hadrian.Utilities
import System.Console.ANSI

import Flavour
import Expression

-- See doc/user-settings.md for instructions.

-- | All build results are put into the 'buildRoot' directory.
userBuildRoot :: BuildRoot
userBuildRoot = BuildRoot "_build"

-- | User defined build flavours. See 'defaultFlavour' as an example.
userFlavours :: [Flavour]
userFlavours = []

-- | Add user defined packages. Note, this only lets Hadrian know about the
-- existence of a new package; to actually build it you need to create a new
-- build flavour, modifying the list of packages that are built by default.
userPackages :: [Package]
userPackages = []

-- | Set to 'True' to print full command lines during the build process. Note:
-- this is a 'Predicate', hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommands = package ghcPrim@.
verboseCommands :: Predicate
verboseCommands = do
    verbosity <- expr getVerbosity
    return $ verbosity >= Loud

-- | Set colour for build progress messages (e.g. executing a build command).
buildProgressColour :: BuildProgressColour
buildProgressColour = BuildProgressColour (Dull, Magenta)

-- | Set colour for success messages (e.g. a package is built successfully).
successColour :: SuccessColour
successColour = SuccessColour (Dull, Green)

-- | Build a cross compiling GHC
-- TODO: Use @Action Bool@ version in @Oracles.Flag@
crossCompiling :: Bool
crossCompiling = False

{-
  Stage1Only=YES means:
   - don't build ghc-stage2 (the executable)
   - don't build utils that rely on ghc-stage2
     See Note [No stage2 packages when CrossCompiling or Stage1Only] in
     ./ghc.mk.
   - install ghc-stage1 instead of ghc-stage2
   - install the ghc-pkg that was built with the stage0 compiler
   - (*do* still build compiler/stage2 (i.e. the ghc library))
   - (*do* still build all other libraries)
-}
-- | Stage1Only flag, default off
-- | TODO: Set this dynamically
stage1Only :: Bool
stage1Only = False
