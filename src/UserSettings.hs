-- If you want to customise your build you should copy this file from
-- hadrian/src/UserSettings.hs to hadrian/UserSettings.hs and edit your copy.
-- If you don't copy the file your changes will be tracked by git and you can
-- accidentally commit them.
module UserSettings (
    buildRootPath, userFlavours, userKnownPackages, verboseCommands,
    putBuild, putSuccess, defaultDestDir, defaultStage1Only
    ) where

import Development.Shake
import Hadrian.Utilities
import System.Console.ANSI

import CmdLineFlag
import Flavour
import Expression

-- See doc/user-settings.md for instructions.

-- | All build results are put into 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = "_build"

-- | User defined build flavours. See 'defaultFlavour' as an example.
userFlavours :: [Flavour]
userFlavours = []

-- | Add user defined packages. Note, this only let's Hadrian know about the
-- existence of a new package; to actually build it you need to create a new
-- build flavour, modifying the list of packages that are built by default.
userKnownPackages :: [Package]
userKnownPackages = []

-- | Set to True to print full command lines during the build process. Note,
-- this is a Predicate, hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommands = package ghcPrim@.
verboseCommands :: Predicate
verboseCommands = return False

-- | Customise build progress messages (e.g. executing a build command).
putBuild :: String -> Action ()
putBuild = putColoured cmdProgressColour Dull Magenta

-- | Customise build success messages (e.g. a package is built successfully).
putSuccess :: String -> Action ()
putSuccess = putColoured cmdProgressColour Dull Green

-- | Path to the GHC install destination. It is empty by default, which
-- corresponds to the root of the file system. You can replace it by a specific
-- directory. Make sure you use correct absolute path on Windows, e.g. "C:/path".
-- The destination directory is used with a @prefix@, commonly @/usr/local@,
-- i.e. GHC is installed into "C:/path/usr/local" for the above example.
defaultDestDir :: FilePath
defaultDestDir = ""

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
defaultStage1Only :: Bool
defaultStage1Only = False
