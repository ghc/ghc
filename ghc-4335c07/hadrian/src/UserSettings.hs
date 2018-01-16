-- If you want to customise your build you should copy this file from
-- hadrian/src/UserSettings.hs to hadrian/UserSettings.hs and edit your copy.
-- If you don't copy the file your changes will be tracked by git and you can
-- accidentally commit them.
module UserSettings (
    userBuildRoot, userFlavours, userPackages, verboseCommand,
    buildProgressColour, successColour, stage1Only
    ) where

import Hadrian.Utilities
import System.Console.ANSI

import Flavour
import Expression
import {-# SOURCE #-} Settings.Default

-- See doc/user-settings.md for instructions.
-- Please update doc/user-settings.md when committing changes to this file.

-- | All build results are put into the 'buildRoot' directory.
userBuildRoot :: BuildRoot
userBuildRoot = BuildRoot "_build"

-- | User-defined build flavours. See 'userFlavour' as an example.
userFlavours :: [Flavour]
userFlavours = [userFlavour] -- Add more build flavours if need be.

-- | This is an example user-defined build flavour. Feel free to modify it and
-- use by passing @--flavour=user@ from the command line.
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user" } -- Modify other settings here.

-- | Add user-defined packages. Note, this only lets Hadrian know about the
-- existence of a new package; to actually build it you need to create a new
-- build flavour, modifying the list of packages that are built by default.
userPackages :: [Package]
userPackages = []

-- | Set to 'True' to print full command lines during the build process. Note:
-- this is a 'Predicate', hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommand = package ghcPrim@.
verboseCommand :: Predicate
verboseCommand = do
    verbosity <- expr getVerbosity
    return $ verbosity >= Loud

-- | Set colour for build progress messages (e.g. executing a build command).
buildProgressColour :: BuildProgressColour
buildProgressColour = BuildProgressColour (Dull, Magenta)

-- | Set colour for success messages (e.g. a package is built successfully).
successColour :: SuccessColour
successColour = SuccessColour (Dull, Green)

-- TODO: Set this flag from the command line.
-- | Set this flag to 'True' to disable building Stage2 GHC (i.e. the @ghc-stage2@
-- executable) and Stage2 utilities (such as @haddock@). Note that all Stage0
-- and Stage1 libraries (including 'compiler') will still be built. Enabling
-- this flag during installation leads to installing @ghc-stage1@ instead of
-- @ghc-stage2@, and @ghc-pkg@ that was build with the Stage0 compiler.
-- Also see Note [No stage2 packages when CrossCompiling or Stage1Only] in the
-- top-level @ghc.mk@.
stage1Only :: Bool
stage1Only = False
