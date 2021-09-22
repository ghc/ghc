-- Ensure we don't expose any unfoldings to guarantee quick rebuilds
{-# OPTIONS_GHC -O0 #-}

-- If you want to customise your build you should copy this file from
-- hadrian/src/UserSettings.hs to hadrian/UserSettings.hs and edit your copy.
-- If you don't copy the file your changes will be tracked by git and you can
-- accidentally commit them.
--
-- See doc/user-settings.md for instructions, and src/Flavour.hs for auxiliary
-- functions for manipulating flavours.
-- Please update doc/user-settings.md when committing changes to this file.
module UserSettings (
    userFlavours, userPackages, userDefaultFlavour,
    verboseCommand, buildProgressColour, successColour, finalStage
    ) where

import Flavour.Type
import Expression
import {-# SOURCE #-} Settings.Default

-- See doc/user-settings.md for instructions.
-- Please update doc/user-settings.md when committing changes to this file.

-- | Name of the default flavour, i.e the one used when no --flavour=<name>
--   argument is passed to Hadrian.
userDefaultFlavour :: String
userDefaultFlavour = "default"

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
    return $ verbosity >= Verbose

-- | Set colour for build progress messages (e.g. executing a build command).
buildProgressColour :: BuildProgressColour
buildProgressColour = mkBuildProgressColour (Dull Magenta)

-- | Set colour for success messages (e.g. a package is built successfully).
successColour :: SuccessColour
successColour = mkSuccessColour (Dull Green)

-- | Stop after building the StageN compiler.
-- For example, setting the 'finalStage' to 'Stage1' will just build the
-- 'Stage1' compiler. Setting it to 'Stage3' will build the 'Stage3'
-- compiler. Setting it to 'Stage0' will mean nothing gets built at all.
finalStage :: Stage
finalStage = Stage2
