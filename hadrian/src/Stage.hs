{-# LANGUAGE LambdaCase #-}
module Stage (Stage (..), WhichLibs(..), isStage0, stage0InTree, stage0Boot, allStages,predStage, succStage, stageString) where

import Development.Shake.Classes
import GHC.Generics

-- | A stage refers to a certain compiler in GHC's build process.
--
-- * Stage0 GlobalLibs is for **executables** which are built with the boot compiler
--   and boot compiler packages. For example, this was motivated by needing to
--   build hsc2hs, a build dependency of unix with just the boot toolchain. (See #21634)
--
-- * Stage 0 (InTreeLibs) is built with the bootstrapping compiler, i.e. the one already
--   installed on the user's system. The compiler that is produced during
--   stage 0 is called /stage 1 compiler/. Stage0 executables and libraries are
--   build against the other libraries (in-tree) built by the stage 0 compiler.
--
-- * Stage 1 is built using the stage 1 compiler and all GHC sources. The result
--   is called /stage 2 compiler/ and it has all features of the new GHC.
--
-- * Stage 2 is built using the stage 2 compiler. The result is a compiler
--   fully "built by itself", commonly referred to as /bootstrapping/.
--
-- * Stage 3 is built as a self test. The resulting compiler should have
--   the same object code as the one built in stage 2, which is a good test
--   for the compiler. Since it serves no other purpose than that, the stage 3
--   build is usually omitted in the build process.
data Stage = Stage0 WhichLibs | Stage1 | Stage2 | Stage3
    deriving (Show, Eq, Ord, Generic)


-- | See Note [Stage 0 build plans]
data WhichLibs = GlobalLibs -- ^ Build build tools against the globally installed libraries
               | InTreeLibs -- ^ Build the compiler against the in-tree libraries.
               deriving (Show, Eq, Ord, Generic)

allStages :: [Stage]
allStages = [Stage0 GlobalLibs, Stage0 InTreeLibs, Stage1, Stage2, Stage3]

stage0InTree, stage0Boot :: Stage
stage0InTree = Stage0 InTreeLibs
stage0Boot = Stage0 GlobalLibs

isStage0 :: Stage -> Bool
isStage0 Stage0 {} = True
isStage0 _ = False


predStage :: Stage -> Stage
predStage Stage1 = stage0InTree
predStage Stage2 = Stage1
predStage Stage3 = Stage2
predStage s = error ("predStage: " ++ show s)

succStage :: Stage -> Stage
succStage (Stage0 {}) = Stage1
succStage Stage1    = Stage2
succStage Stage2    = Stage3
succStage Stage3    = error "succStage: Stage3"

instance Binary   Stage
instance Hashable Stage
instance NFData   Stage

instance Binary   WhichLibs
instance Hashable WhichLibs
instance NFData   WhichLibs

-- | Prettyprint a 'Stage'.
stageString :: Stage -> String
stageString = \case
  Stage0 GlobalLibs -> "stageBoot"
  Stage0 InTreeLibs -> "stage0"
  Stage1 -> "stage1"
  Stage2 -> "stage2"
  Stage3 -> "stage3"

{-
Note [Stage 0 build plans]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The Stage refers to which compiler we will use to perform the builds.

  Stage0: Build with the boot toolchain
  Stage1: Build with compiler built in stage 0
  Stage2: Build with compiler built in stage 1

Stage 0 also has two different package databases.

  Stage0 GlobalLibs: Used for building build tool dependencies (hsc2hs, unlit, linters etc)
                     Mostly using the libraries from the boot compiler.
  Stage0 InTreeLibs: Used for building the Stage 1 compiler (ghc executable) and all libraries
                     needed by that.

The reason for this split is

1. bytestring depends on template-haskell so we need to build bytestring with stage0 (and all
   packages which depend on it). This includes unix and hence directory (which depends on unix) but
   unix depends on hsc2hs (which depends on directory) and you get a loop in the build
   rules if you try to build them all in the same package database.
   The solution is to build hsc2hs with the global boot libraries in Stage0 GlobalLibs
2. We want to build linters and other build tools which we don't distribute in a separate
   package database so they don't end up in the bindist package database.

-}
