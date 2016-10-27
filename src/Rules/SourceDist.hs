module Rules.SourceDist (sourceDistRules) where

import Base
import Builder
import Oracles.Config.Setting
import Oracles.DirectoryContent
import Rules.Actions
import UserSettings

sourceDistRules :: Rules ()
sourceDistRules = do
    "sdist-ghc" ~> do
        version <- setting ProjectVersion
        need ["sdistprep/ghc-" ++ version ++ "-src.tar.xz"]
        putSuccess "| Done. "
    "sdistprep/ghc-*-src.tar.xz" %> \fname -> do
        let tarName = takeFileName fname
            treePath = "sdistprep/ghc" </> dropTarXz tarName
        prepareTree treePath
        runBuilderWith [Cwd "sdistprep/ghc"] Tar ["cJf", ".." </> tarName, dropTarXz tarName]
    "GIT_COMMIT_ID" %> \fname ->
        setting ProjectGitCommitId >>= writeFileChanged fname
    "VERSION" %> \fname ->
        setting ProjectVersion >>= writeFileChanged fname
  where
    dropTarXz = dropExtension . dropExtension


prepareTree :: FilePath -> Action ()
prepareTree dest = do
    mapM_ cpDir  srcDirs
    mapM_ cpFile srcFiles
  where
    cpFile a = copyFile a (dest </> a)
    cpDir  a = copyDirectoryContent (Not excluded) a (dest </> takeFileName a)
    excluded = Or
      [ Test "//.*"
      , Test "//#*"
      , Test "//*-SAVE"
      , Test "//*.orig"
      , Test "//*.rej"
      , Test "//*~"
      , Test "//autom4te*"
      , Test "//dist"
      , Test "//log"
      , Test "//stage0"
      , Test "//stage1"
      , Test "//stage2"
      , Test "//stage3"
      , Test "hadrian/cabal.sandbox.config"
      , Test "hadrian/cfg/system.config"
      , Test "hadrian/dist"
      , Test "hadrian/UserSettings.hs"
      , Test "libraries//*.buildinfo"
      , Test "libraries//GNUmakefile"
      , Test "libraries//config.log"
      , Test "libraries//config.status"
      , Test "libraries//configure"
      , Test "libraries//ghc.mk"
      , Test "libraries//include/Hs*Config.h"
      , Test "libraries/dph"
      , Test "libraries/parallel"
      , Test "libraries/primitive"
      , Test "libraries/random"
      , Test "libraries/stm"
      , Test "libraries/vector"
      , Test "mk/build.mk" ]
    srcDirs =
      [ "bindisttest"
      , "compiler"
      , "distrib"
      , "docs"
      , "docs"
      , "driver"
      , "ghc"
      , "hadrian"
      , "includes"
      , "iserv"
      , "libffi"
      , "libffi-tarballs"
      , "libraries"
      , "mk"
      , "rts"
      , "rules"
      , "utils" ]
    srcFiles =
      [ "ANNOUNCE"
      , "GIT_COMMIT_ID"
      , "HACKING.md"
      , "INSTALL.md"
      , "LICENSE"
      , "MAKEHELP.md"
      , "Makefile"
      , "README.md"
      , "VERSION"
      , "aclocal.m4"
      , "boot"
      , "config.guess"
      , "config.sub"
      , "configure"
      , "configure.ac"
      , "ghc.mk"
      , "install-sh"
      , "packages"
      , "settings.in" ]
