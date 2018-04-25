module Rules.SourceDist (sourceDistRules) where

import Hadrian.Oracles.DirectoryContents

import Base
import Builder
import Oracles.Setting
import Rules.Clean

sourceDistRules :: Rules ()
sourceDistRules = do
    "sdist-ghc" ~> do
        -- We clean the source tree first.
        -- See https://github.com/snowleopard/hadrian/issues/384.
        cleanSourceTree
        version <- setting ProjectVersion
        need ["sdistprep/ghc-" ++ version ++ "-src.tar.xz"]
        putSuccess "| Done"
    "sdistprep/ghc-*-src.tar.xz" %> \fname -> do
        let tarName   = takeFileName fname
            dropTarXz = dropExtension . dropExtension
            treePath  = "sdistprep/ghc" -/- dropTarXz tarName
        prepareTree treePath
        runBuilderWithCmdOptions [Cwd "sdistprep/ghc"] (Tar Create)
            ["cJf", ".." -/- tarName,  dropTarXz tarName]
            ["cJf", ".." -/- tarName] [dropTarXz tarName]
    "GIT_COMMIT_ID" %> \fname ->
        writeFileChanged fname =<< setting ProjectGitCommitId
    "VERSION" %> \fname ->
        writeFileChanged fname =<< setting ProjectVersion

prepareTree :: FilePath -> Action ()
prepareTree dest = do
    mapM_ cpDir  srcDirs
    mapM_ cpFile srcFiles
  where
    cpFile a = copyFile a (dest -/- a)
    cpDir  a = copyDirectoryContents (Not excluded) a (dest -/- a)
    excluded = Or
        [ Test "//.*"
        , Test "//#*"
        , Test "//*-SAVE"
        , Test "//*.orig"
        , Test "//*.rej"
        , Test "//*~"
        , Test "//autom4te*"
        , Test "//dist"
        , Test "//dist-install"
        , Test "//log"
        , Test "//stage0"
        , Test "//stage1"
        , Test "//stage2"
        , Test "//stage3"
        , Test "hadrian/.cabal-sandbox"
        , Test "hadrian/.stack-work"
        , Test "hadrian/UserSettings.hs"
        , Test "hadrian/cabal.sandbox.config"
        , Test "hadrian/cfg/system.config"
        , Test "hadrian/bin"
        , Test "hadrian/dist"
        , Test "hadrian/dist-newstyle"
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
