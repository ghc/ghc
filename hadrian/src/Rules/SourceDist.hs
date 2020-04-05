module Rules.SourceDist (sourceDistRules) where

import Hadrian.Oracles.DirectoryContents

import Base
import Builder
import Context
import Oracles.Setting
import Packages
import Rules.Clean

sourceDistRules :: Rules ()
sourceDistRules = do
    root <- buildRootRules
    "source-dist" ~> do
        -- We clean the source tree first.
        -- See https://github.com/snowleopard/hadrian/issues/384.
        -- TODO: Do we still need to clean the tree?
        cleanSourceTree
        version <- setting ProjectVersion
        need [root -/- "source-dist" -/- ("ghc-" ++ version ++ "-src.tar.xz")]
        putSuccess "| Done"
    root -/- "source-dist" -/- "ghc-*-src.tar.xz" %> \fname -> do
        let tarName   = takeFileName fname
            dropTarXz = dropExtension . dropExtension
            treeDir   = dropTarXz tarName
            treePath  = root -/- "source-dist" -/- treeDir
        prepareTree treePath
        runBuilderWithCmdOptions
            [Cwd $ root -/- "source-dist"]
            (Tar Create)
            ["cJf", tarName,  treeDir]
            ["cJf", tarName] [treeDir]
    "GIT_COMMIT_ID" %> \fname ->
        writeFileChanged fname =<< setting ProjectGitCommitId
    "VERSION" %> \fname ->
        writeFileChanged fname =<< setting ProjectVersion

prepareTree :: FilePath -> Action ()
prepareTree dest = do
    root <- buildRoot
    mapM_ cpDir  srcDirs
    mapM_ cpFile srcFiles
    copyAlexHappyFiles root
  where

    copyAlexHappyFiles root =
      forM_ alexHappyFiles $ \(stg, pkg, inp, out) -> do
        let dir = root -/- buildDir (Context stg pkg vanilla)
            srcInputFile = dest -/- pkgPath pkg -/- inp
        -- We first make sure that the generated file is... generated.
        need [ dir -/- out ]
        -- We then copy the generated file in the source dist, right
        -- next to the input file.
        copyFile (dir -/- out)
                 (dest -/- pkgPath pkg -/- out)
        -- We finally add a ".source" suffix to the input file to
        -- prevent it from being used when building GHC, since the
        -- generated file being there already should prevent
        -- the need for the original input.
        moveFile srcInputFile (srcInputFile <.> "source")

    cpFile a = copyFile a (dest -/- a)
    cpDir  a = copyDirectoryContents (Not excluded) a (dest -/- a)
    excluded = Or
        [ Test "**/.*"
        , Test "**/#*"
        , Test "**/*-SAVE"
        , Test "**/*.orig"
        , Test "**/*.rej"
        , Test "**/*~"
        , Test "**/autom4te*"
        , Test "**/dist"
        , Test "**/dist-install"
        , Test "**/log"
        , Test "**/stage0"
        , Test "**/stage1"
        , Test "**/stage2"
        , Test "**/stage3"
        , Test "hadrian/.cabal-sandbox"
        , Test "hadrian/.stack-work"
        , Test "hadrian/UserSettings.hs"
        , Test "hadrian/cabal.sandbox.config"
        , Test "hadrian/cfg/system.config"
        , Test "hadrian/bin"
        , Test "hadrian/dist"
        , Test "hadrian/dist-newstyle"
        , Test "libraries/**/*.buildinfo"
        , Test "libraries/**/GNUmakefile"
        , Test "libraries/**/config.log"
        , Test "libraries/**/config.status"
        , Test "libraries/**/ghc.mk"
        , Test "libraries/**/include/Hs*Config.h"
        , Test "libraries/dph"
        , Test "libraries/primitive"
        , Test "libraries/random"
        , Test "libraries/vector"
        , Test "rts/rts.cabal"
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
        , "libffi"
        , "libffi-tarballs"
        , "libraries"
        , "mk"
        , "rts"
        , "rules"
        , "utils" ]
    srcFiles =
        [ "GIT_COMMIT_ID"
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
        , "llvm-targets"
        , "llvm-passes"
        ]

    -- (stage, package, input file, output file)
    alexHappyFiles =
        [ (Stage0, compiler,      "GHC/Cmm/Parser.y",   "GHC/Cmm/Parser.hs")
        , (Stage0, compiler,      "GHC/Cmm/Lexer.x",    "GHC/Cmm/Lexer.hs")
        , (Stage0, compiler,      "GHC/Parser.y",       "GHC/Parser.hs")
        , (Stage0, compiler,      "GHC/Parser/Lexer.x", "GHC/Parser/Lexer.hs")
        , (Stage0, hpcBin,        "HpcParser.y",        "HpcParser.hs")
        , (Stage0, genprimopcode, "Parser.y",           "Parser.hs")
        , (Stage0, genprimopcode, "Lexer.x",            "Lexer.hs")
        ]
