module Rules.SourceDist (sourceDistRules) where

import Base
import Builder
import Context
import Oracles.Setting
import Packages
import Utilities (askWithResources, build)
import Hadrian.Target (target)
import qualified System.Directory.Extra as IO
import Oracles.ModuleFiles (determineBuilder)

sourceDistRules :: Rules ()
sourceDistRules = alternatives $ do
    root <- buildRootRules
    "source-dist" ~> do
        version <- setting ProjectVersion
        need [root -/- "source-dist" -/- ("ghc-" ++ version ++ "-src.tar.xz")]
        need [root -/- "source-dist" -/- ("ghc-" ++ version ++ "-testsuite.tar.xz")]
        need [root -/- "source-dist" -/- ("ghc-" ++ version ++ "-windows-extra-src.tar.xz")]
        putSuccess "| Done"

    -- Ordering of rules is important so that windows-extra-src matches before src
    root -/- "source-dist" -/- "ghc-*-windows-extra-src.tar.xz" %>
      archiveSourceTree prepareWindowsExtraTree
    root -/- "source-dist" -/- "ghc-*-testsuite.tar.xz" %>
      archiveSourceTree prepareTestsuiteTree
    root -/- "source-dist" -/- "ghc-*-src.tar.xz" %>
      archiveSourceTree prepareTree
    "GIT_COMMIT_ID" %> \fname ->
        writeFileChanged fname =<< setting ProjectGitCommitId

    -- Rules to download mingw tarballs
    let mingw_tarballs_stamp = "ghc-tarballs/mingw-w64/.mingw-w64.download.stamp"
    ["ghc-tarballs/mingw-w64/*/*.tar.*","ghc-tarballs/mingw-w64/*/SHA256SUMS"]  |%> \_ ->
      need [mingw_tarballs_stamp]
    mingw_tarballs_stamp %> \stamp -> do
      build (target (vanillaContext Stage1 compiler) (Win32Tarballs DownloadTarballs) [] [])
      writeFile' stamp "OK"


archiveSourceTree :: (FilePath -> Action ()) -> FilePath -> Action ()
archiveSourceTree prepare fname = do
  root <- buildRoot
  version <- setting ProjectVersion
  let dropTarXz = dropExtension . dropExtension
      tarName   = takeFileName fname
      dirName   = dropTarXz tarName
      baseName  = "ghc-" ++ version
      treeDir   = dirName -/- baseName
      treePath  = sourceDistRoot -/- treeDir
      sourceDistRoot = root -/- "source-dist"
  removeDirectory treePath
  prepare treePath
  runBuilderWithCmdOptions
      [Cwd $ sourceDistRoot -/- dirName]
      (Tar Create)
      ["chJf", ".." -/- tarName,  baseName]
      ["chJf", ".." -/- tarName] [baseName]


-- | This creates a symlink to the 'source' at 'target'
-- $tar -h$ will eventually copy the source into the tarball
-- This is also how `make sdist` works.
-- 1. It preserves relative symlinks
-- 2. It copies non-empty directories also. This is because git includes
--    directories in its output if they are non empty.
copyFileSourceDist :: FilePath -> FilePath -> Action ()
copyFileSourceDist source target = do
  isSymlink <- liftIO $ IO.pathIsSymbolicLink source
  if isSymlink then do
    link_target <- liftIO $ IO.getSymbolicLinkTarget source
    when (not $ isRelative link_target) $
      error ("source-dist: tried to create non-relative symlink in source dist: " ++ show link_target)
    putProgressInfo =<< renderAction ("Create symlink (" ++ link_target ++ ")") source target
    isDirectory <- liftIO $ IO.doesDirectoryExist source
    -- We don't want to call `need` on broken symlinks
    linkTargetExists <- liftIO $ IO.doesPathExist link_target
    when (not isDirectory && linkTargetExists) $
      need [source]
    let createLink src tgt
          | isDirectory = liftIO $ IO.createDirectoryLink src tgt
          | otherwise = liftIO $ IO.createFileLink src tgt
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    liftIO $ removeFile_ target
    createLink link_target target
  else do
    isDirectory <- liftIO $ IO.doesDirectoryExist source
    if isDirectory then do
      contents <- liftIO $ IO.listDirectory source
      when (not $ null contents) $ -- Git only includes directories in the output if they are empty
        error ("source-dist: non-empty dir" ++ show source)
      createDirectory target
    else createFileLink source target

prepareTestsuiteTree :: FilePath -> Action ()
prepareTestsuiteTree dest = do
  top <- topDirectory
  let testsuiteFiles = filter testFilter . split (=='\NUL')
      testFilter file = not (null file) && ("testsuite//" ?== file)
  files <- testsuiteFiles <$> askWithResources [] (target (vanillaContext Stage1 compiler) (Git ListFiles) [] [])
  forM_ files $ \source -> do
    let target = dest -/- source
    copyFileSourceDist (top -/- source) target

prepareWindowsExtraTree :: FilePath -> Action ()
prepareWindowsExtraTree dest = do
  top <- topDirectory

  files <- lines <$> askWithResources [] (target (vanillaContext Stage1 compiler) (Win32Tarballs ListTarballs) [] [])
  need files
  build (target (vanillaContext Stage1 compiler) (Win32Tarballs VerifyTarballs) [] [])

  createDirectory dest
  liftIO $ IO.createFileLink (top -/- "ghc-tarballs") (dest -/- "ghc-tarballs")

prepareTree :: FilePath -> Action ()
prepareTree dest = do
    out <- askWithResources [] (target (vanillaContext Stage1 compiler) (Git ListFiles) [] [])
    top <- topDirectory
    let files = ["GIT_COMMIT_ID", "VERSION"] ++ getFiles out
    need ["GIT_COMMIT_ID"]
    forM_ files $ \source -> do
      let target = dest -/- source
      copyFileSourceDist (top -/- source) target
    copyAlexHappyFiles
    copyBootFiles
  where

    getFiles = filter treeFilter . split (=='\NUL')
    treeFilter file = not (null file) && not ("testsuite//" ?== file)

    -- Copy files created by running ./boot
    copyBootFiles = do
      top <- topDirectory
      forM_ bootFiles $ \file -> do
        let src_file = top -/- file
            dest_file = top -/- dest -/- file
        createFileLink src_file dest_file
      -- And move ./boot so we can't accidentally call it in CI
      moveFile (dest -/- "boot") (dest -/- "boot.source")

    bootFiles =
      [ pkgPath rts -/- "configure"
      , pkgPath rts -/- "ghcautoconf.h.autoconf.in"
      , pkgPath process -/- "include" -/- "HsProcessConfig.h.in"
      , pkgPath process -/- "configure"
      , pkgPath ghcBignum -/- "configure"
      , pkgPath ghcInternal -/- "configure"
      , pkgPath ghcInternal -/- "include" -/- "HsBaseConfig.h.in"
      , pkgPath directory -/- "configure"
      , pkgPath directory -/- "HsDirectoryConfig.h.in"
      , pkgPath time -/- "configure"
      , pkgPath time -/- "lib" -/- "include" -/- "HsTimeConfig.h.in"
      , pkgPath unix -/- "configure"
      , pkgPath unix -/- "include" -/- "HsUnixConfig.h.in"
      , pkgPath terminfo -/- "configure"
      , "configure"
      , "aclocal.m4"
      ]

    copyAlexHappyFiles =
      forM_ alexHappyFiles $ \(stg, pkg, inp, out) -> do
        let ctx = Context stg pkg vanilla Inplace
            srcInputFile = dest -/- pkgPath pkg -/- inp
            generatedFile = dest -/- pkgPath pkg -/- out
            builder =
                case determineBuilder stg inp of
                  Just builder -> builder
                  Nothing -> error $ "Failed to determine builder for " ++ inp

        -- We first make sure that the generated file is... generated.
        build $ target ctx builder [srcInputFile] [generatedFile]

        -- We finally add a ".source" suffix to the input file to
        -- prevent it from being used when building GHC, since the
        -- generated file being there already should prevent
        -- the need for the original input.
        moveFile srcInputFile (srcInputFile <.> "source")

    -- (stage, package, input file, output file)
    alexHappyFiles =
        [ (stage0InTree , compiler,      "GHC/Cmm/Parser.y",   "GHC/Cmm/Parser.hs")
        , (stage0InTree , compiler,      "GHC/Cmm/Lexer.x",    "GHC/Cmm/Lexer.hs")
        , (stage0InTree , compiler,      "GHC/Parser.y",       "GHC/Parser.hs")
        , (stage0InTree , compiler,      "GHC/Parser/Lexer.x", "GHC/Parser/Lexer.hs")
        , (stage0InTree , compiler,      "GHC/Parser/HaddockLex.x", "GHC/Parser/HaddockLex.hs")
        , (stage0InTree , hpcBin,        "src/Trace/Hpc/Parser.y", "src/Trace/Hpc/Parser.hs")
        , (stage0InTree , genprimopcode, "Parser.y",           "Parser.hs")
        , (stage0InTree , genprimopcode, "Lexer.x",            "Lexer.hs")
        , (stage0InTree , cabalSyntax  , "src/Distribution/Fields/Lexer.x",  "src/Distribution/Fields/Lexer.hs")
        ]
