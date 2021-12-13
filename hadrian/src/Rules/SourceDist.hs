module Rules.SourceDist (sourceDistRules) where

import Base
import Builder
import Context
import Oracles.Setting
import Packages
import Utilities (askWithResources, build)
import Hadrian.Target (target)
import qualified System.Directory.Extra as IO
import qualified Control.Exception.Base as IO
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
    "VERSION" %> \fname ->
        writeFileChanged fname =<< setting ProjectVersion

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
    when (not isDirectory) $
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
    need ["GIT_COMMIT_ID", "VERSION"]
    forM_ files $ \source -> do
      let target = dest -/- source
      copyFileSourceDist (top -/- source) target
    copyAlexHappyFiles
  where

    getFiles = filter treeFilter . split (=='\NUL')
    treeFilter file = not (null file) && not ("testsuite//" ?== file)

    copyAlexHappyFiles =
      forM_ alexHappyFiles $ \(stg, pkg, inp, out) -> do
        let ctx = Context stg pkg vanilla
            srcInputFile = dest -/- pkgPath pkg -/- inp
            generatedFile = dest -/- pkgPath pkg -/- out
            Just builder = determineBuilder stg inp

        -- We first make sure that the generated file is... generated.
        build $ target ctx builder [srcInputFile] [generatedFile]

        -- We finally add a ".source" suffix to the input file to
        -- prevent it from being used when building GHC, since the
        -- generated file being there already should prevent
        -- the need for the original input.
        moveFile srcInputFile (srcInputFile <.> "source")

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
