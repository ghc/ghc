module Rules.Libffi (libffiRules, libffiDependencies) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

libffiDependencies :: [FilePath]
libffiDependencies = ["ffi.h", "ffitarget.h"]

libffiLibrary :: FilePath
libffiLibrary = "inst/lib/libffi.a"

rtsLibffiLibrary :: Stage -> Way -> Action FilePath
rtsLibffiLibrary stage way = do
    name    <- libffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath stage
    return $ rtsPath -/- "lib" ++ name ++ suf

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Stage -> Action [CmdOption]
configureEnvironment stage = do
    cFlags  <- interpretInContext (libffiContext stage) $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext (libffiContext stage) ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC stage
             , builderEnvironment "CXX" $ Cc CompileC stage
             , builderEnvironment "LD" (Ld stage)
             , builderEnvironment "AR" (Ar Unpack stage)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = forM_ [Stage1 ..] $ \stage -> do
    root <- buildRootRules
    let path       = root -/- stageString stage
        libffiPath = path -/- pkgName libffi -/- "build"
        libffiOuts = [libffiPath -/- libffiLibrary] ++
                     fmap ((path -/- "rts/build") -/-) libffiDependencies

    -- We set a higher priority because this rule overlaps with the build rule
    -- for static libraries 'Rules.Library.libraryRules'.
    priority 2.0 $ libffiOuts &%> \(out : _) -> do
        useSystemFfi <- flag UseSystemFfi
        rtsPath      <- rtsBuildPath stage
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsPath -/- file)
            putSuccess "| Successfully copied system FFI library header files"
        else do
            build $ target (libffiContext stage) (Make libffiPath) [] []

            -- Here we produce 'libffiDependencies'
            hs <- liftIO $ getDirectoryFilesIO "" [libffiPath -/- "inst/include/*"]
            forM_ hs $ \header -> do
                let target = rtsPath -/- takeFileName header
                copyFileUntracked header target
                produces [target]

            ways <- interpretInContext (libffiContext stage)
                                       (getLibraryWays <> getRtsWays)
            forM_ (nubOrd ways) $ \way -> do
                rtsLib <- rtsLibffiLibrary stage way
                copyFileUntracked out rtsLib
                produces [rtsLib]

            putSuccess "| Successfully built custom library 'libffi'"

    fmap (libffiPath -/-) ["Makefile.in", "configure" ] &%> \[mkIn, _] -> do
        removeDirectory libffiPath
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]

        need [tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        root <- buildRoot
        removeDirectory (root -/- libname)
        -- TODO: Simplify.
        actionFinally (do
            build $ target (libffiContext stage) (Tar Extract) [tarball] [path]
            moveDirectory (path -/- libname) libffiPath) $
            -- And finally:
            removeFiles (path) [libname <//> "*"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

        files <- liftIO $ getDirectoryFilesIO "." [libffiPath <//> "*"]
        produces files

    fmap (libffiPath -/-) ["Makefile", "config.guess", "config.sub"] &%> \[mk, _, _] -> do
        need [mk <.> "in"]
        forM_ ["config.guess", "config.sub"] $ \file -> do
            copyFile file (libffiPath -/- file)
        env <- configureEnvironment stage
        buildWithCmdOptions env $
            target (libffiContext stage) (Configure libffiPath) [mk <.> "in"] [mk]

        dir   <- setting BuildPlatform
        files <- liftIO $ getDirectoryFilesIO "." [libffiPath -/- dir <//> "*"]
        produces files
