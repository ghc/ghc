module Rules.Libffi (libffiRules, libffiDependencies, libffiName) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

{-
Note [Hadrian: install libffi hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 2 important steps in handling libffi's .a and .so files:

  1. libffi's .a and .so|.dynlib|.dll files are copied from the libffi build dir
  to the rts build dir. This is because libffi is ultimately bundled with the
  rts package. Relevant code is in the libffiRules function.
  2. The rts is "installed" via the hadrian/src/Hadrian/Haskell/Cabal/Parse.hs
  copyPackage action. This uses the "cabal copy" command which (among other
  things) attempts to copy the bundled .a and .so|.dynlib|.dll files from the
  rts build dir to the install dir.

There is an issue in step 1. that the name of the shared library files is not
know untill after libffi is built. As a workaround, the rts package needs just
the libffiDependencies, and the corresponding rule (defined below in
libffiRules) does the extra work of installing the shared library files into the
rts build directory after building libffi.
-}

-- | Context for @libffi@.
libffiContext :: Stage -> Action Context
libffiContext stage = do
    ways <- interpretInContext
            (Context stage libffi (error "libffiContext: way not set"))
            getLibraryWays
    return . Context stage libffi $ if any (wayUnit Dynamic) ways
        then dynamic
        else vanilla

-- | The name of the (locally built) library
libffiName :: Expr String
libffiName = do
    windows <- expr windowsHost
    way <- getWay
    return $ libffiName' windows (Dynamic `wayUnit` way)

-- | The name of the (locally built) library
libffiName' :: Bool -> Bool -> String
libffiName' windows dynamic
    = (if dynamic then "" else "C")
    ++ (if windows then "ffi-6" else "ffi")

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
    context <- libffiContext stage
    cFlags  <- interpretInContext context $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext context ldArgs
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
    -- See [Hadrian: install libffi hack], this rule installs libffi into the
    -- rts build path.
    priority 2.0 $ libffiOuts &%> \_ -> do
        context <- libffiContext stage
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
            build $ target context (Make libffiPath) [] []

            -- Here we produce 'libffiDependencies'
            headers <- liftIO $ getDirectoryFilesIO libffiPath ["inst/include/*"]
            forM_ headers $ \header -> do
                let target = rtsPath -/- takeFileName header
                copyFileUntracked (libffiPath -/- header) target
                produces [target]

            -- Find ways.
            ways <- interpretInContext context
                                       (getLibraryWays <> getRtsWays)
            let (dynamicWays, staticWays) = partition (wayUnit Dynamic) ways

            -- Install static libraries.
            forM_ staticWays $ \way -> do
                rtsLib <- rtsLibffiLibrary stage way
                copyFileUntracked (libffiPath -/- "inst/lib/libffi.a") rtsLib
                produces [rtsLib]

            -- Install dynamic libraries.
            when (not $ null dynamicWays) $ do
                -- Find dynamic libraries.
                windows <- windowsHost
                osx     <- osxHost
                let libffiName'' = libffiName' windows True
                (dynLibsSrcDir, dynLibFiles) <- if windows
                    then do
                        let libffiDll = "lib" ++ libffiName'' ++ ".dll"
                        return (libffiPath -/- "inst/bin", [libffiDll])
                    else do
                        let libffiLibPath = libffiPath -/- "inst/lib"
                        dynLibsRelative <- liftIO $ getDirectoryFilesIO
                            libffiLibPath
                            (if osx
                                then ["lib" ++ libffiName'' ++ ".dylib*"]
                                else ["lib" ++ libffiName'' ++ ".so*"])
                        return (libffiLibPath, dynLibsRelative)

                -- Install dynamic libraries.
                rtsPath <- rtsBuildPath stage
                forM_ dynLibFiles $ \dynLibFile -> do
                    let target = rtsPath -/- dynLibFile
                    copyFileUntracked (dynLibsSrcDir -/- dynLibFile) target

                    -- On OSX the dylib's id must be updated to a relative path.
                    when osx $ cmd
                        [ "install_name_tool"
                        , "-id", "@rpath/" ++ dynLibFile
                        , target
                        ]

                    produces [target]

            putSuccess "| Successfully bundled custom library 'libffi' with rts"

    fmap (libffiPath -/-) ["Makefile.in", "configure" ] &%> \[mkIn, _] -> do
        -- Extract libffi tar file
        context <- libffiContext stage
        removeDirectory libffiPath
        top <- topDirectory
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles top ["libffi-tarballs/libffi*.tar.gz"]

        need [top -/- tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        -- Move extracted directory to libffiPath.
        root <- buildRoot
        removeDirectory (root -/- libname)
        actionFinally (do
            build $ target context (Tar Extract) [tarball] [path]
            moveDirectory (path -/- libname) libffiPath) $
            -- And finally:
            removeFiles (path) [libname <//> "*"]

        fixFile mkIn (fixLibffiMakefile top)

        files <- liftIO $ getDirectoryFilesIO "." [libffiPath <//> "*"]
        produces files

    fmap (libffiPath -/-) ["Makefile", "config.guess", "config.sub"] &%> \[mk, _, _] -> do
        context <- libffiContext stage

        -- This need rule extracts the libffi tar file to libffiPath.
        need [mk <.> "in"]

        -- Configure.
        forM_ ["config.guess", "config.sub"] $ \file -> do
            copyFile file (libffiPath -/- file)
        env <- configureEnvironment stage
        buildWithCmdOptions env $
            target context (Configure libffiPath) [mk <.> "in"] [mk]

        dir   <- setting BuildPlatform
        files <- liftIO $ getDirectoryFilesIO "." [libffiPath -/- dir <//> "*"]
        produces files
