module Rules.Rts (rtsRules, needRtsLibffiTargets, needRtsSymLinks) where

import Packages (rts, rtsBuildPath, libffiBuildPath, libffiLibraryName, rtsContext)
import Rules.Libffi
import Hadrian.Utilities
import Settings.Builders.Common

-- | This rule has priority 3 to override the general rule for generating shared
-- library files (see Rules.Library.libraryRules).
rtsRules :: Rules ()
rtsRules = priority 3 $ do
    -- Dynamic RTS library files need symlinks without the dummy version number.
    -- This is for backwards compatibility (the old make build system omitted the
    -- dummy version number).
    root <- buildRootRules
    [ root -/- "**/libHSrts_*-ghc*.so",
      root -/- "**/libHSrts_*-ghc*.dylib",
      root -/- "**/libHSrts-ghc*.so",
      root -/- "**/libHSrts-ghc*.dylib"]
      |%> \ rtsLibFilePath' -> createFileLink
            (addRtsDummyVersion $ takeFileName rtsLibFilePath')
            rtsLibFilePath'

    -- Libffi
    forM_ [Stage1 ..] $ \ stage -> do
        let buildPath = root -/- buildDir (rtsContext stage)

        -- Header files
        (fmap (buildPath -/-) libffiHeaderFiles) &%> const (copyLibffiHeaders stage)

        -- Static libraries.
        buildPath -/- "libCffi*.a"     %> copyLibffiStatic stage

        -- Dynamic libraries
        buildPath -/- "libffi*.dylib*" %> copyLibffiDynamicUnix stage ".dylib"
        buildPath -/- "libffi*.so*"    %> copyLibffiDynamicUnix stage ".so"
        buildPath -/- "libffi*.dll*"   %> copyLibffiDynamicWin  stage

withLibffi :: Stage -> (FilePath -> FilePath -> Action a) -> Action a
withLibffi stage action = needLibffi stage
                        >> (join $ action <$> libffiBuildPath stage
                                          <*> rtsBuildPath    stage)

-- | Copy all header files wither from the system libffi or from the libffi
-- build dir to the rts build dir.
copyLibffiHeaders :: Stage -> Action ()
copyLibffiHeaders stage = do
    rtsPath      <- rtsBuildPath stage
    useSystemFfi <- flag UseSystemFfi
    (fromStr, headers) <- if useSystemFfi
        then ("system",) <$> libffiSystemHeaders
        else needLibffi stage
          >> ("custom",) <$> libffiHeaders stage
    forM_ headers $ \ header -> copyFile header
                                         (rtsPath -/- takeFileName header)
    putSuccess $ "| Successfully copied " ++ fromStr ++ " FFI library header "
                ++ "files to RTS build directory."

-- | Copy a static library file from the libffi build dir to the rts build dir.
copyLibffiStatic :: Stage -> FilePath -> Action ()
copyLibffiStatic stage target = withLibffi stage $ \ libffiPath _ -> do
    -- Copy the vanilla library, and symlink the rest to it.
    vanillaLibFile <- rtsLibffiLibrary stage vanilla
    if target == vanillaLibFile
    then copyFile' (libffiPath -/- libffiLibrary) target
    else createFileLink (takeFileName vanillaLibFile) target


-- | Copy a dynamic library file from the libffi build dir to the rts build dir.
copyLibffiDynamicUnix :: Stage -> String -> FilePath -> Action ()
copyLibffiDynamicUnix stage libSuf target = do
    needLibffi stage
    dynLibs <- askLibffilDynLibs stage

    -- If no version number suffix, then copy else just symlink.
    let versionlessSourceFilePath = fromMaybe
                (error $ "Needed " ++ show target ++ " which is not any of " ++
                    "libffi's built shared libraries: " ++ show dynLibs)
                (find (libSuf `isSuffixOf`) dynLibs)
    let versionlessSourceFileName = takeFileName versionlessSourceFilePath
    if versionlessSourceFileName == takeFileName target
    then do
        copyFile' versionlessSourceFilePath target

        -- On OSX the dylib's id must be updated to a relative path.
        when osxHost $ cmd
            [ "install_name_tool"
            , "-id", "@rpath/" ++ takeFileName target
            , target
            ]
    else createFileLink versionlessSourceFileName target

-- | Copy a dynamic library file from the libffi build dir to the rts build dir.
copyLibffiDynamicWin :: Stage -> FilePath -> Action ()
copyLibffiDynamicWin stage target = do
    needLibffi stage
    dynLibs <- askLibffilDynLibs stage
    let source = fromMaybe
            (error $ "Needed " ++ show target ++ " which is not any of " ++
                "libffi's built shared libraries: " ++ show dynLibs)
            (find (\ lib -> takeFileName target == takeFileName lib) dynLibs)
    copyFile' source target

rtsLibffiLibrary :: Stage -> Way -> Action FilePath
rtsLibffiLibrary stage way = do
    name    <- libffiLibraryName
    suf     <- libsuf stage way
    rtsPath <- rtsBuildPath stage
    return $ rtsPath -/- "lib" ++ name ++ suf

-- | Get the libffi files bundled with the rts (header and library files).
-- Unless using the system libffi, this needs the libffi library. It must be
-- built before the targets can be calcuulated.
needRtsLibffiTargets :: Stage -> Action [FilePath]
needRtsLibffiTargets stage = do
    rtsPath      <- rtsBuildPath stage
    useSystemFfi <- flag UseSystemFfi

    -- Header files (in the rts build dir).
    let headers = fmap (rtsPath -/-) libffiHeaderFiles

    if useSystemFfi
    then return headers
    else do
        -- Need Libffi
        -- This returns the dynamic library files (in the Libffi build dir).
        needLibffi stage
        dynLibffSource <- askLibffilDynLibs stage

        -- Header files (in the rts build dir).
        let headers = fmap (rtsPath -/-) libffiHeaderFiles

        -- Dynamic library files (in the rts build dir).
        let dynLibffis = fmap (\ lib -> rtsPath -/- takeFileName lib)
                              dynLibffSource

        -- Static Libffi files (in the rts build dir).
        staticLibffis <- do
            ways <- interpretInContext (stageContext stage)
                                       (getLibraryWays <> getRtsWays)
            let staticWays = filter (not . wayUnit Dynamic) ways
            mapM (rtsLibffiLibrary stage) staticWays

        return $ concat [ headers, dynLibffis, staticLibffis ]

-- Need symlinks generated by rtsRules.
needRtsSymLinks :: Stage -> [Way] -> Action ()
needRtsSymLinks stage rtsWays
    = forM_ (filter (wayUnit Dynamic) rtsWays) $ \ way -> do
        let ctx = Context stage rts way
        libPath     <- libPath ctx
        distDir     <- distDir stage
        rtsLibFile  <- takeFileName <$> pkgLibraryFile ctx
        need [removeRtsDummyVersion (libPath </> distDir </> rtsLibFile)]

prefix, versionlessPrefix :: String
versionlessPrefix = "libHSrts"
prefix = versionlessPrefix ++ "-1.0"

-- removeRtsDummyVersion "a/libHSrts-1.0-ghc1.2.3.4.so"
--                    == "a/libHSrts-ghc1.2.3.4.so"
removeRtsDummyVersion :: FilePath -> FilePath
removeRtsDummyVersion = replaceLibFilePrefix prefix versionlessPrefix

-- addRtsDummyVersion "a/libHSrts-ghc1.2.3.4.so"
--                 == "a/libHSrts-1.0-ghc1.2.3.4.so"
addRtsDummyVersion :: FilePath -> FilePath
addRtsDummyVersion = replaceLibFilePrefix versionlessPrefix prefix

replaceLibFilePrefix :: String -> String -> FilePath -> FilePath
replaceLibFilePrefix oldPrefix newPrefix oldFilePath = let
    oldFileName = takeFileName oldFilePath
    newFileName = maybe
        (error $ "Expected RTS library file to start with " ++ oldPrefix)
        (newPrefix ++)
        (stripPrefix oldPrefix oldFileName)
    in replaceFileName oldFilePath newFileName
