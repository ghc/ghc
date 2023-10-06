{-# LANGUAGE MultiWayIf #-}

module Rules.Rts (rtsRules, needRtsLibffiTargets, needRtsSymLinks) where

import qualified Data.Set as Set

import Packages (rts)
import Rules.Libffi
import Hadrian.Utilities
import Settings.Builders.Common
import Context.Type

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
    forM_ [Stage1, Stage2, Stage3 ] $ \ stage -> do
        let buildPath = root -/- buildDir (rtsContext stage)

        -- Header files
        -- See Note [Packaging libffi headers] in GHC.Driver.CodeOutput.
        forM_ libffiHeaderFiles $ \header ->
            buildPath -/- "include" -/- header %> copyLibffiHeader stage

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

-- | Copy a header files wither from the system libffi or from the libffi
-- build dir to the rts build dir.
--
-- See Note [Packaging libffi headers] in GHC.Driver.CodeOutput.
copyLibffiHeader :: Stage -> FilePath -> Action ()
copyLibffiHeader stage header = do
    useSystemFfi <- flag UseSystemFfi
    (fromStr, headerDir) <- if useSystemFfi
        then ("system",) <$> libffiSystemHeaderDir
        else needLibffi stage
          >> ("custom",) <$> libffiHeaderDir stage
    copyFile
        (headerDir -/- takeFileName header)
        header
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
    name    <- interpretInContext ((rtsContext stage) { way = way }) libffiName
    suf <- if wayUnit Dynamic way
                then do
                  extension <- setting DynamicExtension -- e.g., .dll or .so
                  let suffix = waySuffix (removeWayUnit Dynamic way)
                  return (suffix ++ extension)
                -- Static suffix
                else return (waySuffix way ++ ".a") -- e.g., _p.a
    rtsPath <- rtsBuildPath stage
    return $ rtsPath -/- "lib" ++ name ++ suf

-- | Get the libffi files bundled with the rts (header and library files).
-- Unless using the system libffi, this needs the libffi library. It must be
-- built before the targets can be calculated.
needRtsLibffiTargets :: Stage -> Action [FilePath]
needRtsLibffiTargets stage = do
    rtsPath      <- rtsBuildPath stage
    useSystemFfi <- flag UseSystemFfi
    jsTarget     <- isJsTarget stage

    -- Header files (in the rts build dir).
    let headers = fmap ((rtsPath -/- "include") -/-) libffiHeaderFiles

    if | jsTarget     -> return []
       | useSystemFfi -> return []
       | otherwise    -> do
        -- Need Libffi
        -- This returns the dynamic library files (in the Libffi build dir).
        needLibffi stage
        dynLibffSource <- askLibffilDynLibs stage

        -- Dynamic library files (in the rts build dir).
        let dynLibffis = fmap (\ lib -> rtsPath -/- takeFileName lib)
                                  dynLibffSource

        -- Libffi files (in the rts build dir).
        libffis_libs <- do
            ways <- interpretInContext (stageContext stage)
                                       (getLibraryWays <> getRtsWays)
            mapM (rtsLibffiLibrary stage) (Set.toList ways)
        return $ concat [ headers, dynLibffis, libffis_libs ]

-- Need symlinks generated by rtsRules.
needRtsSymLinks :: Stage -> Set.Set Way -> Action ()
needRtsSymLinks stage rtsWays
    = forM_ (Set.filter (wayUnit Dynamic) rtsWays) $ \ way -> do
        let ctx = Context stage rts way Final
        distDir     <- distDynDir ctx
        rtsLibFile  <- takeFileName <$> pkgLibraryFile ctx
        need [removeRtsDummyVersion (distDir </> rtsLibFile)]

prefix, versionlessPrefix :: String
versionlessPrefix = "libHSrts"
prefix = versionlessPrefix ++ "-1.0.2"

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
