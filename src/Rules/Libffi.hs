module Rules.Libffi (rtsBuildPath, libffiRules, libffiDependencies) where

import Base
import Expression
import GHC
import Oracles
import Rules.Actions
import Settings.Builders.Common
import Settings.Packages.Rts
import Settings.Paths
import Settings.User

-- TODO: this should be moved elsewhere
rtsBuildPath :: FilePath
rtsBuildPath = targetPath Stage1 rts -/- "build"

-- TODO: Why copy these include files in rts? Move to libffi!
libffiDependencies :: [FilePath]
libffiDependencies = (rtsBuildPath -/-) <$> [ "ffi.h", "ffitarget.h" ]

libffiTarget :: PartialTarget
libffiTarget = PartialTarget Stage0 libffi

libffiBuild :: FilePath
libffiBuild = buildRootPath -/- "stage0/libffi"

libffiLibrary :: FilePath
libffiLibrary = libffiBuild -/- "inst/lib/libffi.a"

libffiMakefile :: FilePath
libffiMakefile = libffiBuild -/- "Makefile.in"

fixLibffiMakefile :: String -> String
fixLibffiMakefile = unlines . map
    ( replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" "$(subst ../install-sh,C:/msys/home/chEEtah/ghc/install-sh,@INSTALL@)"
    ) . lines

-- TODO: remove code duplication (see Settings/Builders/GhcCabal.hs)
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretPartial libffiTarget . fromDiffExpr $ mconcat
               [ cArgs
               , argStagedSettingList ConfCcArgs ]
    ldFlags <- interpretPartial libffiTarget $ fromDiffExpr ldArgs
    sequence [ builderEnv "CC" $ Gcc Stage0
             , builderEnv "CXX" $ Gcc Stage0
             , builderEnv "LD" Ld
             , builderEnv "AR" Ar
             , builderEnv "NM" Nm
             , builderEnv "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]
  where
    builderEnv var builder = do
        needBuilder False builder
        path <- builderPath builder
        return $ AddEnv var path

configureArguments :: Action [String]
configureArguments = do
    top            <- topDirectory
    targetPlatform <- setting TargetPlatform
    return [ "--prefix=" ++ top -/- libffiBuild -/- "inst"
           , "--libdir=" ++ top -/- libffiBuild -/- "inst/lib"
           , "--enable-static=yes"
           , "--enable-shared=no" -- TODO: add support for yes
           , "--host=" ++ targetPlatform ]

-- TODO: remove code duplication (need sourcePath)
-- TODO: split into multiple rules
libffiRules :: Rules ()
libffiRules = do
    libffiDependencies &%> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/Libffi.hs"]
        liftIO $ removeFiles libffiBuild ["//*"]
        tarballs <- getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
        when (length tarballs /= 1) $
            putError $ "libffiRules: exactly one libffi tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."

        need tarballs
        let libname = dropExtension . dropExtension . takeFileName $ head tarballs

        withTempDir $ \tmpDir -> do
            let unifiedTmpDir = unifyPath tmpDir
            build $ fullTarget libffiTarget Tar tarballs [unifiedTmpDir]
            moveDirectory (unifiedTmpDir -/- libname) libffiBuild

        fixFile libffiMakefile fixLibffiMakefile

        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiBuild -/- file)

        envs <- configureEnvironment
        args <- configureArguments
        runConfigure libffiBuild envs args

        runMake libffiBuild ["MAKEFLAGS="]
        runMake libffiBuild ["MAKEFLAGS=", "install"]

        forM_ ["ffi.h", "ffitarget.h"] $ \file -> do
            let src = libffiBuild -/- "inst/lib" -/- libname -/- "include" -/- file
            copyFile src (rtsBuildPath -/- file)

        libffiName <- rtsLibffiLibraryName
        copyFile libffiLibrary (rtsBuildPath -/- "lib" ++ libffiName <.> "a")

        putSuccess $ "| Successfully built custom library 'libffi'"

-- chmod +x libffi/ln
-- # wc on OS X has spaces in its output, which libffi's Makefile
-- # doesn't expect, so we tweak it to sed them out
-- mv libffi/build/Makefile libffi/build/Makefile.orig
-- sed "s#wc -w#wc -w | sed 's/ //g'#" < libffi/build/Makefile.orig > libffi/build/Makefile
