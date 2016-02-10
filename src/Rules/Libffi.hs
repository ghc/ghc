module Rules.Libffi (rtsBuildPath, libffiRules, libffiDependencies) where

import Base
import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.WindowsPath
import Rules.Actions
import Settings.Builders.Common
import Settings.Packages.Rts
import Settings.Paths
import Settings.User

-- TODO: this should be moved elsewhere
rtsBuildPath :: FilePath
rtsBuildPath = targetPath Stage1 rts -/- "build"

-- TODO: Why copy these include files into rts? Keep in libffi!
libffiDependencies :: [FilePath]
libffiDependencies = (rtsBuildPath -/-) <$> [ "ffi.h", "ffitarget.h" ]

libffiTarget :: PartialTarget
libffiTarget = PartialTarget Stage1 libffi

libffiBuild :: FilePath
libffiBuild = buildRootPath -/- "stage1/libffi"

libffiLibrary :: FilePath
libffiLibrary = libffiBuild -/- "inst/lib/libffi.a"

fixLibffiMakefile :: String -> String
fixLibffiMakefile =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" "$(subst ../install-sh,C:/msys/home/chEEtah/ghc/install-sh,@INSTALL@)"

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
        useSystemFfi <- flag UseSystemFfi
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsBuildPath -/- file)
            putSuccess $ "| Successfully copied system FFI library header files"
        else do
            removeDirectory libffiBuild
            createDirectory $ buildRootPath -/- stageString Stage0

            tarballs <- getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
            when (length tarballs /= 1) $
                 putError $ "libffiRules: exactly one libffi tarball expected"
                              ++ "(found: " ++ show tarballs ++ ")."

            need tarballs
            let libname = dropExtension . dropExtension . takeFileName $ head tarballs

            removeDirectory (buildRootPath -/- libname)
            -- TODO: Simplify.
            actionFinally (do
                build $ fullTarget libffiTarget Tar tarballs [buildRootPath]
                moveDirectory (buildRootPath -/- libname) libffiBuild) $
                    removeFiles buildRootPath [libname <//> "*"]

            fixFile (libffiBuild -/- "Makefile.in") fixLibffiMakefile

            forM_ ["config.guess", "config.sub"] $ \file ->
                copyFile file (libffiBuild -/- file)

            envs <- configureEnvironment
            args <- configureArguments
            runConfigure libffiBuild envs args

            runMake libffiBuild ["MAKEFLAGS="]
            runMake libffiBuild ["MAKEFLAGS=", "install"]

            let ffiHDir = libffiBuild -/- "inst/lib" -/- libname -/- "include"
            forM_ ["ffi.h", "ffitarget.h"] $ \file -> do
                copyFile (ffiHDir -/- file) (rtsBuildPath -/- file)

            libffiName <- rtsLibffiLibraryName
            copyFile libffiLibrary (rtsBuildPath -/- "lib" ++ libffiName <.> "a")

            putSuccess $ "| Successfully built custom library 'libffi'"
