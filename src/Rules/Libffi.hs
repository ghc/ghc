module Rules.Libffi (rtsBuildPath, libffiRules, libffiDependencies) where

import Base
import Builder
import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Rules.Actions
import Settings.Builders.Common
import Settings.Packages.Rts
import Settings.Paths
import Settings.User
import Target

-- TODO: this should be moved elsewhere
rtsBuildPath :: FilePath
rtsBuildPath = buildPath rtsContext

-- TODO: Why copy these include files into rts? Keep in libffi!
libffiDependencies :: [FilePath]
libffiDependencies = (rtsBuildPath -/-) <$> [ "ffi.h", "ffitarget.h" ]

libffiContext :: Context
libffiContext = vanillaContext Stage1 libffi

libffiLibrary :: FilePath
libffiLibrary = libffiBuildPath -/- "inst/lib/libffi.a"

libffiMakefile :: FilePath
libffiMakefile = libffiBuildPath -/- "Makefile"

fixLibffiMakefile :: String -> String
fixLibffiMakefile =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" "$(subst ../install-sh,C:/msys/home/chEEtah/ghc/install-sh,@INSTALL@)"

-- TODO: remove code duplication (see Settings/Builders/GhcCabal.hs)
-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretInContext libffiContext . fromDiffExpr $ mconcat
               [ cArgs
               , argStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext libffiContext $ fromDiffExpr ldArgs
    sequence [ builderEnvironment "CC" $ Cc Compile Stage1
             , builderEnvironment "CXX" $ Cc Compile Stage1
             , builderEnvironment "LD" Ld
             , builderEnvironment "AR" Ar
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

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
            removeDirectory libffiBuildPath
            createDirectory $ buildRootPath -/- stageString Stage0

            tarballs <- getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
            tarball  <- case tarballs of
                [file] -> return $ unifyPath file
                _      -> putError $ "libffiRules: exactly one tarball expected"
                          ++ "(found: " ++ show tarballs ++ ")."

            need [tarball]
            let libname = dropExtension . dropExtension $ takeFileName tarball

            removeDirectory (buildRootPath -/- libname)
            -- TODO: Simplify.
            actionFinally (do
                build $ Target libffiContext Tar [tarball] [buildRootPath]
                moveDirectory (buildRootPath -/- libname) libffiBuildPath) $
                    removeFiles buildRootPath [libname <//> "*"]

            fixFile (libffiMakefile <.> "in") fixLibffiMakefile

            forM_ ["config.guess", "config.sub"] $ \file ->
                copyFile file (libffiBuildPath -/- file)

            env <- configureEnvironment
            buildWithCmdOptions env $
                Target libffiContext (Configure libffiBuildPath)
                       [libffiMakefile <.> "in"] [libffiMakefile]

            -- The old build system did runMake libffiBuildPath ["MAKEFLAGS="]
            -- TODO: Find out why. It seems redundant, so I removed it.
            runMake libffiBuildPath ["MAKEFLAGS=", "install"]

            let ffiHDir = libffiBuildPath -/- "inst/lib" -/- libname -/- "include"
            forM_ ["ffi.h", "ffitarget.h"] $ \file -> do
                copyFile (ffiHDir -/- file) (rtsBuildPath -/- file)

            libffiName <- rtsLibffiLibraryName
            copyFile libffiLibrary (rtsBuildPath -/- "lib" ++ libffiName <.> "a")

            putSuccess $ "| Successfully built custom library 'libffi'"
