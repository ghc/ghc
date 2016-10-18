module Rules.Libffi (rtsBuildPath, libffiRules, libffiDependencies) where

import Base
import Builder
import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.WindowsPath
import Rules.Actions
import Settings.Builders.Common
import Settings.Packages.Rts
import Settings.Paths
import Target
import UserSettings

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

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: remove code duplication (see Settings/Builders/GhcCabal.hs)
-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretInContext libffiContext . fromDiffExpr $ mconcat
               [ cArgs
               , argStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext libffiContext $ fromDiffExpr ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
             , builderEnvironment "CXX" $ Cc CompileC Stage1
             , builderEnvironment "LD" Ld
             , builderEnvironment "AR" Ar
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = do
    libffiDependencies &%> \_ -> do
        useSystemFfi <- flag UseSystemFfi
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsBuildPath -/- file)
            putSuccess $ "| Successfully copied system FFI library header files"
        else do
            build $ Target libffiContext (Make libffiBuildPath) [] []

            hs <- getDirectoryFiles "" [libffiBuildPath -/- "inst/lib/*/include/*"]
            forM_ hs $ \header ->
                copyFile header (rtsBuildPath -/- takeFileName header)

            libffiName <- rtsLibffiLibraryName
            copyFile libffiLibrary (rtsBuildPath -/- "lib" ++ libffiName <.> "a")

            putSuccess $ "| Successfully built custom library 'libffi'"

    libffiMakefile <.> "in" %> \mkIn -> do
        removeDirectory libffiBuildPath
        createDirectory $ buildRootPath -/- stageString Stage0
        let tarballs = "libffi-tarballs/libffi*.tar.gz"
        tarball <- unifyPath <$> getSingleton (getDirectoryFiles "" [tarballs])
                                 "Exactly one LibFFI tarball is expected."
        need [tarball]
        let libname = dropExtension . dropExtension $ takeFileName tarball

        removeDirectory (buildRootPath -/- libname)
        -- TODO: Simplify.
        actionFinally (do
            build $ Target libffiContext Tar [tarball] [buildRootPath]
            moveDirectory (buildRootPath -/- libname) libffiBuildPath) $
                removeFiles buildRootPath [libname <//> "*"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

    libffiMakefile %> \mk -> do
        need [mk <.> "in"]
        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiBuildPath -/- file)

        env <- configureEnvironment
        buildWithCmdOptions env $
            Target libffiContext (Configure libffiBuildPath) [mk <.> "in"] [mk]
