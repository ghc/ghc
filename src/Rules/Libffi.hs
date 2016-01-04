module Rules.Libffi (libffiRules, libffiLibrary) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.Builders.Common
import Settings.User

-- We use this file to track the whole libffi library
libffiLibrary :: FilePath
libffiLibrary = libffiBuild -/- "inst/lib/libffi.a"

libffiBuild :: FilePath
libffiBuild = "libffi/build"

libffiMakefile :: FilePath
libffiMakefile = libffiBuild -/- "Makefile.in"

fixLibffiMakefile :: String -> String
fixLibffiMakefile = unlines . map
    ( replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" "$(subst ../install-sh,C:/msys/home/chEEtah/ghc/install-sh,@INSTALL@)"
    ) . lines

target :: PartialTarget
target = PartialTarget Stage0 libffi

-- TODO: remove code duplication (see Settings/Builders/GhcCabal.hs)
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretPartial target . fromDiffExpr $ mconcat
               [ cArgs
               , argStagedSettingList ConfCcArgs ]
    ldFlags <- interpretPartial target $ fromDiffExpr ldArgs
    sequence [ builderEnv "CC" $ Gcc Stage1
             , builderEnv "CXX" $ Gcc Stage1
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
    top            <- setting GhcSourcePath
    targetPlatform <- setting TargetPlatform
    return [ "--prefix=" ++ top ++ "/libffi/build/inst"
           , "--libdir=" ++ top ++ "/libffi/build/inst/lib"
           , "--enable-static=yes"
           , "--enable-shared=no" -- TODO: add support for yes
           , "--host=" ++ targetPlatform ]

libffiRules :: Rules ()
libffiRules = do
    libffiLibrary %> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/Libffi.hs"]
        liftIO $ removeFiles libffiBuild ["//*"]
        tarballs <- getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
        when (length tarballs /= 1) $
            putError $ "libffiRules: exactly one libffi tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."

        need tarballs
        build $ fullTarget target Tar tarballs ["libffi-tarballs"]

        let libname = dropExtension . dropExtension . takeFileName $ head tarballs
        moveDirectory ("libffi-tarballs" -/- libname) libffiBuild

        fixFile libffiMakefile fixLibffiMakefile

        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiBuild -/- file)

        envs <- configureEnvironment
        args <- configureArguments
        runConfigure libffiBuild envs args

        runMake libffiBuild []
        runMake libffiBuild ["install"]

        putSuccess $ "| Successfully built custom library 'libffi'"

    "libffi/build/inst/lib/*/include/*.h" %> \_ -> need [libffiLibrary]

-- chmod +x libffi/ln
-- # wc on OS X has spaces in its output, which libffi's Makefile
-- # doesn't expect, so we tweak it to sed them out
-- mv libffi/build/Makefile libffi/build/Makefile.orig
-- sed "s#wc -w#wc -w | sed 's/ //g'#" < libffi/build/Makefile.orig > libffi/build/Makefile
