module Rules.Libffi (libffiRules, libffiLibrary) where

import System.Directory

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.Builders.Common

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

-- Not efficient, but simple and works fast enough for libffi Makefile.in
replace :: String -> String -> String -> String
replace from to = go
  where
    skipFrom = drop $ length from
    go [] = []
    go s @ (x : xs)
        | from `isPrefixOf` s = to ++ go (skipFrom s)
        | otherwise           = x  :  go xs

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
        need [sourcePath -/- "Rules/Libffi.hs"]
        liftIO $ removeFiles libffiBuild ["//*"]
        tarballs <- getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
        when (length tarballs /= 1) $
            putError $ "libffiRules: exactly one libffi tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."
        let libname = dropExtension . dropExtension . takeFileName $ head tarballs

        need tarballs
        build $ fullTarget target Tar tarballs ["libffi-tarballs"]

        let libffiExtracted = "libffi-tarballs" -/- libname
        liftIO $ renameDirectory libffiExtracted libffiBuild
        putBuild $ "| Move " ++ libffiExtracted ++ " -> " ++ libffiBuild

        old <- liftIO $ readFile libffiMakefile
        let new = fixLibffiMakefile old
        length new `seq` liftIO $ writeFile libffiMakefile new
        putBuild $ "| Fix " ++ libffiMakefile

        forM_ ["config.guess", "config.sub"] $ \file -> do
            copyFileChanged file $ libffiBuild -/- file
            putBuild $ "| Copy " ++ file ++ " -> " ++ (libffiBuild -/- file)

        putBuild $ "| Running libffi configure..."
        envs <- configureEnvironment
        args <- configureArguments
        unit $ cmd Shell [Cwd libffiBuild] "bash configure" envs args

        putBuild $ "| Running make..."
        unit $ cmd Shell "make" ["-C", libffiBuild, "MAKEFLAGS="]

        putBuild $ "| Running make install..."
        unit $ cmd Shell "make" ["-C", libffiBuild, "MAKEFLAGS= install"]

        putSuccess $ "| Successfully built custom library 'libffi'"

    "libffi/build/inst/lib/*/include/*.h" %> \_ -> need [libffiLibrary]

-- chmod +x libffi/ln
-- # wc on OS X has spaces in its output, which libffi's Makefile
-- # doesn't expect, so we tweak it to sed them out
-- mv libffi/build/Makefile libffi/build/Makefile.orig
-- sed "s#wc -w#wc -w | sed 's/ //g'#" < libffi/build/Makefile.orig > libffi/build/Makefile
