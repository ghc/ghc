{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
module Package.Data (buildPackageData) where

import Package.Base

libraryArgs :: [Way] -> Args
libraryArgs ways = 
    let argEnable x suffix = arg $ (if x then "--enable-" else "--disable-") ++ suffix
    in mconcat
        [ argEnable False "library-for-ghci" -- TODO: why always disable?
        , argEnable (vanilla `elem` ways) "library-vanilla"
        , when (ghcWithInterpreter && not DynamicGhcPrograms && vanilla `elem` ways) $
            argEnable True "library-for-ghci"
        , argEnable (profiling `elem` ways) "library-profiling"
        , argEnable (dynamic   `elem` ways) "shared"
        ]

configureArgs :: Stage -> Settings -> Args
configureArgs stage settings = 
    let argConf key as = unless (null <$> as) $ joinArgs "--configure-option=" key "=" (as :: Args)

        cflags   = joinArgsSpaced (commonCcArgs `filterOut` ["-Werror"])
                                  (ConfCcArgs stage)
                                  (customCcArgs settings)
                                  (commonCcWarninigArgs)
        ldflags  = joinArgsSpaced commonLdArgs  (ConfGccLinkerArgs stage) (customLdArgs  settings)
        cppflags = joinArgsSpaced commonCppArgs (ConfCppArgs       stage) (customCppArgs settings)
                   
    in mconcat
        [ argConf "CFLAGS"   cflags
        , argConf "LDFLAGS"  ldflags
        , argConf "CPPFLAGS" cppflags
        , joinArgs "--gcc-options=" cflags " " ldflags
        , argConf "--with-iconv-includes"  $ arg IconvIncludeDirs
        , argConf "--with-iconv-libraries" $ arg IconvLibDirs
        , argConf "--with-gmp-includes"    $ arg GmpIncludeDirs
        , argConf "--with-gmp-libraries"   $ arg GmpLibDirs
        , when CrossCompiling $ argConf "--host" $ arg TargetPlatformFull -- TODO: why not host?
        , argConf "--with-cc" $ arg Gcc
        ]

buildPackageData :: Package -> TodoItem -> Rules ()
buildPackageData pkg @ (Package name path _) (stage, dist, settings) =
        ((path </> dist) </>) <$>
        [ "package-data.mk",
          "haddock-prologue.txt",
          "inplace-pkg-config",
          "setup-config",
          "build" </> "autogen" </> "cabal_macros.h",
          "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs" -- TODO: Is this needed? Also check out Paths_cpsa.hs.
        ] &%> \_ -> do
            need ["shake/src/Package/Data.hs"] -- Track changes in this file
            need [path </> name <.> "cabal"]
            when (doesFileExist $ path </> "configure.ac") $ need [path </> "configure"]
            run GhcCabal cabalArgs
            when (registerPackage settings) $ run (GhcPkg stage) ghcPkgArgs
            postProcessPackageData $ path </> dist </> "package-data.mk"
              where
                cabalArgs, ghcPkgArgs :: Args
                cabalArgs = mconcat
                    [ args "configure" path dist
                    -- this is a positional argument, hence:
                    -- * if it is empty, we need to emit one empty string argument
                    -- * if there are many, we must collapse them into one space-separated string
                    , joinArgsSpaced "" (customDllArgs settings)
                    , with $ Ghc stage -- TODO: used to be stage01 (using max Stage1 GHC)
                    , with $ GhcPkg stage

                    , customConfArgs settings
                    , libraryArgs =<< ways settings

                    , when hsColourSrcs $ with HsColour
                    , configureArgs stage settings

                    , when (stage == Stage0) $ bootPkgConstraints
                    , with Gcc
                    , when (stage /= Stage0) $ with Ld
                    
                    , with Ar
                    , with Alex
                    , with Happy
                    ] -- TODO: reorder with's

                ghcPkgArgs = args "update" "--force"
                    (when (stage == Stage0) $ arg "--package-db=libraries/bootstrapping.conf")
                    (path </> dist </> "inplace-pkg-config")
