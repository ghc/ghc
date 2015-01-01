{-# LANGUAGE NoImplicitPrelude #-}
module Package (
    packageRules
    ) where

import Base
import Util
import Ways
import Oracles

-- These are the packages we build
packages :: [Package]
packages = [libraryPackage "deepseq" Stage1 defaultSettings]

data Settings = Settings
     {
         customConfArgs  :: Args,
         customCcArgs    :: Args,
         customLdArgs    :: Args,
         customCppArgs   :: Args,
         customDllArgs   :: Args,
         registerPackage :: Bool,
         ways            :: Action [Way]
     }

defaultSettings :: Stage -> Settings
defaultSettings stage = Settings mempty mempty mempty mempty mempty True (defaultWays stage)

type TodoItem = (Stage, FilePath, Settings)
-- Stage is the stage of the GHC that we use to build the package
-- FilePath is the directory to put the build results
-- Settings are various Args which may be different for different combinations of Stage & FilePath
-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName :: String,     -- e.g., "deepseq"
         pkgPath :: FilePath,   -- e.g., "libraries/deepseq"
         pkgTodo :: [TodoItem]  -- e.g., [(Stage1, "dist-install", defaultSettings)]
     }

libraryPackage :: String -> Stage -> (Stage -> Settings) -> Package
libraryPackage name stage settings =
    Package
        name
        ("libraries" </> name)
        [(
            stage,
            if stage == Stage0 then "dist-boot" else "dist-install",
            settings stage
        )]

commonCcArgs :: Args
commonCcArgs = when Validating $ arg ["-Werror", "-Wall"]

commonLdArgs :: Args
commonLdArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCppArgs :: Args
commonCppArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCcWarninigArgs :: Args
commonCcWarninigArgs = when Validating $ mconcat
    [ when GccIsClang                      $ arg ["-Wno-unknown-pragmas"]
    , when (not GccIsClang && not GccLt46) $ arg ["-Wno-error=inline"]
    , when (    GccIsClang && not GccLt46) $ hostOsCppWarning
    ]
      where
        hostOsCppWarning = do
            hostOsCpp <- option HostOsCpp
            when (hostOsCpp /= "mingw32") $ arg ["-Werror=unused-but-set-variable" ]

bootPkgConstraints :: Args
bootPkgConstraints = mempty

libraryArgs :: [Way] -> Args
libraryArgs ways = 
    let argEnable x suffix = arg [(if x then "--enable-" else "--disable-") ++ suffix]
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
    let argConf key args = joinArgs $ arg ["--configure-option=", key, "="] <> joinArgsWithSpaces args

        argConfWith key opt = do
            value <- option opt
            when (value /= "") $ argConf ("--with-" ++ key) $ arg [value]

        cflags   = mconcat
                   [ commonCcArgs `filterOut` ["-Werror"]
                   , argOption $ ConfCcArgs stage
                   , customCcArgs settings
                   , commonCcWarninigArgs
                   ]
        ldflags  = mconcat [  commonLdArgs, argOption $ ConfGccLinkerArgs stage,  customLdArgs settings ]
        cppflags = mconcat [ commonCppArgs, argOption $ ConfCppArgs       stage, customCppArgs settings ]
                   
    in mconcat
        [ argConf "CFLAGS"   cflags
        , argConf "LDFLAGS"  ldflags
        , argConf "CPPFLAGS" cppflags
        , joinArgs $ mconcat [arg ["--gcc-options="], cflags, arg [" "], ldflags]
        , argConfWith "iconv-includes"  IconvIncludeDirs
        , argConfWith "iconv-libraries" IconvLibDirs
        , argConfWith "gmp-includes"    GmpIncludeDirs
        , argConfWith "gmp-libraries"   GmpLibDirs
        , when CrossCompiling $ argConf "--host" $ argOption $ TargetPlatformFull -- TODO: why not host?
        , argConf "--with-cc" $ argPath Gcc
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
            need [path </> name <.> "cabal"]
            when (doesFileExist $ path </> "configure.ac") $ need [path </> "configure"]
            run GhcCabal cabalArgs
            when (registerPackage settings) $ run (GhcPkg stage) ghcPkgArgs
            postProcessPackageData $ path </> dist </> "package-data.mk"
              where
                cabalArgs, ghcPkgArgs :: Args
                cabalArgs = mconcat
                    [ arg ["configure", path, dist]
                    -- this is a positional argument, hence:
                    -- * if it is empty, we need to emit one empty string argument
                    -- * if there are many, we must collapse them into one string argument
                    , joinArgsWithSpaces $ customDllArgs settings 
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

                ghcPkgArgs = mconcat
                    [ arg ["update", "--force"]
                    , when (stage == Stage0) $ arg ["--package-db=libraries/bootstrapping.conf"]
                    , arg [path </> dist </> "inplace-pkg-config"]
                    ]


-- $1_$2_$3_MOST_DIR_HC_OPTS = \
--  $$($1_$2_$3_MOST_HC_OPTS) \
--  -odir $1/$2/build -hidir $1/$2/build -stubdir $1/$2/build

-- # Some of the Haskell files (e.g. utils/hsc2hs/Main.hs) (directly or
-- # indirectly) include the generated includes files.
-- $$($1_$2_depfile_haskell) : $$(includes_H_CONFIG) $$(includes_H_PLATFORM)
-- 
-- $$($1_$2_depfile_haskell) : $$($1_$2_HS_SRCS) $$($1_$2_HS_BOOT_SRCS) $$$$($1_$2_HC_MK_DEPEND_DEP) | $$$$(dir $$$$@)/.
--     $$(call removeFiles,$$@.tmp)
-- ifneq "$$($1_$2_HS_SRCS)" ""
--     "$$($1_$2_HC_MK_DEPEND)" -M \
--         $$($1_$2_$$(firstword $$($1_$2_WAYS))_MOST_DIR_HC_OPTS) \
--         $$($1_$2_MKDEPENDHS_FLAGS) \
--         $$($1_$2_HS_SRCS)
-- endif
--     echo "$1_$2_depfile_haskell_EXISTS = YES" >> $$@.tmp
-- ifneq "$$($1_$2_SLASH_MODS)" ""
--     for dir in $$(sort $$(foreach mod,$$($1_$2_SLASH_MODS),$1/$2/build/$$(dir $$(mod)))); do \
--         if test ! -d $$$$dir; then mkdir -p $$$$dir; fi \
--     done
-- endif

-- "inplace/bin/ghc-stage1.exe" -M -static  -H32m -O    -this-package-key deeps_FT5iVCELxOr62eHY0nbvnU -hide-all-packages -i -ilibraries/deepseq/. -ilibraries/deepseq/dist-install/build -ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/dist-install/build -Ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/.    -optP-include -optPlibraries/deepseq/dist-install/build/autogen/cabal_macros.h -package-key array_3w0nMK0JfaFJPpLFn2yWAJ -package-key base_469rOtLAqwTGFEOGWxSUiQ -package-key ghcpr_FgrV6cgh2JHBlbcx1OSlwt -Wall -XHaskell2010 -O2  -no-user-package-db -rtsopts      -odir libraries/deepseq/dist-install/build -hidir libraries/deepseq/dist-install/build -stubdir libraries/deepseq/dist-install/build -dep-makefile libraries/deepseq/dist-install/build/.depend-v-p.haskell.tmp -dep-suffix "" -dep-suffix "p_" -include-pkg-deps  libraries/deepseq/./Control/DeepSeq.hs

-- $1_$2_$3_MOST_HC_OPTS = \
--  $$(WAY_$3_HC_OPTS) \
--  $$(CONF_HC_OPTS) \
--  $$(SRC_HC_OPTS) \
--  $$($1_HC_OPTS) \
--  $$($1_$2_HC_PKGCONF) \
--  $$(if $$($1_$2_PROG),, \
--         $$(if $$($1_PACKAGE),$$($4_THIS_PACKAGE_KEY) $$($1_$2_PACKAGE_KEY))) \
--  $$(if $$($1_PACKAGE),-hide-all-packages) \
--  -i $$(if $$($1_$2_HS_SRC_DIRS),$$(foreach dir,$$($1_$2_HS_SRC_DIRS),-i$1/$$(dir)),-i$1) \
--  -i$1/$2/build -i$1/$2/build/autogen \
--  -I$1/$2/build -I$1/$2/build/autogen \
--  $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
--  $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
--  $$(foreach inc,$$($1_$2_INCLUDE),-\#include "$$(inc)") \
--  $$(foreach opt,$$($1_$2_CPP_OPTS),-optP$$(opt)) \
--  $$(if $$($1_PACKAGE),-optP-include -optP$1/$2/build/autogen/cabal_macros.h) \
--  $$($1_$2_$4_DEP_OPTS) \
--  $$($1_$2_HC_OPTS) \
--  $$(CONF_HC_OPTS_STAGE$4) \
--  $$($1_$2_MORE_HC_OPTS) \
--  $$($1_$2_EXTRA_HC_OPTS) \
--  $$($1_$2_$3_HC_OPTS) \
--  $$($$(basename $$(subst ./,,$$<))_HC_OPTS) \
--  $$(SRC_HC_WARNING_OPTS) \
--  $$(EXTRA_HC_OPTS)

-- TODO: make sure SrcDirs ($1_$2_HS_SRC_DIRS) is not empty ('.' by default)
-- TODO: add $1_HC_OPTS
-- TODO: check that the package is not a program ($1_$2_PROG == "")
-- TODO: handle empty $1_PACKAGE (can it be empty?)
-- TODO: $1_$2_INCLUDE appears to be not set. Safe to skip?
-- Option CONF_HC_OPTS is skipped
buildPackageDeps :: Package -> TodoItem -> Rules ()
buildPackageDeps pkg @ (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
    in
    (buildDir </> "build" </> name <.> "m") %> \out -> do
        let pkgData = buildDir </> "package-data.mk"
        usePackageKey <- SupportsPackageKey || stage /= Stage0 -- TODO: check reasoning (distdir-way-opts)
        [mods, srcDirs, includeDirs, deps, depKeys] <-
            mapM ((fmap words) . (packagaDataOption pkgData))
            [Modules, SrcDirs, IncludeDirs, Deps, DepKeys]
        srcs <- getDirectoryFiles "" $ do
            dir       <- srcDirs
            modPath   <- map (replaceEq '.' pathSeparator) mods
            extension <- ["hs", "lhs"]
            return $ path </> dir </> modPath <.> extension
        packageKey <- packagaDataOption pkgData PackageKey
        run (Ghc stage) $ mconcat
            [ arg ["-M"]
            , wayHcOpts vanilla -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
            , splitArgs $ argOption SrcHcOpts
            , when (stage == Stage0) $ arg ["-package-db libraries/bootstrapping.conf"]
            , arg [if usePackageKey then "-this-package-key" else "-package-name"]
            , arg [packageKey] -- TODO: check reasoning ($$($4_THIS_PACKAGE_KEY) $$($1_$2_PACKAGE_KEY))
            , arg ["-hide-all-packages"]
            , arg ["-i"] -- resets the search path to nothing; TODO: check if really needed
            , arg $ map (\d -> "-i" ++ path </> d) srcDirs
            , arg $ do
                prefix <- ["-i", "-I"] -- 'import' and '#include' search paths
                suffix <- ["build", "build/autogen"]
                return $ prefix ++ buildDir </> suffix
            , arg $ map (\d -> "-I" ++ path </> d) $ filter isRelative includeDirs
            , arg $ map (\d -> "-I" ++          d) $ filter isAbsolute includeDirs
            , arg ["-optP-include"]
            , arg ["-optP" ++ buildDir </> "build/autogen/cabal_macros.h"]
            , if usePackageKey 
              then arg $ concatMap (\d -> ["-package-key", d]) depKeys
              else arg $ concatMap (\d -> ["-package"    , d]) deps
            , arg ["-dep-makefile", out, "-dep-suffix", "", "-include-pkg-deps"]
            , arg $ map normalise srcs
            ]

-- $1_$2_MKDEPENDHS_FLAGS = -dep-makefile $$($1_$2_depfile_haskell).tmp $$(foreach way,$$($1_$2_WAYS),-dep-suffix "$$(-- patsubst %o,%,$$($$(way)_osuf))")
-- $1_$2_MKDEPENDHS_FLAGS += -include-pkg-deps


buildPackage :: Package -> TodoItem -> Rules ()
buildPackage pkg todoItem = do
    buildPackageData pkg todoItem
    buildPackageDeps pkg todoItem

packageRules :: Rules ()
packageRules = do

    want ["libraries/deepseq/dist-install/build/deepseq.m"]
    forM_ packages $ \pkg -> do
        forM_ (pkgTodo pkg) $ \todoItem -> do
            buildPackage pkg todoItem

-- TODO:
-- $1_$2_$3_MOST_HC_OPTS = [ "$$(WAY_$3_HC_OPTS) $$(CONF_HC_OPTS) $$(SRC_HC_OPTS) $$($1_HC_OPTS) $$($1_$2_HC_PKGCONF) $$(if $$($1_$2_PROG),, $$(if $$($1_PACKAGE),$$($4_THIS_PACKAGE_KEY) $$($1_$2_PACKAGE_KEY))) $$(if $$($1_PACKAGE),-hide-all-packages) -i $$(if $$($1_$2_HS_SRC_DIRS),$$(foreach dir,$$($1_$2_HS_SRC_DIRS),-i$1/$$(dir)),-i$1) -i$1/$2/build -i$1/$2/build/autogen -I$1/$2/build -I$1/$2/build/autogen $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) $$(foreach inc,$$($1_$2_INCLUDE),-\ " |  ]
-- $1_$2_$3_MOST_DIR_HC_OPTS = [ "$$($1_$2_$3_MOST_HC_OPTS) -odir $1/$2/build -hidir $1/$2/build -stubdir $1/$2/build " |  ]
-- $$(if $$(findstring YES,$$($1_$2_SplitObjs)),$$(if $$(findstring dyn,$3),,-split-objs),)
-- $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),$$(if $$(findstring v,$3),-dynamic-too)) " |  ]
--  [ "-dll-split " ++ pkgPath pkg </> dist </> dll-split | not $ "$$($1_$2_dll0_MODULES)" == "", "$$(HostOS_CPP)" == "mingw32", "$3" == "dyn" ]    -- rules/distdir-way-opts.mk
ghcOpts :: Package -> Stage -> Way -> Action [String]
ghcOpts pkg stage way = do
    return $ ["-hisuf " ++ hisuf way]
        ++   ["-osuf "  ++ osuf  way]
        ++   ["-hcsuf " ++ hcsuf way]

--buildPackage :: Package -> Stage -> Action ()
--buildPackage pkg stage = do
--    hsFiles <- getDirectoryFiles path ["//*.hs", "//*.lhs"]
--    let ohi = \way -> map (-<.> osuf way) hsFiles ++ map (-<.> hisuf way) hsFiles
--    need . concatMap ohi $ ways pkg stage


-- TODO: implement bootPkgConstraints oracle
--BOOT_PKG_CONSTRAINTS := \
--    $(foreach d,$(PACKAGES_STAGE0),\
--        $(foreach p,$(basename $(notdir $(wildcard libraries/$d/*.cabal))),\
--            --constraint "$p == $(shell grep -i "^Version:" libraries/$d/$p.cabal | sed "s/[^0-9.]//g")"))