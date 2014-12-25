{-# LANGUAGE NoImplicitPrelude #-}
module Package (
    packageRules
    ) where

import Base
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
        , when (GhcWithInterpreter && not DynamicGhcPrograms && vanilla `elem` ways) $
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
buildPackageData pkg @ (Package name path todo) (stage, dist, settings) =
        ((path </> dist) </>) <$>
        [ "package-data.mk",
          "haddock-prologue.txt",
          "inplace-pkg-config",
          "setup-config",
          "build" </> "autogen" </> "cabal_macros.h"
        ] &%> \_ -> do
            need [path </> name <.> "cabal"]
            when (doesFileExist $ path </> "configure.ac") $ need [path </> "configure"]
            run GhcCabal cabalArgs
            when (registerPackage settings) $ run (GhcPkg stage) ghcPkgArgs
              where
                cabalArgs, ghcPkgArgs :: Args
                cabalArgs = mconcat $
                    [ arg ["configure", path, dist]
                    -- this is a positional argument, hence:
                    -- * if it is empty, we need to emit one empty string argument
                    -- * if there are many, we must collapse them into one string argument
                    , joinArgsWithSpaces $ customDllArgs settings 
                    , with $ Ghc stage -- TODO: used to be stage01 (using max Stage1 GHC)
                    , with $ GhcPkg stage             

                    , customConfArgs settings
                    , libraryArgs =<< ways settings                

                    , when HsColourSrcs $ with HsColour
                    , configureArgs stage settings

                    , when (stage == Stage0) $ bootPkgConstraints
                    , with Gcc
                    , when (stage /= Stage0) $ with Ld
                    
                    , with Ar
                    , with Alex
                    , with Happy
                    ] -- TODO: reorder with's

                ghcPkgArgs = mconcat $
                    [ arg ["update", "--force"]
                    , when (stage == Stage0) $ arg ["--package-db=libraries/bootstrapping.conf"]
                    , arg [path </> dist </> "inplace-pkg-config"]
                    ]

buildPackage :: Package -> TodoItem -> Rules ()
buildPackage pkg todoItem = do
    buildPackageData pkg todoItem

packageRules :: Rules ()
packageRules = do

    want ["libraries/deepseq/dist-install/package-data.mk"]
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