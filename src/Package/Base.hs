{-# LANGUAGE NoImplicitPrelude #-}
module Package.Base (
    module Base,
    module Ways,
    module Util,
    module Oracles,
    Package (..), Settings (..), TodoItem (..),
    defaultSettings, libraryPackage,
    commonCcArgs, commonLdArgs, commonCppArgs, commonCcWarninigArgs,
    bootPkgConstraints,
    packageArgs, includeArgs, srcArgs
    ) where

import Base
import Ways
import Util
import Oracles

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
commonCcWarninigArgs = when Validating $
       when GccIsClang                                     (arg "-Wno-unknown-pragmas")
    <> when (not GccIsClang && not GccLt46)                (arg "-Wno-error=inline")
    <> when (    GccIsClang && not GccLt46 && windowsHost) (arg "-Werror=unused-but-set-variable")

bootPkgConstraints :: Args
bootPkgConstraints = mempty

-- TODO: implement bootPkgConstraints oracle
--BOOT_PKG_CONSTRAINTS := \
--    $(foreach d,$(PACKAGES_STAGE0),\
--        $(foreach p,$(basename $(notdir $(wildcard libraries/$d/*.cabal))),\
--            --constraint "$p == $(shell grep -i "^Version:" libraries/$d/$p.cabal | sed "s/[^0-9.]//g")"))

packageArgs :: Stage -> FilePath -> Args
packageArgs stage pkgData = do
    usePackageKey <- SupportsPackageKey || stage /= Stage0
    arg ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
        <> when (stage == Stage0) (arg "-package-db libraries/bootstrapping.conf")
        <> keyArgs usePackageKey
  where
    keyArgs True  = prefixArgs "-this-package-key" (PackageKey pkgData) <>
                    prefixArgs "-package-key"      (DepKeys    pkgData)
    keyArgs False = prefixArgs "-package-name"     (PackageKey pkgData) <>
                    prefixArgs "-package"          (Deps       pkgData)

includeArgs :: ShowArgs a => String -> FilePath -> a -> Args
includeArgs prefix path as = map includePath <$> arg as
  where
    includePath dir | isRelative dir = prefix ++ path </> dir
                    | isAbsolute dir = prefix         </> dir

srcArgs :: FilePath -> FilePath -> Args
srcArgs path pkgData = do
    mods <- map (replaceEq '.' pathSeparator) <$> arg (Modules pkgData)
    dirs <- arg (SrcDirs pkgData)
    srcs <- getDirectoryFiles ""
        [path </> dir </> mPath <.> ext | dir <- dirs, mPath <- mods, ext <- ["hs", "lhs"]]
    arg (map normaliseEx srcs)
