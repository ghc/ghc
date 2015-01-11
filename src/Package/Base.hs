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
    pathArgs, packageArgs, includeArgs, srcArgs
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
defaultSettings stage =
    Settings mempty mempty mempty mempty mempty True (defaultWays stage)

-- Stage is the stage of the GHC that we use to build the package
-- FilePath is the directory to put the build results
-- Settings may be different for different combinations of Stage & FilePath
type TodoItem = (Stage, FilePath, Settings)

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName :: String,    -- For example: "deepseq"
         pkgPath :: FilePath,  -- "libraries/deepseq"
         pkgTodo :: [TodoItem] -- [(Stage1, "dist-install", defaultSettings)]
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

-- TODO: simplify
commonCcWarninigArgs :: Args
commonCcWarninigArgs = when Validating $
       GccIsClang <?> arg "-Wno-unknown-pragmas" 
    <> (not GccIsClang && not GccLt46) <?> arg "-Wno-error=inline" 
    <> (GccIsClang && not GccLt46 && windowsHost) <?>
       arg "-Werror=unused-but-set-variable"

bootPkgConstraints :: Args
bootPkgConstraints = mempty

-- TODO: implement bootPkgConstraints oracle
-- BOOT_PKG_CONSTRAINTS := \
-- $(foreach d,$(PACKAGES_STAGE0),\
--  $(foreach p,$(basename $(notdir $(wildcard libraries/$d/*.cabal))),\
--   --constraint "$p == $(shell grep -i "^Version:" libraries/$d/$p.cabal |
--     sed "s/[^0-9.]//g")"))

pathArgs :: ShowArgs a => String -> FilePath -> a -> Args
pathArgs key path as = map (\a -> key ++ normaliseEx (path </> a)) <$> arg as

packageArgs :: Stage -> FilePath -> Args
packageArgs stage pkgData = do
    usePackageKey <- SupportsPackageKey || stage /= Stage0
    arg ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
        <> (stage == Stage0) <?> arg "-package-db libraries/bootstrapping.conf"
        <> keyArgs usePackageKey
  where
    keyArgs True  = productArgs "-this-package-key" (PackageKey pkgData) <>
                    productArgs "-package-key"      (DepKeys    pkgData)
    keyArgs False = productArgs "-package-name"     (PackageKey pkgData) <>
                    productArgs "-package"          (Deps       pkgData)

includeArgs :: FilePath -> FilePath -> Args
includeArgs path dist = 
    let pkgData  = path </> dist </> "package-data.mk"
        buildDir = path </> dist </> "build"
    in arg "-i"
    <> pathArgs "-i" path     (SrcDirs pkgData)
    <> concatArgs ["-i", "-I"] [buildDir, buildDir </> "autogen"]
    <> pathArgs "-I" path     (IncludeDirs pkgData)
    <> arg "-optP-include" -- TODO: Shall we also add -cpp?
    <> concatArgs "-optP" (buildDir </> "autogen/cabal_macros.h")

srcArgs :: FilePath -> FilePath -> Args
srcArgs path pkgData = do
    mods <- arg (Modules pkgData)
    dirs <- arg (SrcDirs pkgData)
    srcs <- getDirectoryFiles "" $ do
        dir       <- dirs
        modPath   <- map (replaceEq '.' pathSeparator) mods
        extension <- ["hs", "lhs"]
        return $ path </> dir </> modPath <.> extension
    arg (map normaliseEx srcs)
