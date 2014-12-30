{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Oracles (
    module Control.Monad,
    module Development.Shake.Rule,
    module Prelude,
    Builder (..), Flag (..), Option (..),
    path, with, run, argPath,
    option, argOption,
    Condition, test, when, unless, not, (&&), (||),
    packagaDataOption, PackageDataKey (..),
    oracleRules
    ) where

import Development.Shake.Rule
import Development.Shake.Config
import Development.Shake.Classes
import Control.Monad hiding (when, unless)
import qualified Data.HashMap.Strict as M
import qualified Prelude
import Prelude hiding (not, (&&), (||))
import Base
import Util
import Config

data Builder = Ar | Ld | Gcc | Alex | Happy | HsColour | GhcCabal | GhcPkg Stage | Ghc Stage

path :: Builder -> Action FilePath
path builder = do
    let key = case builder of
            Ar            -> "ar"
            Ld            -> "ld"
            Gcc           -> "gcc"
            Alex          -> "alex"
            Happy         -> "happy"
            HsColour      -> "hscolour"
            GhcCabal      -> "ghc-cabal"
            Ghc Stage0    -> "system-ghc"     -- Ghc Stage0 is the bootstrapping compiler 
            Ghc Stage1    -> "ghc-stage1"     -- Ghc StageN, N > 0, is the one built on stage (N - 1)
            Ghc Stage2    -> "ghc-stage2"
            Ghc Stage3    -> "ghc-stage3"
            GhcPkg Stage0 -> "system-ghc-pkg" -- GhcPkg Stage0 is the bootstrapping GhcPkg 
            GhcPkg _      -> "ghc-pkg"        -- GhcPkg StageN, N > 0, is the one built on stage 0 (TODO: need only Stage1?)
    cfgPath <- askConfigWithDefault key $
        error $ "\nCannot find path to '"
        ++ key
        ++ "' in configuration files."
    let cfgPathExe = if cfgPath /= "" then cfgPath -<.> exe else ""
    windows <- test WindowsHost
    if (windows && "/" `isPrefixOf` cfgPathExe)
    then do
        root <- option Root
        return $ root ++ (drop 1 $ cfgPathExe)
    else
        return cfgPathExe

argPath :: Builder -> Args
argPath builder = do
    path <- path builder
    arg [path]

-- When LaxDeps flag is set (by adding 'lax-dependencies = YES' to user.config),
-- dependencies on the GHC executable are turned into order-only dependencies to
-- avoid needless recompilation when making changes to GHC's sources. In certain
-- situations this can lead to build failures, in which case you should reset
-- the flag (at least temporarily).
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    target  <- path ghc
    laxDeps <- test LaxDeps
    if laxDeps then orderOnly [target] else need [target]

needBuilder builder = do 
    target <- path builder
    need [target]

-- Action 'with Gcc' returns an argument '--with-gcc=/path/to/gcc' and needs the builder 
with :: Builder -> Args
with builder = do 
    let prefix = case builder of 
            Ar       -> "--with-ar="
            Ld       -> "--with-ld="
            Gcc      -> "--with-gcc="
            Ghc _    -> "--with-ghc="
            Alex     -> "--with-alex="
            Happy    -> "--with-happy="
            GhcPkg _ -> "--with-ghc-pkg="
            HsColour -> "--with-hscolour="
    suffix <- path builder
    needBuilder builder
    return [prefix ++ suffix]

run :: Builder -> Args -> Action ()
run builder args = do
    needBuilder builder
    exe   <- path builder
    args' <- args
    cmd [exe :: FilePath] args'

data Option = TargetOS | TargetArch | TargetPlatformFull
            | ConfCcArgs Stage | ConfGccLinkerArgs Stage | ConfLdLinkerArgs Stage | ConfCppArgs Stage
            | IconvIncludeDirs | IconvLibDirs | GmpIncludeDirs | GmpLibDirs
            | HostOsCpp | Root

option :: Option -> Action String
option Root = do
    windows <- test WindowsHost
    if (windows)
    then do
        Stdout out <- quietly $ cmd ["cygpath", "-m", "/"]
        return $ dropWhileEnd isSpace out
    else
        return "/"

option opt = askConfig $ case opt of 
    TargetOS                -> "target-os"
    TargetArch              -> "target-arch"
    TargetPlatformFull      -> "target-platform-full"
    ConfCcArgs        stage -> "conf-cc-args-stage-"         ++ (show . fromEnum) stage
    ConfCppArgs       stage -> "conf-cpp-args-stage-"        ++ (show . fromEnum) stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-stage-" ++ (show . fromEnum) stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-stage-"  ++ (show . fromEnum) stage
    IconvIncludeDirs        -> "iconv-include-dirs"
    IconvLibDirs            -> "iconv-lib-dirs"
    GmpIncludeDirs          -> "gmp-include-dirs"
    GmpLibDirs              -> "gmp-lib-dirs"
    HostOsCpp               -> "host-os-cpp"

argOption :: Option -> Args
argOption opt = do
    opt' <- option opt
    arg [opt']

data Flag = LaxDeps | DynamicGhcPrograms | GhcWithInterpreter | HsColourSrcs
          | GccIsClang | GccLt46 | CrossCompiling | Validating | PlatformSupportsSharedLibs
          | WindowsHost

test :: Flag -> Action Bool
test GhcWithInterpreter = do
    os   <- option TargetOS
    arch <- option TargetArch
    return $
        os `elem` ["mingw32", "cygwin32", "linux", "solaris2", "freebsd", "dragonfly", "netbsd", "openbsd", "darwin", "kfreebsdgnu"]
        &&
        arch `elem` ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

test PlatformSupportsSharedLibs = do
    platform <- option TargetPlatformFull
    return $ platform `notElem` [ "powerpc-unknown-linux", "x86_64-unknown-mingw32", "i386-unknown-mingw32" ] -- TODO: i386-unknown-solaris2?

test HsColourSrcs = do
    hscolour <- path HsColour
    return $ hscolour /= ""

test WindowsHost = do
    hostOsCpp <- option HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]

test flag = do
    (key, defaultValue) <- return $ case flag of
        LaxDeps            -> ("lax-dependencies"    , False) -- TODO: move flags to a separate file
        DynamicGhcPrograms -> ("dynamic-ghc-programs", False)
        GccIsClang         -> ("gcc-is-clang"        , False)
        GccLt46            -> ("gcc-lt-46"           , False)
        CrossCompiling     -> ("cross-compiling"     , False)
        Validating         -> ("validating"          , False)
    let defaultString = if defaultValue then "YES" else "NO"
    value <- askConfigWithDefault key $
        do putLoud $ "\nFlag '" -- TODO: Give the warning *only once* per key
                ++ key
                ++ "' not set in configuration files. "
                ++ "Proceeding with default value '"
                ++ defaultString
                ++ "'.\n"
           return defaultString
    return $ value == "YES"

type Condition = Action Bool

class ToCondition a where
    toCondition :: a -> Condition

instance ToCondition Condition where
    toCondition = id

instance ToCondition Bool where
    toCondition = return

instance ToCondition Flag where
    toCondition = test

when :: (ToCondition a, Monoid m) => a -> Action m -> Action m
when x args = do
    bool <- toCondition x
    if bool then args else mempty

unless :: (ToCondition a, Monoid m) => a -> Action m -> Action m
unless x args = do
    bool <- toCondition x
    if bool then mempty else args

class Not a where
    type NotResult a
    not :: a -> NotResult a

instance Not Bool where
    type NotResult Bool = Bool
    not = Prelude.not

instance Not Condition where
    type NotResult Condition = Condition
    not x = not <$> (toCondition x)

instance Not Flag where
    type NotResult Flag = Condition
    not x = not (toCondition x)

class AndOr a b where
    type AndOrResult a b
    (&&) :: a -> b -> AndOrResult a b
    (||) :: a -> b -> AndOrResult a b

infixr 3 &&
infixr 2 ||

instance AndOr Bool Bool where
    type AndOrResult Bool Bool = Bool
    (&&) = (Prelude.&&)
    (||) = (Prelude.||)

instance ToCondition a => AndOr Condition a where
    type AndOrResult Condition a = Condition
    x && y = (Prelude.&&) <$> toCondition x <*> toCondition y
    x || y = (Prelude.||) <$> toCondition x <*> toCondition y

instance ToCondition a => AndOr Flag a where
    type AndOrResult Flag a = Condition
    x && y = toCondition x && y
    x || y = toCondition x || y

newtype ConfigKey = ConfigKey String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askConfigWithDefault :: String -> Action String -> Action String
askConfigWithDefault key defaultAction = do
    maybeValue <- askOracle $ ConfigKey key 
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key $
    error $ "\nCannot find key '" ++ key ++ "' in configuration files."

newtype PackageDataPair = PackageDataPair (FilePath, String)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

packagaDataOptionWithDefault :: FilePath -> String -> Action String -> Action String
packagaDataOptionWithDefault file key defaultAction = do
    maybeValue <- askOracle $ PackageDataPair (file, key) 
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

data PackageDataKey = Modules | SrcDirs

packagaDataOption :: FilePath -> PackageDataKey -> Action String
packagaDataOption file key = do
    let keyName = replaceIf isSlash '_' $ takeDirectory file ++ case key of
           Modules -> "_MODULES"
           SrcDirs -> "_HS_SRC_DIRS"
    packagaDataOptionWithDefault file keyName $
        error $ "\nCannot find key '" ++ keyName ++ "' in " ++ file ++ "."

oracleRules :: Rules ()
oracleRules = do
    cfg <- newCache $ \() -> do
        unless (doesFileExist $ cfgPath </> "default.config.in") $ do
            error $ "\nDefault configuration file '"
                ++ (cfgPath </> "default.config.in")
                ++ "' is missing; unwilling to proceed."
            return ()
        need [cfgPath </> "default.config"]
        cfgDefault <- liftIO $ readConfigFile $ cfgPath </> "default.config"
        existsUser <- doesFileExist $ cfgPath </> "user.config"
        cfgUser    <- if existsUser
                      then liftIO $ readConfigFile $ cfgPath </> "user.config"
                      else do
                          putLoud $ "\nUser defined configuration file '"
                              ++ (cfgPath </> "user.config")
                              ++ "' is missing; proceeding with default configuration.\n"
                          return M.empty
        return $ cfgUser `M.union` cfgDefault

    addOracle $ \(ConfigKey key) -> M.lookup key <$> cfg ()

    pkgData <- newCache $ \file -> do
        need [file]
        liftIO $ readConfigFile file

    addOracle $ \(PackageDataPair (file, key)) -> M.lookup key <$> pkgData file
    return ()
