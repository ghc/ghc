{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveGeneric, ConstraintKinds #-}

module Oracles (
    module Control.Monad,
    module Development.Shake.Rule,
    module Prelude,
    Builder (..), Flag (..), Option (..),
    path, with, run, argPath,
    option, argOption,
    test, when, unless, not, (&&), (||),
    oracleRules
    ) where

import Development.Shake.Config
import Development.Shake.Rule
import Development.Shake.Classes
import Control.Monad hiding (when, unless)
import qualified System.Directory as System
import qualified Data.HashMap.Strict as M
import qualified Prelude
import Prelude hiding (not, (&&), (||))
import Data.Char
import Base
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
        return $ root ++ cfgPathExe
    else
        return cfgPathExe

argPath :: Builder -> Args
argPath builder = do
    path <- path builder
    arg [path]

-- Explain!
-- TODO: document change in behaviour (LaxDeps)
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    target  <- path ghc
    laxDeps <- test LaxDeps -- TODO: get rid of test?
    if laxDeps then orderOnly [target] else need [target]

needBuilder builder = do 
    target <- path builder
    need [target]

-- 'with Gcc' generates --with-gcc=/usr/bin/gcc and needs it
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
        Stdout out <- cmd ["cygpath", "-m", "/"]   
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

data Flag = LaxDeps | Stage1Only | DynamicGhcPrograms | GhcWithInterpreter | HsColourSrcs
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
        Stage1Only         -> ("stage-1-only"        , False) -- TODO: target only
        DynamicGhcPrograms -> ("dynamic-ghc-programs", False)
        GccIsClang         -> ("gcc-is-clang"        , False)
        GccLt46            -> ("gcc-lt-46"           , False)
        CrossCompiling     -> ("cross-compiling"     , False)
        Validating         -> ("validating"          , False)
    let defaultString = if defaultValue then "YES" else "NO"
    value <- askConfigWithDefault key $
        do putLoud $ "\nFlag '"
                ++ key
                ++ "' not set in configuration files. "
                ++ "Proceeding with default value '"
                ++ defaultString
                ++ "'.\n"
           return defaultString
    return $ value == "YES"

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
    maybeValue <- askOracle $ ConfigKey $ key 
    case maybeValue of
        Just value -> return value
        Nothing    -> do
                        result <- defaultAction
                        return result

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key $ error $ "\nCannot find key '"
                                         ++ key
                                         ++ "' in configuration files."

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
    addOracle $ \(ConfigKey x) -> M.lookup x <$> cfg ()
    return ()
