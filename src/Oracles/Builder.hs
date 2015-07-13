{-# LANGUAGE NoImplicitPrelude #-}

module Oracles.Builder (
    Builder (..), builderKey, withBuilderKey,
    with, run, verboseRun, specified
    ) where

import Data.Char
import Base
import Util
import Oracles.Base
import Oracles.Flag
import Oracles.Option

-- A Builder is an external command invoked in separate process using Shake.cmd
--
-- Ghc Stage0 is the bootstrapping compiler
-- Ghc StageN, N > 0, is the one built on stage (N - 1)
-- GhcPkg Stage0 is the bootstrapping GhcPkg
-- GhcPkg StageN, N > 0, is the one built on stage 0 (TODO: need only Stage1?)
-- TODO: add Cpp and Haddock builders
-- TODO: rename Gcc to Cc?
data Builder = Ar
             | Ld
             | Alex
             | Happy
             | HsColour
             | GhcCabal
             | Gcc Stage
             | Ghc Stage
             | GhcPkg Stage
             deriving (Show, Eq)

builderKey :: Builder -> String
builderKey builder = case builder of
    Ar            -> "ar"
    Ld            -> "ld"
    Alex          -> "alex"
    Happy         -> "happy"
    HsColour      -> "hscolour"
    GhcCabal      -> "ghc-cabal"
    Ghc Stage0    -> "system-ghc"
    Ghc Stage1    -> "ghc-stage1"
    Ghc Stage2    -> "ghc-stage2"
    Ghc Stage3    -> "ghc-stage3"
    Gcc Stage0    -> "system-gcc"
    Gcc _         -> "gcc"
    GhcPkg Stage0 -> "system-ghc-pkg"
    GhcPkg _      -> "ghc-pkg"

instance ShowArg Builder where
    showArg builder = toStandard <$> do
        cfgPath <- askConfigWithDefault (builderKey builder) $
            redError $ "\nCannot find path to '" ++ (builderKey builder)
                     ++ "' in configuration files."
        let cfgPathExe = if null cfgPath then "" else cfgPath -<.> exe
        windows <- windowsHost
        -- Note, below is different from FilePath.isAbsolute:
        if (windows && "/" `isPrefixOf` cfgPathExe)
        then do
            Stdout out <- quietly $ cmd ["cygpath", "-m", "/"]
            return $ dropWhileEnd isSpace out ++ drop 1 cfgPathExe
        else
            return cfgPathExe

-- When LaxDeps flag is set ('lax-dependencies = YES' in user.config),
-- dependencies on the GHC executable are turned into order-only dependencies
-- to avoid needless recompilation when making changes to GHC's sources. In
-- certain situations this can lead to build failures, in which case you
-- should reset the flag (at least temporarily).

-- Make sure the builder exists on the given path and rebuild it if out of date
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    exe     <- showArg ghc
    laxDeps <- test LaxDeps
    if laxDeps then orderOnly [exe] else need [exe]

needBuilder builder = do
    exe <- showArg builder
    need [exe]

-- Action 'with Gcc' returns '--with-gcc=/path/to/gcc' and needs Gcc
with :: Builder -> Action String
with builder = do
    exe <- showArg builder
    needBuilder builder
    return $ withBuilderKey builder ++ exe

withBuilderKey :: Builder -> String
withBuilderKey builder = case builder of
    Ar       -> "--with-ar="
    Ld       -> "--with-ld="
    Gcc _    -> "--with-gcc="
    Ghc _    -> "--with-ghc="
    Alex     -> "--with-alex="
    Happy    -> "--with-happy="
    GhcPkg _ -> "--with-ghc-pkg="
    HsColour -> "--with-hscolour="
    _        -> error "withBuilderKey: not supported builder"

-- Run the builder with a given collection of arguments
verboseRun :: ShowArgs a => Builder -> a -> Action ()
verboseRun builder as = do
    needBuilder builder
    exe  <- showArg builder
    args <- showArgs as
    cmd [exe] args

-- Run the builder with a given collection of arguments printing out a
-- terse commentary with only 'interesting' info for the builder.
run :: ShowArgs a => Builder -> a -> Action ()
run builder as = do
    args <- showArgs as
    putColoured White $ "/--------\n" ++
        "| Running " ++ show builder ++ " with arguments:"
    mapM_ (putColoured White . ("|   " ++)) $
        interestingInfo builder args
    putColoured White $ "\\--------"
    quietly $ verboseRun builder as

interestingInfo :: Builder -> [String] -> [String]
interestingInfo builder ss = case builder of
    Ar       -> prefixAndSuffix 2 1 ss
    Ld       -> prefixAndSuffix 4 0 ss
    Gcc _    -> if head ss == "-MM"
                then prefixAndSuffix 1 1 ss
                else prefixAndSuffix 0 4 ss
    Ghc _    -> if head ss == "-M"
                then prefixAndSuffix 1 1 ss
                else prefixAndSuffix 0 4 ss
    GhcPkg _ -> prefixAndSuffix 3 0 ss
    GhcCabal -> prefixAndSuffix 3 0 ss
    _        -> ss
  where
    prefixAndSuffix n m ss =
        if length ss <= n + m + 1
        then ss
        else take n ss
             ++ ["... skipping "
             ++ show (length ss - n - m)
             ++ " arguments ..."]
             ++ drop (length ss - m) ss

-- Check if the builder is specified in config files
specified :: Builder -> Condition
specified = fmap (not . null) . showArg
