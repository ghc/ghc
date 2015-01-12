{-# LANGUAGE NoImplicitPrelude #-}

module Oracles.Builder (
    Builder (..),
    with, run, specified
    ) where

import Data.Char
import Base
import Oracles.Base
import Oracles.Flag
import Oracles.Option

-- Ghc Stage0 is the bootstrapping compiler
-- Ghc StageN, N > 0, is the one built on stage (N - 1)
-- GhcPkg Stage0 is the bootstrapping GhcPkg 
-- GhcPkg StageN, N > 0, is the one built on stage 0 (TODO: need only Stage1?)
data Builder = Ar
             | Ld
             | Gcc
             | Alex
             | Happy
             | HsColour
             | GhcCabal
             | Ghc Stage
             | GhcPkg Stage

instance ShowArgs Builder where
    showArgs builder = showArgs $ fmap words $ do
        let key = case builder of
                Ar            -> "ar"
                Ld            -> "ld"
                Gcc           -> "gcc"
                Alex          -> "alex"
                Happy         -> "happy"
                HsColour      -> "hscolour"
                GhcCabal      -> "ghc-cabal"
                Ghc Stage0    -> "system-ghc"
                Ghc Stage1    -> "ghc-stage1"
                Ghc Stage2    -> "ghc-stage2"
                Ghc Stage3    -> "ghc-stage3"
                GhcPkg Stage0 -> "system-ghc-pkg"
                GhcPkg _      -> "ghc-pkg"
        cfgPath <- askConfigWithDefault key $
            error $ "\nCannot find path to '" ++ key
                  ++ "' in configuration files."
        let cfgPathExe = if cfgPath /= "" then cfgPath -<.> exe else ""
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
-- Raise an error if the builder is not uniquely specified in config files
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    [exe]   <- showArgs ghc
    laxDeps <- test LaxDeps
    if laxDeps then orderOnly [exe] else need [exe]

needBuilder builder = do 
    [exe] <- showArgs builder
    need [exe]

-- Action 'with Gcc' returns '--with-gcc=/path/to/gcc' and needs Gcc
-- Raises an error if the builder is not uniquely specified in config files
with :: Builder -> Args
with builder = do 
    let key = case builder of 
            Ar       -> "--with-ar="
            Ld       -> "--with-ld="
            Gcc      -> "--with-gcc="
            Ghc _    -> "--with-ghc="
            Alex     -> "--with-alex="
            Happy    -> "--with-happy="
            GhcPkg _ -> "--with-ghc-pkg="
            HsColour -> "--with-hscolour="
    [exe] <- showArgs builder
    needBuilder builder
    arg $ key ++ normaliseEx exe

-- Run the builder with a given collection of arguments
-- Raises an error if the builder is not uniquely specified in config files
run :: Builder -> Args -> Action ()
run builder args = do
    needBuilder builder
    [exe] <- showArgs builder
    cmd [exe] =<< args

-- Check if the builder is uniquely specified in config files
specified :: Builder -> Condition
specified builder = do
    exes <- showArgs builder
    return $ case exes of
        [_] -> True
        _   -> False
