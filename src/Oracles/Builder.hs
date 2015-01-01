{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Oracles.Builder (
    Builder (..),
    path, with, run, argPath,
    hsColourSrcs
    ) where

import Data.Char
import Base
import Oracles.Base
import Oracles.Flag
import Oracles.Option

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
    windows <- windowsHost
    if (windows && "/" `isPrefixOf` cfgPathExe)
    then do
        Stdout out <- quietly $ cmd ["cygpath", "-m", "/"]
        return $ dropWhileEnd isSpace out ++ drop 1 cfgPathExe
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

hsColourSrcs :: Condition
hsColourSrcs = do
    hscolour <- path HsColour
    return $ hscolour /= ""
