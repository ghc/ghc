module Predicates (
    stage, package, builder, stagedBuilder, file, way,
    stage0, stage1, stage2, notStage, notStage0,
    registerPackage, splitObjects
    ) where

import Base
import Expression
import Oracles.Flag
import Oracles.Setting
import Settings.Default

-- Basic predicates (see Switches.hs for derived predicates)
stage :: Stage -> Predicate
stage s = liftM (s ==) getStage

package :: Package -> Predicate
package p = liftM (p ==) getPackage

-- For unstaged builders, e.g. GhcCabal
builder :: Builder -> Predicate
builder b = liftM (b ==) getBuilder

-- For staged builders, e.g. Ghc Stage
stagedBuilder :: (Stage -> Builder) -> Predicate
stagedBuilder sb = (builder . sb) =<< getStage

file :: FilePattern -> Predicate
file f = liftM (any (f ?==)) getFiles

way :: Way -> Predicate
way w = liftM (w ==) getWay

-- Derived predicates
stage0 :: Predicate
stage0 = stage Stage0

stage1 :: Predicate
stage1 = stage Stage1

stage2 :: Predicate
stage2 = stage Stage2

notStage :: Stage -> Predicate
notStage = notP . stage

notStage0 :: Predicate
notStage0 = notP stage0

-- TODO: Actually, we don't register compiler in some circumstances -- fix.
registerPackage :: Predicate
registerPackage = return True

splitObjects :: Predicate
splitObjects = do
    goodStage <- notStage0 -- We don't split bootstrap (stage 0) packages
    goodPkg   <- notP $ package compiler -- We don't split compiler
    broken    <- lift $ flag SplitObjectsBroken
    ghcUnreg  <- lift $ flag GhcUnregisterised
    goodArch  <- lift $ targetArchs [ "i386", "x86_64", "powerpc", "sparc" ]
    goodOs    <- lift $ targetOss   [ "mingw32", "cygwin32", "linux", "darwin"
                                    , "solaris2", "freebsd", "dragonfly"
                                    , "netbsd", "openbsd" ]
    return $ goodStage && goodPkg && not broken && not ghcUnreg && goodArch && goodOs
