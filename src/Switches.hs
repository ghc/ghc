module Switches (
    stage0, stage1, stage2, notStage, notStage0,
    registerPackage, splitObjects
    ) where

import Stage
import Oracles.Flag
import Oracles.Setting
import Expression

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
    stage       <- asks getStage
    notBroken   <- notP . flag $ SplitObjectsBroken
    notGhcUnreg <- notP . flag $ GhcUnregisterised
    goodArch    <- lift $ targetArchs [ "i386", "x86_64", "powerpc", "sparc" ]
    goodOs      <- lift $ targetOss   [ "mingw32", "cygwin32", "linux"
                                      , "darwin", "solaris2", "freebsd"
                                      , "dragonfly", "netbsd", "openbsd"]
    return $ notBroken && notGhcUnreg && stage == Stage1 && goodArch && goodOs
