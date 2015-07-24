module Switches (
    stage0, stage1, stage2, notStage, notStage0,
    registerPackage, splitObjects
    ) where

import Stage
import Expression
import Settings.Util
import Oracles.Flag
import Oracles.Setting

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
    stage    <- getStage -- We don't split bootstrap (stage 0) packages
    broken   <- getFlag SplitObjectsBroken
    ghcUnreg <- getFlag GhcUnregisterised
    goodArch <- lift $ targetArchs [ "i386", "x86_64", "powerpc", "sparc" ]
    goodOs   <- lift $ targetOss   [ "mingw32", "cygwin32", "linux"
                                   , "darwin", "solaris2", "freebsd"
                                   , "dragonfly", "netbsd", "openbsd"]
    return $ not broken && not ghcUnreg && stage == Stage1 && goodArch && goodOs
