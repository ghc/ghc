module Rules.Actions (
    build, buildWhen, run, verboseRun,
    ) where

import Util
import Builder
import Expression
import Settings.Args
import Oracles.ArgsHash
import Development.Shake

-- Build a given target using an appropriate builder. Force a rebuilt if the
-- argument list has changed since the last built (that is, track changes in
-- the build system).
build :: FullTarget -> Action ()
build target = do
    argList <- interpret target args
    putColoured Green (show target)
    putColoured Green (show argList)
    -- The line below forces the rule to be rerun if the args hash has changed
    argsHash <- askArgsHash target
    run (getBuilder target) argList

buildWhen :: Predicate -> FullTarget -> Action ()
buildWhen predicate target = do
    bool <- interpretExpr target predicate
    when bool $ build target

-- Run the builder with a given collection of arguments
verboseRun :: Builder -> [String] -> Action ()
verboseRun builder args = do
    needBuilder builder
    path <- builderPath builder
    cmd [path] args

-- Run the builder with a given collection of arguments printing out a
-- terse commentary with only 'interesting' info for the builder.
run :: Builder -> [String] -> Action ()
run builder args = do
    putColoured White $ "/--------\n" ++
        "| Running " ++ show builder ++ " with arguments:"
    mapM_ (putColoured White . ("|   " ++)) $ interestingInfo builder args
    putColoured White $ "\\--------"
    quietly $ verboseRun builder args

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
