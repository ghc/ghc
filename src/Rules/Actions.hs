module Rules.Actions (
    build, buildWithResources, run, verboseRun
    ) where

import Util
import Builder
import Expression
import qualified Target
import Settings.Args
import Settings.Util
import Oracles.ArgsHash
import Development.Shake

-- Build a given target using an appropriate builder and acquiring necessary
-- resources. Force a rebuilt if the argument list has changed since the last
-- built (that is, track changes in the build system).
buildWithResources :: [(Resource, Int)] -> FullTarget -> Action ()
buildWithResources rs target = do
    need $ Target.dependencies target
    argList <- interpret target args
    -- The line below forces the rule to be rerun if the args hash has changed
    argsHash <- askArgsHash target
    run rs (Target.builder target) argList

-- Most targets are built without explicitly acquiring resources
build :: FullTarget -> Action ()
build = buildWithResources []

-- Run the builder with a given collection of arguments
verboseRun :: [(Resource, Int)] -> Builder -> [String] -> Action ()
verboseRun rs builder args = do
    needBuilder builder
    path <- builderPath builder
    withResources rs $ cmd [path] args

-- Run the builder with a given collection of arguments printing out a
-- terse commentary with only 'interesting' info for the builder.
run :: [(Resource, Int)] -> Builder -> [String] -> Action ()
run rs builder args = do
    putColoured White $ "/--------\n" ++
        "| Running " ++ show builder ++ " with arguments:"
    mapM_ (putColoured White . ("|   " ++)) $ interestingInfo builder args
    putColoured White $ "\\--------"
    quietly $ verboseRun rs builder args

interestingInfo :: Builder -> [String] -> [String]
interestingInfo builder ss = case builder of
    Ar       -> prefixAndSuffix 2 1 ss
    Ld       -> prefixAndSuffix 4 0 ss
    Gcc _    -> prefixAndSuffix 0 4 ss
    GccM _   -> prefixAndSuffix 0 1 ss
    Ghc _    -> prefixAndSuffix 0 4 ss
    --GhcM _   -> prefixAndSuffix 1 1 ss
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
