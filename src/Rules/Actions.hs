module Rules.Actions (
    build, buildWithResources
    ) where

import Base
import Util
import Builder
import Expression
import qualified Target
import Settings.Args
import Settings.Util
import Oracles.ArgsHash

-- Build a given target using an appropriate builder and acquiring necessary
-- resources. Force a rebuilt if the argument list has changed since the last
-- built (that is, track changes in the build system).
buildWithResources :: [(Resource, Int)] -> FullTarget -> Action ()
buildWithResources rs target = do
    let builder = Target.builder target
        deps    = Target.dependencies target
    needBuilder builder
    need deps
    path    <- builderPath builder
    argList <- interpret target getArgs
    -- The line below forces the rule to be rerun if the args hash has changed
    argsHash <- askArgsHash target
    withResources rs $ do
        putBuild $ "/--------\n" ++ "| Running "
                 ++ show builder ++ " with arguments:"
        mapM_ (putBuild . ("|   " ++)) $ interestingInfo builder argList
        putBuild $ "\\--------"
        quietly $ cmd [path] argList

-- Most targets are built without explicitly acquiring resources
build :: FullTarget -> Action ()
build = buildWithResources []

interestingInfo :: Builder -> [String] -> [String]
interestingInfo builder ss = case builder of
    Ar       -> prefixAndSuffix 2 1 ss
    Ld       -> prefixAndSuffix 4 0 ss
    Gcc _    -> prefixAndSuffix 0 4 ss
    GccM _   -> prefixAndSuffix 0 1 ss
    Ghc _    -> prefixAndSuffix 0 4 ss
    GhcM _   -> prefixAndSuffix 1 1 ss
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
