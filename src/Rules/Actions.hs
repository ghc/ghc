module Rules.Actions (
    build, buildWithResources
    ) where

import Base
import Util
import Builder
import Expression
import qualified Target
import Oracles.Setting
import Oracles.ArgsHash
import Settings.Args
import Settings.User
import Settings.Builders.Ar

-- Build a given target using an appropriate builder and acquiring necessary
-- resources. Force a rebuilt if the argument list has changed since the last
-- built (that is, track changes in the build system).
buildWithResources :: [(Resource, Int)] -> FullTarget -> Action ()
buildWithResources rs target = do
    let builder = Target.builder target
    needBuilder laxDependencies builder
    path    <- builderPath builder
    argList <- interpret target getArgs
    -- The line below forces the rule to be rerun if the args hash has changed
    when trackBuildSystem $ checkArgsHash target
    withResources rs $ do
        putBuild $ "/--------\n" ++ "| Running "
                 ++ show builder ++ " with arguments:"
        mapM_ (putBuild . ("|   " ++)) $ interestingInfo builder argList
        putBuild $ "\\--------"
        quietly $ if builder /= Ar
            then cmd [path] argList
            else do -- Split argument list into chunks as otherwise Ar chokes up
                maxChunk <- cmdLineLengthLimit
                let persistentArgs = take arPersistentArgsCount argList
                    remainingArgs  = drop arPersistentArgsCount argList
                forM_ (chunksOfSize maxChunk remainingArgs) $ \argsChunk ->
                    unit . cmd [path] $ persistentArgs ++ argsChunk

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
    prefixAndSuffix n m list =
        if length list <= n + m + 1
        then list
        else take n list
             ++ ["... skipping "
             ++ show (length list - n - m)
             ++ " arguments ..."]
             ++ drop (length list - m) list
