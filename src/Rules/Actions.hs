module Rules.Actions (build, buildWithResources) where

import Expression
import Oracles.ArgsHash
import Settings
import Settings.Args
import Settings.Builders.Ar
import qualified Target

-- Build a given target using an appropriate builder and acquiring necessary
-- resources. Force a rebuilt if the argument list has changed since the last
-- built (that is, track changes in the build system).
buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = do
    let builder = Target.builder target
    needBuilder laxDependencies builder
    path    <- builderPath builder
    argList <- interpret target getArgs
    -- The line below forces the rule to be rerun if the args hash has changed
    checkArgsHash target
    withResources rs $ do
        putBuild $ "/--------\n| Running " ++ show builder ++ " with arguments:"
        mapM_ (putBuild . ("|   " ++)) $ interestingInfo builder argList
        putBuild $ "\\--------"
        quietly $ case builder of
            Ar -> arCmd path argList

            HsCpp -> do
                let file = head $ Target.files target  -- TODO: ugly
                Stdout output <- cmd [path] argList
                writeFileChanged file output

            GenPrimopCode -> do
                let src  = head $ Target.sources target -- TODO: ugly
                    file = head $ Target.files   target
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            _  -> cmd [path] argList

-- Most targets are built without explicitly acquiring resources
build :: Target -> Action ()
build = buildWithResources []

interestingInfo :: Builder -> [String] -> [String]
interestingInfo builder ss = case builder of
    Alex     -> prefixAndSuffix 0 3 ss
    Ar       -> prefixAndSuffix 2 1 ss
    Gcc _    -> prefixAndSuffix 0 4 ss
    GccM _   -> prefixAndSuffix 0 1 ss
    Ghc _    -> prefixAndSuffix 0 4 ss
    GhcCabal -> prefixAndSuffix 3 0 ss
    GhcM _   -> prefixAndSuffix 1 1 ss
    GhcPkg _ -> prefixAndSuffix 3 0 ss
    Haddock  -> prefixAndSuffix 1 0 ss
    Happy    -> prefixAndSuffix 0 3 ss
    Hsc2Hs   -> prefixAndSuffix 0 3 ss
    HsCpp    -> prefixAndSuffix 0 1 ss
    Ld       -> prefixAndSuffix 4 0 ss
    _        -> ss
  where
    prefixAndSuffix n m list =
        let len = length list in
        if len <= n + m + 1
        then list
        else take n list
             ++ ["... skipping " ++ show (len - n - m) ++ " arguments ..."]
             ++ drop (len - m) list
