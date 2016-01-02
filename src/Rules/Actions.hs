{-# LANGUAGE RecordWildCards #-}
module Rules.Actions (build, buildWithResources) where

import Base
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
    verbose <- interpret target verboseCommands
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    -- The line below forces the rule to be rerun if the args hash has changed
    checkArgsHash target
    withResources rs $ do
        unless verbose $ putInfo target
        quietlyUnlessVerbose $ case builder of
            Ar -> arCmd path argList

            HsCpp    -> captureStdout target path argList
            GenApply -> captureStdout target path argList

            GenPrimopCode -> do
                src  <- interpret target getInput
                file <- interpret target getOutput
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            _  -> cmd [path] argList

-- Most targets are built without explicitly acquiring resources
build :: Target -> Action ()
build = buildWithResources []

captureStdout :: Target -> FilePath -> [String] -> Action ()
captureStdout target path argList = do
    file <- interpret target getOutput
    Stdout output <- cmd [path] argList
    writeFileChanged file output

-- Print out key information about the command being executed
putInfo :: Target.Target -> Action ()
putInfo (Target.Target {..}) = putBuild $ renderBox $
    [ "Running " ++ show builder
      ++ " (" ++ stageInfo
      ++ "package = " ++ pkgNameString package
      ++ wayInfo ++ "):"
    , "    input: " ++ digest inputs
    , "=> output: " ++ digest outputs ]
  where
    stageInfo = if isStaged builder then "" else "stage = " ++ show stage ++ ", "
    wayInfo   = if way == vanilla   then "" else ", way = " ++ show way
    digest list = case list of
        []  -> "none"
        [x] -> x
        xs  -> head xs ++ " (and " ++ show (length xs - 1) ++ " more)"
