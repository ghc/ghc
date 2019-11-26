-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Builder
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- A typical build system invokes several build tools, or /builders/, such as
-- compilers, linkers, etc., some of which may be built by the build system
-- itself. This module defines the 'Builder' type class and a few associated
-- functions that can be used to invoke builders.
-----------------------------------------------------------------------------
module Hadrian.Builder (
    Builder (..), BuildInfo (..), needBuilder, runBuilder,
    runBuilderWithCmdOptions, build, buildWithResources, buildWithCmdOptions,
    getBuilderPath, builderEnvironment, askWithResources
    ) where

import Data.List
import Development.Shake

import Hadrian.Expression hiding (inputs, outputs)
import Hadrian.Oracles.ArgsHash
import Hadrian.Target
import Hadrian.Utilities

-- | This data structure captures all information relevant to invoking a builder.
data BuildInfo = BuildInfo {
    -- | Command line arguments.
    buildArgs :: [String],
    -- | Input files.
    buildInputs :: [FilePath],
    -- | Output files.
    buildOutputs :: [FilePath],
    -- | Options to be passed to Shake's 'cmd' function.
    buildOptions :: [CmdOption],
    -- | Resources to be acquired.
    buildResources :: [(Resource, Int)] }

class ShakeValue b => Builder b where
    -- | The path to a builder.
    builderPath :: b -> Action FilePath

    -- | Ask the builder for information.
    -- E.g. ask @ghc-pkg@ for package dependencies
    -- capture the @stdout@ result and return it.
    askBuilderWith :: b -> BuildInfo -> Action String

    -- | Runtime dependencies of a builder. For example, on Windows GHC requires
    -- the utility @touchy.exe@ to be available on a specific path.
    runtimeDependencies :: b -> Action [FilePath]
    runtimeDependencies _ = return []

    -- | Run a builder with a given 'BuildInfo'. Also see 'runBuilder'.
    runBuilderWith :: b -> BuildInfo -> Action ()
    runBuilderWith builder buildInfo = do
        let args = buildArgs buildInfo
        needBuilder builder
        path <- builderPath builder
        let msg = if null args then "" else " (" ++ intercalate ", " args ++ ")"
        putBuild $ "| Run " ++ show builder ++ msg
        quietly $ cmd (buildOptions buildInfo) [path] args

-- | Make sure a builder and its runtime dependencies are up-to-date.
needBuilder :: Builder b => b -> Action ()
needBuilder builder = do
    path <- builderPath builder
    deps <- runtimeDependencies builder
    need (path : deps)

-- | Run a builder with a specified list of command line arguments, reading a
-- list of input files and writing a list of output files. A lightweight version
-- of 'runBuilderWith'.
runBuilder :: Builder b => b -> [String] -> [FilePath] -> [FilePath] -> Action ()
runBuilder = runBuilderWithCmdOptions []

-- | Like 'runBuilder' but passes given options to Shake's 'cmd'.
runBuilderWithCmdOptions :: Builder b => [CmdOption] -> b -> [String] -> [FilePath] -> [FilePath] -> Action ()
runBuilderWithCmdOptions opts builder args inputs outputs =
    runBuilderWith builder $ BuildInfo { buildArgs      = args
                                       , buildInputs    = inputs
                                       , buildOutputs   = outputs
                                       , buildOptions   = opts
                                       , buildResources = [] }

-- | Build a 'Target' using the list of command line arguments computed from a
-- given 'Args' expression. Force a rebuild if the argument list has changed
-- since the last build.
build :: (Builder b, ShakeValue c) => Target c b -> Args c b -> Action ()
build = buildWith [] []

-- | Like 'build' but acquires necessary resources.
buildWithResources :: (Builder b, ShakeValue c) => [(Resource, Int)] -> Target c b -> Args c b -> Action ()
buildWithResources rs = buildWith rs []

askWithResources :: (Builder b, ShakeValue c) => [(Resource, Int)] -> Target c b -> Args c b -> Action String
askWithResources rs = askWith rs []

-- | Like 'build' but passes given options to Shake's 'cmd'.
buildWithCmdOptions :: (Builder b, ShakeValue c) => [CmdOption] -> Target c b -> Args c b -> Action ()
buildWithCmdOptions = buildWith []

doWith :: (Builder b, ShakeValue c)
       => (b -> BuildInfo -> Action a)
       -> (Target c b -> Action ())
       -> [(Resource, Int)] -> [CmdOption] -> Target c b -> Args c b -> Action a
doWith f info rs opts target args = do
    needBuilder (builder target)
    argList <- interpret target args
    trackArgsHash target -- Rerun the rule if the hash of argList has changed.
    info target
    verbose <- interpret target verboseCommand
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    quietlyUnlessVerbose $ f (builder target) $
        BuildInfo { buildArgs      = argList
                  , buildInputs    = inputs target
                  , buildOutputs   = outputs target
                  , buildOptions   = opts
                  , buildResources = rs }

buildWith :: (Builder b, ShakeValue c) => [(Resource, Int)] -> [CmdOption] -> Target c b -> Args c b -> Action ()
buildWith = doWith runBuilderWith runInfo

askWith :: (Builder b, ShakeValue c) => [(Resource, Int)] -> [CmdOption] -> Target c b -> Args c b -> Action String
askWith = doWith askBuilderWith askInfo

-- | Print out information about the command being executed.
runInfo :: Show b => Target c b -> Action ()
runInfo t = putProgressInfo =<< renderAction
    ("Run " ++ show (builder t)) -- TODO: Bring back contextInfo.
    (digest $ inputs  t)
    (digest $ outputs t)
  where
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

askInfo :: Show b => Target c b -> Action ()
askInfo t = putProgressInfo =<< renderActionNoOutput
    ("Run " ++ show (builder t)) -- TODO: Bring back contextInfo.
    (digest $ inputs  t)
  where
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

-- | Get the path to the current builder.
getBuilderPath :: Builder b => b -> Expr c b FilePath
getBuilderPath = expr . builderPath

-- | Write a builder path into a given environment variable.
builderEnvironment :: Builder b => String -> b -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path
