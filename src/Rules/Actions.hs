{-# LANGUAGE RecordWildCards #-}
module Rules.Actions (
    build, buildWithResources, copyFile, createDirectory, removeDirectory, moveDirectory,
    fixFile, runConfigure, runMake, applyPatch, runBuilder, makeExecutable
    ) where

import qualified System.Directory as IO

import Base
import Expression
import Oracles
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
            Ar -> do
                output <- interpret target getOutput
                if "//*.a" ?== output
                then arCmd path argList
                else do
                    input <- interpret target getInput
                    top   <- topDirectory
                    cmd [path] [Cwd output] "x" (top -/- input)

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

copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    putBuild $ renderAction "Copy file" source target
    copyFileChanged source target

createDirectory :: FilePath -> Action ()
createDirectory dir = do
    putBuild $ "| Create directory " ++ dir
    liftIO $ IO.createDirectoryIfMissing True dir

removeDirectory :: FilePath -> Action ()
removeDirectory dir = do
    putBuild $ "| Remove directory " ++ dir
    removeDirectoryIfExists dir

-- Note, the source directory is untracked
moveDirectory :: FilePath -> FilePath -> Action ()
moveDirectory source target = do
    putBuild $ renderAction "Move directory" source target
    liftIO $ IO.renameDirectory source target

-- Transform a given file by applying a function to its contents
fixFile :: FilePath -> (String -> String) -> Action ()
fixFile file f = do
    putBuild $ "| Fix " ++ file
    old <- liftIO $ readFile file
    let new = f old
    length new `seq` liftIO $ writeFile file new

runConfigure :: FilePath -> [CmdOption] -> [String] -> Action ()
runConfigure dir opts args = do
    need [dir -/- "configure"]
    putBuild $ "| Run configure in " ++ dir ++ "..."
    quietly $ cmd Shell (EchoStdout False) [Cwd dir] "bash configure" opts' args
    where
        -- Always configure with bash.
        -- This also injects /bin/bash into `libtool`, instead of /bin/sh
        opts' = opts ++ [AddEnv "CONFIG_SHELL" "/bin/bash"]

runMake :: FilePath -> [String] -> Action ()
runMake dir args = do
    need [dir -/- "Makefile"]
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run make" ++ note ++ " in " ++ dir ++ "..."
    quietly $ cmd Shell (EchoStdout False) makeCommand ["-C", dir] args

applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    need [file]
    needBuilder False Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd Shell (EchoStdout False) [Cwd dir] [path, "-p0 <", patch]

runBuilder :: Builder -> [String] -> Action ()
runBuilder builder args = do
    needBuilder laxDependencies builder
    path <- builderPath builder
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ show builder ++ note
    quietly $ cmd [path] args

makeExecutable :: FilePath -> Action ()
makeExecutable file = do
    putBuild $ "| Make '" ++ file ++ "' executable."
    quietly $ cmd "chmod +x " [file]

-- Print out key information about the command being executed
putInfo :: Target.Target -> Action ()
putInfo Target.Target {..} = putBuild $ renderAction
    ("Run " ++ show builder ++ " (" ++ stageInfo
    ++ "package = " ++ pkgNameString package ++ wayInfo ++ ")")
    (digest inputs)
    (digest outputs)
  where
    stageInfo = if isStaged builder then "" else "stage = " ++ show stage ++ ", "
    wayInfo   = if way == vanilla   then "" else ", way = " ++ show way
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"
