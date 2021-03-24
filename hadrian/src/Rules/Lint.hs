module Rules.Lint
  ( lintRules
  ) where

import Base
import Context.Path (buildPath)
import qualified Packages as P
import Settings.Builders.Common (Setting (..), setting, topDirectory, vanillaContext)
import System.Directory (findExecutable)


lintRules :: Rules ()
lintRules = do
  "lint:base" ~> lint base
  "lint:compiler" ~> lint compiler

lint :: Action () -> Action ()
lint lintAction = do
  isHlintPresent <- isJust <$> liftIO (findExecutable "hlint")
  if isHlintPresent
  then do
    putBuild "| Running the linter…"
    lintAction
    putSuccess "| Done."
  else
    putFailure "| Please make sure you have the `hlint` executable in your $PATH"

base :: Action ()
base = do
  topDir   <- topDirectory
  let context = vanillaContext Stage1 P.base
  stage1Lib <- stageLibPath Stage1
  let basePath = topDir </> pkgPath P.base
  let topIncludes = topDir </> "includes"
  includeDeps' <- includesDependencies Stage1
  buildPath' <- buildPath context
  let buildIncludes = topDir </> buildPath' </> "include"
  let includeDeps = fmap (topDir </>) includeDeps'
  need includeDeps
  let baseIncludes  = topDir </> basePath </> "include"
  let hlintYaml = topDir </> "libraries/base/.hlint.yaml"
  hostArch <- (<> "_HOST_ARCH") <$> setting HostArch
  let cmdLine = "hlint -j --cpp-define " <> hostArch <> " --cpp-include=" <> topIncludes <>
                " --cpp-include=" <> buildIncludes <>
                " --cpp-include=" <> stage1Lib <>
                " --cpp-include=" <> baseIncludes <>
                " -h " <> hlintYaml <> " libraries/base"
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine

compiler :: Action ()
compiler = do
  topDir   <- topDirectory
  buildDir <- buildRoot
  let stage1Lib      = topDir </> buildDir </> "stage1/lib"
  let stage1Compiler = topDir </> buildDir </> "stage1/compiler/build"
  let machDeps       = topDir </> "includes/MachDeps.h"
  let hsVersions     = topDir </> "compiler/HsVersions.h"
  let compilerDir    = topDir </> "compiler"
  let ghcautoconf    = stage1Lib </> "ghcautoconf.h"
  let ghcplatform    = stage1Lib </> "ghcplatform.h"
  let pmv            = stage1Compiler </> "primop-vector-uniques.hs-incl"
  need [ghcautoconf, ghcplatform, machDeps, hsVersions, pmv]
  let include0  = topDir </> "includes"
  let include1  = stage1Lib
  let hlintYaml = topDir </> "compiler/.hlint.yaml"
  hostArch <- (<> "_HOST_ARCH") <$> setting HostArch
  let cmdLine = "hlint -j --cpp-define " <> hostArch <>
                " --cpp-include=" <> include0 <>
                " --cpp-include=" <> include1 <>
                " --cpp-include=" <> compilerDir <>
                " --cpp-include=" <> ghcplatform <>
                " --cpp-include=" <> stage1Compiler <>
                " -h " <> hlintYaml <> " compiler"
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine

