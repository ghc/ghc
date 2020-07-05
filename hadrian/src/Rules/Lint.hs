module Rules.Lint
  ( lintRules
  ) where

import Base
import Settings.Builders.Common
import System.Directory (findExecutable)

lintRules :: Rules ()
lintRules = "lint" ~> lint

lint :: Action ()
lint = do
  isHlintPresent <- isJust <$> (liftIO $ findExecutable "hlint")
  if isHlintPresent
  then do
    putBuild "| Running the linter…"
    lintBase
    putSuccess "| Done."
  else
    putFailure "| Please make sure you have the `hlint` executable in your $PATH"

lintBase :: Action ()
lintBase = do
  topDir   <- topDirectory
  buildDir <- buildRoot
  let stage1Lib    = topDir </> buildDir </> "stage1/lib"
  let machDeps     = topDir </> "includes/MachDeps.h"
  let hsBaseConfig = topDir </> buildDir </> "stage1/libraries/base/build/include/HsBaseConfig.h"
  let ghcautoconf  = stage1Lib </> "ghcautoconf.h"
  let ghcplatform  = stage1Lib </> "ghcplatform.h"
  need [ghcautoconf, ghcplatform, machDeps, hsBaseConfig]
  let include0  = topDir </> "includes"
  let include1  = topDir </> "libraries/base/include"
  let include2  = stage1Lib
  let include3  = topDir </> buildDir </> "stage1/libraries/base/build/include"
  let hlintYaml = topDir </> "libraries/base/.hlint.yaml"
  hostArch <- (<> "_HOST_ARCH") <$> setting HostArch
  let cmdLine = "hlint -j --cpp-define " <> hostArch <> " --cpp-include=" <> include0 <>
                " --cpp-include=" <> include1 <>
                " --cpp-include=" <> include2 <>
                " --cpp-include=" <> include3 <>
                " -h " <> hlintYaml <> " libraries/base"
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine
