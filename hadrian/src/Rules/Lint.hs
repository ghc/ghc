module Rules.Lint
  ( lintRules
  ) where

import Base
import Settings.Builders.Common
import System.Directory (findExecutable)

lintRules :: Rules ()
lintRules = do
  "lint:base" ~> lint base
  "lint:compiler" ~> lint compiler

lint :: Action () -> Action ()
lint lintAction = do
  isHlintPresent <- isJust <$> (liftIO $ findExecutable "hlint")
  if isHlintPresent
  then do
    putBuild "| Running the linter…"
    lintAction
    putSuccess "| Done."
  else
    putFailure "| Please make sure you have the `hlint` executable in your $PATH"

base :: Action ()
base = do
  buildDir <- buildRoot
  let stage1Lib    = buildDir </> "stage1/lib"
  let machDeps     = "includes/MachDeps.h"
  let ghcautoconf  = stage1Lib </> "ghcautoconf.h"
  let ghcplatform  = stage1Lib </> "ghcplatform.h"
  need ["stage1:lib:base", ghcautoconf, ghcplatform, machDeps]
  let include0  = "includes"
  let include1  = "libraries/base/include"
  let include2  = stage1Lib
  let include3  = buildDir </> "stage1/libraries/base/build/include"
  let hlintYaml = "libraries/base/.hlint.yaml"
  hostArch <- (<> "_HOST_ARCH") <$> setting HostArch
  let cmdLine = "hlint -j --cpp-define " <> hostArch <> " --cpp-include=" <> include0 <>
                " --cpp-include=" <> include1 <>
                " --cpp-include=" <> include2 <>
                " --cpp-include=" <> include3 <>
                " -h " <> hlintYaml <> " libraries/base"
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine

compiler :: Action ()
compiler = do
  buildDir <- buildRoot
  let stage1Lib      = buildDir </> "stage1/lib"
  let stage1Compiler = buildDir </> "stage1/compiler/build"
  let machDeps       = "includes/MachDeps.h"
  let hsVersions     = "compiler/HsVersions.h"
  let compilerDir    = "compiler"
  let ghcautoconf    = stage1Lib </> "ghcautoconf.h"
  let ghcplatform    = stage1Lib </> "ghcplatform.h"
  need $ mconcat [[ghcautoconf, ghcplatform], hsIncls stage1Compiler, [machDeps, hsVersions]]
  let include0  = "includes"
  let include1  = stage1Lib
  let hlintYaml = "compiler/.hlint.yaml"
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

hsIncls :: FilePath -> [FilePath]
hsIncls path = [ path </> "primop-vector-tycons.hs-incl"
               , path </> "primop-vector-tys.hs-incl"
               , path </> "primop-vector-tys-exports.hs-incl"
               , path </> "primop-code-size.hs-incl"
               , path </> "primop-vector-uniques.hs-incl"
               , path </> "primop-data-decl.hs-incl"
               , path </> "primop-tag.hs-incl"
               , path </> "primop-list.hs-incl"
               , path </> "primop-strictness.hs-incl"
               , path </> "primop-fixity.hs-incl"
               , path </> "primop-docs.hs-incl"
               , path </> "primop-primop-info.hs-incl"
               , path </> "primop-out-of-line.hs-incl"
               , path </> "primop-has-side-effects.hs-incl"
               , path </> "primop-can-fail.hs-incl"
               , path </> "primop-commutable.hs-incl"
               ]
