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

runHLint :: [FilePath] -- ^ include directories
         -> [String]   -- ^ CPP defines
         -> FilePath
         -> Action ()
runHLint includeDirs defines dir = do
  threads <- shakeThreads <$> getShakeOptions
  hostArch <- (<> "_HOST_ARCH") <$> setting HostArch
  let hlintYaml = dir </> ".hlint.yaml"
      defines' = [hostArch] ++ defines
      cmdLine = unwords $
        [ "hlint"
        , "-j" <> show threads
        ] ++
        map ("--cpp-define=" <>) defines' ++
        map ("--cpp-include=" <>) includeDirs ++
        [ "-h" <> hlintYaml
        , dir
        ]
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine

base :: Action ()
base = do
  buildDir <- buildRoot
  let stage1Lib    = buildDir </> "stage1/lib"
  let machDeps     = "includes/MachDeps.h"
  let ghcautoconf  = stage1Lib </> "ghcautoconf.h"
  let ghcplatform  = stage1Lib </> "ghcplatform.h"
  need ["stage1:lib:base", ghcautoconf, ghcplatform, machDeps]
  let includeDirs =
        [ "includes"
        , "libraries/base/include"
        , stage1Lib
        , buildDir </> "stage1/libraries/base/build/include"
        ]
  runHLint includeDirs [] "libraries/base"

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
  let includeDirs =
        [ "includes"
        , stage1Lib
        , compilerDir
        , ghcplatform
        , stage1Compiler
        ]
  runHLint includeDirs [] "compiler"

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
