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
  isHlintPresent <- isJust <$> liftIO (findExecutable "hlint")
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
      defines' = hostArch : defines
      cmdLine = unwords $
        [ "hlint"
        , "--colour=never"
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
  let stage1RtsInc = buildDir </> "stage1/rts/stage1/build/include"
  let machDeps     = "rts/include/MachDeps.h"
  let ghcautoconf  = stage1RtsInc </> "ghcautoconf.h"
  let ghcplatform  = stage1RtsInc </> "ghcplatform.h"
  -- The libraries' `./configure`s are called here manually because we need to
  -- generate `HsBaseConfig.h` and `ghcautoconf.h`, which are created from
  -- `HsBaseConfig.h.in` and `ghcautoconf.h.autoconf`, respectfully. These
  -- `./configure`s is usually run by Cabal which generates this file but if we
  -- do that then hadrian thinks it needs to build the stage0 compiler before
  -- attempting to configure. Therefore we just run it directly everytime,
  -- which is slower but still faster than building the whole of stage0.
  cmd_ (Cwd "libraries/base/stage1/build") "../../configure"
  cmd_ (Cwd "rts/stage1/build") "../../configure"
  cmd_ (Cwd "rts") "./configure"
  need [ghcautoconf, ghcplatform, machDeps]
  let includeDirs =
        [ "rts/include"
        , "libraries/base/include"
        , stage1RtsInc
        ]
  runHLint includeDirs [] "libraries/base"

compiler :: Action ()
compiler = do
  buildDir <- buildRoot
  let stage1RtsInc   = buildDir </> "stage1/rts/build/include"
  let stage1Compiler = buildDir </> "stage1/compiler/build"
  let machDeps       = "rts/include/MachDeps.h"
  let compilerDir    = "compiler"
  let ghcautoconf    = stage1RtsInc </> "ghcautoconf.h"
  let ghcplatform    = stage1RtsInc </> "ghcplatform.h"
  need $ mconcat [[ghcautoconf, ghcplatform], hsIncls stage1Compiler, [machDeps]]
  let includeDirs =
        [ stage1RtsInc
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
