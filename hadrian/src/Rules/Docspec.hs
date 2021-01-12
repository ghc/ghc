module Rules.Docspec
  ( docspecRules
  ) where

import Base
import Settings.Builders.Common
import System.Directory (findExecutable)

docspecRules :: Rules ()
docspecRules = do
  "docspec:base" ~> docspec base

docspec :: Action () -> Action ()
docspec lintAction = do
  isExecutablePresent <- isJust <$> liftIO (findExecutable "cabal-docspec")
  if isExecutablePresent
  then do
    putBuild "| Running cabal-docspec…"
    lintAction
    putSuccess "| Done."
  else
    putFailure "| Please make sure you have the `cabal-docspec` executable in your $PATH"

base :: Action ()
base = do
  topDir   <- topDirectory
  buildDir <- buildRoot
  let stage1Lib    = topDir </> buildDir </> "stage1/lib"
  let stage1GHC    = topDir </> buildDir </> "stage1/bin/ghc"
  let cabalFile    = topDir </> "libraries/base/base.cabal"
  let machDeps     = topDir </> "includes/MachDeps.h"
  let hsBaseConfig = topDir </> buildDir </> "stage1/libraries/base/build/include/HsBaseConfig.h"
  let ghcautoconf  = stage1Lib </> "ghcautoconf.h"
  let ghcplatform  = stage1Lib </> "ghcplatform.h"
  need [ghcautoconf, ghcplatform, machDeps, hsBaseConfig]
  let include0  = topDir </> "includes"
  let include1  = topDir </> "libraries/base/include"
  let include2  = stage1Lib
  let include3  = topDir </> buildDir </> "stage1/libraries/base/build/include"
  let cmdLine = "cabal-docspec -w " <> stage1GHC <>
                " --no-cabal-plan" <>
                " --strip-comments" <>
                " --timeout 2" <>
                " --ghci-rtsopts=-K500K" <>
                " --extra-package=mtl --extra-package=deepseq --extra-package=bytestring" <>
                " -XNoImplicitPrelude" <>
                " -I " <> include0 <>
                " -I " <> include1 <>
                " -I " <> include2 <>
                " -I " <> include3 <> " " <>
                cabalFile
  putBuild $ "| " <> cmdLine
  cmd_ cmdLine
