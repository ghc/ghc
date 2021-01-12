module Rules.Docspec
  ( docspecRules
  ) where

import System.Directory (findExecutable)

import Base
import Context.Path
import Hadrian.Haskell.Cabal.Type (ContextData (..))
import Hadrian.Oracles.Cabal
import Settings.Builders.Common
import qualified Packages as P

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
  stage1GHC <- fmap (\p -> topDir </> p </> "ghc") (stageBinPath Stage1)
  stage1Lib <- stageLibPath Stage1
  let basePath = topDir </> "libraries/base"
  let cabalFile = basePath </> "base.cabal"
  let topIncludes = topDir </> "includes"
  let machDep = topIncludes </> "MachDeps.h"
  let context = vanillaContext Stage1 P.base
  contextData <- readContextData context
  includeDeps' <- includesDependencies Stage1
  buildPath' <- buildPath context
  let baseIncludesDir = head $ fmap (basePath </>) (includeDirs contextData)
  let buildIncludesPath = topDir </> buildPath' </> "include"
  let includeDeps = fmap (topDir </>) includeDeps'
  let neededIncludes = includeDeps ++ [machDep]
  need neededIncludes

  command_ [] "cabal-docspec" [ "-w", stage1GHC
                              , "--no-cabal-plan"
                              , "--strip-comments"
                              , "--timeout", "2"
                              , "--ghci-rtsopts=-K500K"
                              , "--extra-package=mtl", "--extra-package=deepseq", "--extra-package=bytestring"
                              , "-XNoImplicitPrelude"
                              , "-I", topIncludes
                              , "-I", baseIncludesDir
                              , "-I", buildIncludesPath
                              , "-I", stage1Lib
                              , cabalFile
                              ]
