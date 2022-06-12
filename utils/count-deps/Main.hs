{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import GHC.Driver.Env
import GHC.Data.IOEnv
import GHC.Unit.Finder
import GHC.Unit.Env
import GHC.Unit.Home
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.State
import GHC.Utils.Outputable
import GHC.Driver.Session
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Types.SourceFile
import GHC.Iface.Load
import GHC.Tc.Types
import GHC
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import GHC.Unit.Module.Deps
import Data.Map.Strict qualified as Map
#if __GLASGOW_HASKELL >= 905
import Data.Set qualified as Set
#endif
import Data.List (isPrefixOf)

-- Example invocation:
--  inplace/bin/count-deps `inplace/bin/ghc-stage2 --print-libdir` "GHC.Parser"
main :: IO ()
main = do
  args <- getArgs
  case args of
    [libdir, modName, "--dot"] -> printDeps libdir modName True
    [libdir, modName] -> printDeps libdir modName False
    _ -> fail "usage: count-deps libdir module [--dot]"

dotSpec :: String -> Map.Map String [String] -> String
dotSpec name g =
 "digraph \"" ++ name ++ "\" {\n" ++
    Map.foldlWithKey' f "" g ++ "}\n"
  where
    f acc k ns = acc ++ concat ["  " ++ show k ++ " -> " ++ show n ++ ";\n" | n <- ns]

printDeps :: String -> String -> Bool -> IO ()
printDeps libdir modName dot = do
  modGraph <-
    Map.map (map modNodeLabel) .
      Map.mapKeys modNodeLabel <$> calcDeps modName libdir
  if not dot then
    do
      let modules = Map.keys modGraph
      putStrLn $ "Found " ++ modName ++ " module dependencies"
      forM_ modules putStrLn
  else
    -- * Copy the digraph output to a file ('deps.dot' say)
    -- * To render it, use a command along the lines of
    --   'tred deps.dot > deps-tred.dot && dot -Tpdf -o deps.pdf deps-tred.dot'
    putStr $ dotSpec modName modGraph

modNodeLabel :: ModuleNameWithIsBoot -> String
modNodeLabel (GWIB mod NotBoot) = moduleNameString mod
modNodeLabel (GWIB mod IsBoot) = moduleNameString mod ++ " (hs-boot)"

type DepMap = Map.Map ModuleNameWithIsBoot Deps
type Deps = [ModuleNameWithIsBoot]

calcDeps :: String -> FilePath -> IO (Map.Map ModuleNameWithIsBoot [ModuleNameWithIsBoot])
calcDeps modName libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        logger <- getLogger
        (df, _, _) <- parseDynamicFlags logger df [noLoc "-v", noLoc "-i=../../compiler"]
        setSessionDynFlags df
        let tid = TargetFile modName Nothing
        setTargets [Target tid False (UnitId "main") Nothing]
        mod_graph <- depanal [] False
        let nodes = mgModSummaries' mod_graph
        liftIO $  print (length nodes)
        let edges = map nodeToEdges nodes
        return $ Map.fromList edges
  where
    nodeToEdges (ModuleNode _ ms) = (msModName ms, modImports ms)
    nodeToEdges _ = error "unknown mod graph node"

    msModName ms = case ms_hsc_src ms of
      HsSrcFile -> GWIB (ms_mod_name ms) NotBoot
      HsBootFile -> GWIB (ms_mod_name ms) IsBoot
      HsigFile -> error "HsigFile found"

    modImports :: ModSummary -> [ModuleNameWithIsBoot]
    modImports ms = map mkBoot (modNames (ms_srcimps ms)) ++
                    map mkNonBoot (modNames (ms_textual_imps ms))

    mkBoot mod = GWIB mod IsBoot
    mkNonBoot mod = GWIB mod NotBoot

    modNames = map (unLoc . snd)
