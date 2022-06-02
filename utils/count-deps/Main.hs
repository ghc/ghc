{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Prelude

import GHC.Driver.Env
import GHC.Unit.Module
import GHC.Driver.Session
import GHC.Driver.Main
import GHC
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import GHC.Unit.Module.Deps
import Data.Map.Strict qualified as Map
#if __GLASGOW_HASKELL >= 905
import Data.Set qualified as Set
#endif

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
    Map.map (map moduleNameString) .
      Map.mapKeys moduleNameString <$> calcDeps modName libdir
  if not dot then
    do
      let modules = Map.keys modGraph
          num = length modules
      putStrLn $ "Found " ++ show num ++ " " ++ modName ++ " module dependencies"
      forM_ modules putStrLn
  else
    -- * Copy the digraph output to a file ('deps.dot' say)
    -- * To render it, use a command along the lines of
    --   'tred deps.dot > deps-tred.dot && dot -Tpdf -o deps.pdf deps-tred.dot'
    putStr $ dotSpec modName modGraph

type DepMap = Map.Map ModuleNameWithIsBoot Deps
type Deps = [ModuleNameWithIsBoot]

calcDeps :: String -> FilePath -> IO (Map.Map ModuleName [ModuleName])
calcDeps modName libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        logger <- getLogger
        (df, _, _) <- parseDynamicFlags logger df [noLoc "-package=ghc"]
        setSessionDynFlags df
        env <- getSession
        loop env Map.empty [GWIB (mkModuleName modName) NotBoot]
  where
    -- Source imports are only guaranteed to show up in the 'mi_deps'
    -- of modules that import them directly and don’t propagate
    -- transitively so we loop.
    loop :: HscEnv -> DepMap -> Deps -> Ghc DepMap
    loop env modules (m : ms) =
      if m `Map.member` modules
        then loop env modules ms
        else do
          mi <- liftIO $ hscGetModuleInterface env (mkModule m)
          let deps = modDeps mi
          modules <- return $ Map.insert m [] modules
          loop env (Map.insert m deps modules) $ ms ++ filter (not . (`Map.member` modules)) deps
    loop _ modules [] = return modules

    mkModule :: ModuleName -> Module
    mkModule = Module (stringToUnit "ghc")

    modDeps :: ModIface -> [ModuleNameWithIsBoot]
#if __GLASGOW_HASKELL < 906
    modDeps mi = map dep_mods (mi_deps mi)
#else
    modDeps mi = map snd $ Set.toList $ dep_direct_mods (mi_deps mi)
#endif
