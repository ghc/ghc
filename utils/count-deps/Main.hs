{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import GHC.Driver.Env
import GHC.Unit.Module
import GHC.Driver.Session
import GHC.Driver.Main
import GHC
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import GHC.Unit.Module.Deps
import GHC.Unit.State
import GHC.Unit.Info
import GHC.Data.FastString
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- Example invocation:
--  inplace/bin/count-deps `inplace/bin/ghc-stage2 --print-libdir` ghc "GHC.Parser"
main :: IO ()
main = do
  args <- getArgs
  case args of
    [libdir, packageName, modName, "--dot"] -> printDeps libdir packageName modName True
    [libdir, packageName, modName] -> printDeps libdir packageName modName False
    _ -> fail "usage: count-deps libdir package module [--dot]"

dotSpec :: String -> Map.Map String [String] -> String
dotSpec name g =
 "digraph \"" ++ name ++ "\" {\n" ++
    Map.foldlWithKey' f "" g ++ "}\n"
  where
    f acc k ns = acc ++ concat ["  " ++ show k ++ " -> " ++ show n ++ ";\n" | n <- ns]

printDeps :: String -> String -> String -> Bool -> IO ()
printDeps libdir packageName modName dot = do
  modGraph <-
    Map.map (map moduleNameString) .
      Map.mapKeys moduleNameString <$> calcDeps (Just modName) packageName libdir
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

calcDeps :: Maybe String -> String -> FilePath -> IO (Map.Map ModuleName [ModuleName])
calcDeps mmodName packageName libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        logger <- getLogger
        (df, _, _) <- parseDynamicFlags logger df [noLoc ("-package=" ++ packageName)]
        setSessionDynFlags df
        env <- getSession
        case lookupPackageName (hsc_units env) (PackageName $ mkFastString packageName)  of
          Nothing -> fail $ "failed to find " ++ packageName ++ "'s unit-id in the compiler info"
          Just unitId -> do
            let initialModules =
                  case mmodName of
                    Just modName -> [mkModuleName modName]
                    -- We are looking at the whole package so get all modules
                    Nothing
                      | Just ghcUnitInfo <- lookupUnitId (hsc_units env) unitId
                      -> map fst (unitExposedModules ghcUnitInfo) ++ unitHiddenModules ghcUnitInfo
            loop unitId env Map.empty initialModules
  where
    -- Source imports are only guaranteed to show up in the 'mi_deps'
    -- of modules that import them directly and don’t propagate
    -- transitively so we loop.
    loop :: UnitId -> HscEnv -> Map.Map ModuleName [ModuleName] -> [ModuleName] -> Ghc (Map.Map ModuleName [ModuleName])
    loop unitId env modules (m : ms) =
      if m `Map.member` modules
        then loop unitId env modules ms
        else do
          mi <- liftIO $ hscGetModuleInterface env (mkModule unitId m)
          let deps = modDeps mi
          modules <- return $ Map.insert m [] modules
          loop unitId env (Map.insert m deps modules) $ ms ++ filter (not . (`Map.member` modules)) deps
    loop _ _ modules [] = return modules

    mkModule :: UnitId -> ModuleName -> Module
    mkModule unitId = Module (RealUnit $ Definite unitId)

    modDeps :: ModIface -> [ModuleName]
    modDeps mi = map (gwib_mod . (\(_, _, mn) -> mn)) $ Set.toList $ dep_direct_mods (mi_deps mi)
