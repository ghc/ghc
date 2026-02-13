{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.Driver.Session
import GHC.Unit.Module
import GHC.Unit.Module.Deps
import GHC.Unit.State
import System.Environment
import System.Exit

-- The parsed output of `ghc --info`
type CompInfo = [(String, String)]


data TargetPackage = TargetPackage
  { tpName :: String
  , tpUnitIdKey :: CompInfo -> Maybe String
  , tpPackageFlags :: [String]
  }

ghcPackage :: TargetPackage
ghcPackage =
  TargetPackage
    { tpName = "ghc"
    , tpUnitIdKey = \info -> lookup "Project Unit Id" info
    , tpPackageFlags = ["ghc"]
    }

ghcInternalPackage :: TargetPackage
ghcInternalPackage =
  TargetPackage
    { tpName = "ghc-internal"
    , tpUnitIdKey = pure (Just "ghc-internal")
    , tpPackageFlags = ["ghc-internal"]
    }

data Options = Options
  { optLibDir :: FilePath
  , optPackage :: TargetPackage
  , optModules :: [String]
  , optDot :: Bool
  }

usage :: String
usage =
  unlines
    [ "usage: count-deps [--package ghc|ghc-internal] libdir [module ...] [--dot]"
    , "       When --dot is supplied modules may be omitted to dump the whole package graph."
    ]

-- Example invocation:
--  inplace/bin/count-deps `inplace/bin/ghc-stage2 --print-libdir` "GHC.Parser"
main :: IO ()
main = do
  args <- getArgs
  opts <- either fail pure (parseArgs args)
  when (null (optModules opts) && not (optDot opts)) $ do
    putStrLn usage
    exitFailure
  printDeps opts

parseArgs :: [String] -> Either String Options
parseArgs = go ghcPackage False []
  where
    go pkg _dot pos ("--dot" : xs) = go pkg True pos xs
    go _ _ _ ["--package"] = Left "--package flag requires a value"
    go _ dot pos ("--package" : pkgName : xs) =
      selectPackage pkgName >>= \pkg' -> go pkg' dot pos xs
    go pkg dot pos (arg : xs)
      | "--package=" `isPrefixOf` arg =
          selectPackage (drop (length ("--package=" :: String)) arg) >>= \pkg' ->
            go pkg' dot pos xs
      | otherwise = go pkg dot (pos ++ [arg]) xs
    go pkg dot pos [] =
      case pos of
        (libdir : rest) ->
          Right
            Options
              { optLibDir = libdir
              , optPackage = pkg
              , optModules = rest
              , optDot = dot
              }
        _ -> Left usage

    selectPackage name =
      case lowercase name of
        "ghc" -> Right ghcPackage
        "ghc-internal" -> Right ghcInternalPackage
        other -> Left $ "unknown package: " ++ other

    lowercase = map toLower

printDeps :: Options -> IO ()
printDeps opts = do
  let moduleRoots
        | optDot opts && null (optModules opts) = Nothing
        | otherwise = Just (map mkModuleName (optModules opts))

  modGraph <-
    Map.map (map moduleNameString)
      . Map.mapKeys moduleNameString
      <$> calcDeps (optPackage opts) moduleRoots (optLibDir opts)

  if optDot opts
    then
      -- * Copy the digraph output to a file ('deps.dot' say)
      -- * To render it, use a command along the lines of
      --   'tred deps.dot > deps-tred.dot && dot -Tpdf -o deps.pdf deps-tred.dot'
      putStr $ dotSpec (graphLabel opts) modGraph
    else do
      let modules = Map.keys modGraph
          label = case optModules opts of
            [single] -> "Found " ++ single ++ " module dependencies"
            xs -> "Found module dependencies for: " ++ intercalate ", " xs
      putStrLn label
      forM_ modules putStrLn

graphLabel :: Options -> String
graphLabel opts =
  case optModules opts of
    [] -> tpName (optPackage opts)
    xs -> intercalate "+" xs

dotSpec :: String -> Map.Map String [String] -> String
dotSpec name g =
  "digraph \"" ++ name ++ "\" {\n"
    ++ Map.foldlWithKey' f "" g
    ++ "}\n"
  where
    f acc k ns = acc ++ concat ["  " ++ show k ++ " -> " ++ show n ++ ";\n" | n <- ns]

calcDeps :: TargetPackage -> Maybe [ModuleName] -> FilePath -> IO (Map.Map ModuleName [ModuleName])
calcDeps targetPackage requestedModules libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      df <- getSessionDynFlags
      logger <- getLogger
      let pkgFlags = map (\pkgName -> noLoc ("-package=" ++ pkgName)) (tpPackageFlags targetPackage)
      (df, _, _) <- parseDynamicFlags logger df pkgFlags
      setSessionDynFlags df
      case tpUnitIdKey targetPackage (compilerInfo df) of
        Nothing ->
          fail $
            "failed to find "
              ++ tpName targetPackage
              ++ "'s unit-id in the compiler info"
        Just unitIdStr -> do
          env <- getSession
          startingModules <-
            case requestedModules of
              Just modules -> pure modules
              Nothing -> packageModules env unitIdStr
          loop unitIdStr env Map.empty startingModules
  where
    -- Source imports are only guaranteed to show up in the 'mi_deps'
    -- of modules that import them directly and don’t propagate
    -- transitively so we loop.
    loop :: String -> HscEnv -> Map.Map ModuleName [ModuleName] -> [ModuleName] -> Ghc (Map.Map ModuleName [ModuleName])
    loop ghcUnitId env modules (m : ms) =
      if m `Map.member` modules
        then loop ghcUnitId env modules ms
        else do
          mi <- liftIO $ hscGetModuleInterface env (mkModule ghcUnitId m)
          let deps = modDeps mi
              modules' = Map.insert m deps modules
              unseen = filter (not . (`Map.member` modules')) deps
          loop ghcUnitId env modules' (ms ++ unseen)
    loop _ _ modules [] = return modules

    mkModule :: String -> ModuleName -> Module
    mkModule ghcUnitId = Module (stringToUnit ghcUnitId)

    modDeps :: ModIface -> [ModuleName]
    modDeps mi = map (gwib_mod . (\(_, _, mn) -> mn)) $ Set.toList $ dep_direct_mods (mi_deps mi)

    packageModules :: HscEnv -> String -> Ghc [ModuleName]
    packageModules env unitIdStr =
      case lookupUnit (hsc_units env) (stringToUnit unitIdStr) of
        Nothing -> fail $ "failed to lookup package info for " ++ unitIdStr
        Just info ->
          pure [modName | (modName, Nothing) <- unitExposedModules info]
