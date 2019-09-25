module Main(main) where

import HscTypes
import Module
import DynFlags
import HscMain
import GHC
import Util
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.Exit

main :: IO ()
main = do
  -- The ghc lib dir to avoid depending on ghc-paths.
  [libdir] <- getArgs -- e.g. /usr/local/lib/ghc-8.8.1
  modules <- parserDeps libdir
  let num = length modules
  -- If the number of modules exceeds expectations, it means this
  -- commit being tested has *significantly* changed the module
  -- dependencies of 'Parser'.
  unless (num < 160) $ exitWith (ExitFailure num)

parserDeps :: FilePath -> IO [String]
parserDeps libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        (df, _, _) <- parseDynamicFlags df [noLoc "-package=ghc"]
        setSessionDynFlags df
        env <- getSession
        nubSort <$> loop env [] ["Parser"]
  where
    loop :: HscEnv -> [String] -> [String] -> Ghc [String]
    loop env modules (m : ms) =
      if m `elem` modules
        then loop env modules ms
        else do
          modules <- return $ m : modules
          mi <- liftIO $ hscGetModuleInterface env (mkModule m)
          loop env modules (ms ++ filter (`notElem` modules) (modDeps mi))
    loop _ modules [] = return modules

    mkModule :: String -> Module
    mkModule m = Module (stringToUnitId "ghc") (mkModuleName m)
    modDeps :: ModIface -> [String]
    modDeps mi = nub (map (moduleNameString . fst) $ dep_mods (mi_deps mi))
