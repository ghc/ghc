module CountDeps (printDeps) where

import GHC.Driver.Env
import GHC.Unit.Module
import GHC.Driver.Session
import GHC.Driver.Main
import GHC
import GHC.Utils.Misc
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.Exit
import GHC.Types.Unique.Set
import GHC.Unit.Module.Deps

printDeps :: String -> IO ()
printDeps modName = do
  [libdir] <- getArgs
  modules <- calcDeps modName libdir
  let num = sizeUniqSet modules
  putStrLn $ "Found " ++ show num ++ " " ++ modName ++ " module dependencies"
  forM_ (map moduleNameString $ nonDetEltsUniqSet modules) putStrLn

calcDeps :: String -> FilePath -> IO (UniqSet ModuleName)
calcDeps modName libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        logger <- getLogger
        (df, _, _) <- parseDynamicFlags logger df [noLoc "-package=ghc"]
        setSessionDynFlags df
        env <- getSession
        loop env emptyUniqSet [mkModuleName modName]
  where
    -- Source imports are only guaranteed to show up in the 'mi_deps'
    -- of modules that import them directly and don’t propagate
    -- transitively so we loop.
    loop :: HscEnv -> UniqSet ModuleName -> [ModuleName] -> Ghc (UniqSet ModuleName)
    loop env modules (m : ms) =
      if m `elementOfUniqSet` modules
        then loop env modules ms
        else do
          modules <- return (addOneToUniqSet modules m)
          mi <- liftIO $ hscGetModuleInterface env (mkModule m)
          loop env modules (ms ++ filter (not . (`elementOfUniqSet` modules)) (modDeps mi))
    loop _ modules [] = return modules

    mkModule :: ModuleName -> Module
    mkModule = Module (stringToUnit "ghc")

    modDeps :: ModIface -> [ModuleName]
    modDeps mi = map gwib_mod $ dep_direct_mods (mi_deps mi)
