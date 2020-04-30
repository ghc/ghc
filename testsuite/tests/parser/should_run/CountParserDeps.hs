module Main(main) where

-- Calculate the number of module dependencies of 'Parser.' If that
-- number exceeds a threshold, that indicates that the dependencies
-- have significantly gone up via the commit under test (and the test
-- is deemed to fail). In that case, this most likely means a cycle
-- has arisen that pulls in modules for Core generation. The
-- motivation for not allowing that to happen is so that the
-- 'ghc-lib-parser' package subset of the GHC API can continue to be
-- provided with as small a number of modules as possible for when the
-- need exists to produce ASTs and nothing more.

import GHC.Driver.Types
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

main :: IO ()
main = do
  [libdir] <- getArgs
  modules <- parserDeps libdir
  let num = sizeUniqSet modules
--  print num
--  print (map moduleNameString $ nonDetEltsUniqSet modules)
  unless (num < 195) $ exitWith (ExitFailure num)

parserDeps :: FilePath -> IO (UniqSet ModuleName)
parserDeps libdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        (df, _, _) <- parseDynamicFlags df [noLoc "-package=ghc"]
        setSessionDynFlags df
        env <- getSession
        loop env emptyUniqSet [mkModuleName "GHC.Parser"]
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
    modDeps mi = map gwib_mod $ dep_mods (mi_deps mi)
