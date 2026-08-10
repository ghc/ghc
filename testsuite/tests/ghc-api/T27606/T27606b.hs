{-# LANGUAGE LambdaCase #-}

-- unloadModules must drop the given modules and everything loaded that
-- refers to them.
--
-- The T27606a scenario; after the reload, unloadModules [A] must also
-- drop B, leaving the loaded set (interactive modules aside) empty.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
import GHC.Driver.Backend (bytecodeBackend)
import GHC.Driver.Env (hscInterp, hsc_HUG, hsc_home_unit, hsc_NC)
import GHC.Linker.Loader (getLoaderState, unloadModules)
import GHC.Linker.Types (LoaderState (..))
import GHC.Types.Name.Cache (NameCache)
import GHC.Unit.Home (mkHomeModule)
import GHC.Unit.Home.Graph (addHomeModInfoToHug, lookupHugByModule)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Module.Env (moduleEnvKeys)
import GHC.Unit.Types (isInteractiveModule)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  [libdir] <- getArgs
  writeA 1
  runGhc (Just libdir) $ do
    setupSession "B.hs"
    _ <- load LoadAllTargets
    setContext [ IIDecl (simpleImportDecl (mkModuleName "A"))
               , IIDecl (simpleImportDecl (mkModuleName "B")) ]

    evalInt "g" >>= liftIO . print
    printLoaded

    liftIO $ writeA 3
    hsc <- getSession
    hmi <- liftIO $ compileA libdir (hsc_NC hsc)
    liftIO $ addHomeModInfoToHug hmi (hsc_HUG hsc)

    evalInt "g" >>= liftIO . print
    evalInt "f" >>= liftIO . print

    liftIO $ unloadModules (hscInterp hsc) hsc [mi_module (hm_iface hmi)]
    printLoaded

writeA :: Int -> IO ()
writeA n = writeFile "A.hs" ("module A where\nf :: Int\nf = " ++ show n ++ "\n")

setupSession :: String -> Ghc ()
setupSession target = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df { backend = bytecodeBackend, ghcLink = LinkInMemory }
  t <- guessTarget target Nothing Nothing
  setTargets [t]

-- Compile the new A in a second session and hand back its HomeModInfo. The
-- second session shares our name cache, so the Names in the result are our
-- Names.
compileA :: String -> NameCache -> IO HomeModInfo
compileA libdir nc = runGhc (Just libdir) $ do
  getSession >>= \h -> setSession h { hsc_NC = nc }
  setupSession "A.hs"
  ok <- load LoadAllTargets
  when (failed ok) $ error "compileA: load failed"
  hsc <- getSession
  let aMod = mkHomeModule (hsc_home_unit hsc) (mkModuleName "A")
  liftIO (lookupHugByModule aMod (hsc_HUG hsc)) >>= \case
    Just hmi -> pure hmi
    Nothing -> error "compileA: A not in the home package table"

evalInt :: String -> Ghc Int
evalInt e = unsafeCoerce <$> compileExpr e

printLoaded :: Ghc ()
printLoaded = do
  hsc <- getSession
  liftIO $ getLoaderState (hscInterp hsc) >>= \case
    Nothing -> error "loader not initialised"
    Just st -> print [ moduleNameString (moduleName m)
                     | m <- moduleEnvKeys (bcos_loaded st)
                     , not (isInteractiveModule m) ]
