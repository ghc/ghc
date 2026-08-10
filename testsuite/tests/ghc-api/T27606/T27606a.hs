{-# LANGUAGE LambdaCase #-}

-- Stale bytecode must not survive a change to a dependency.
--
-- Load B (g = f + 1) and evaluate g. Recompile A with f = 3 in a second
-- session and put the result into the home unit graph, while the loader
-- still holds the old A. Evaluating g again must give the new value.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
import GHC.Driver.Backend (bytecodeBackend)
import GHC.Driver.Env (hsc_HUG, hsc_home_unit, hsc_NC)
import GHC.Types.Name.Cache (NameCache)
import GHC.Unit.Home (mkHomeModule)
import GHC.Unit.Home.Graph (addHomeModInfoToHug, lookupHugByModule)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))

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

    liftIO $ writeA 3
    hsc <- getSession
    hmi <- liftIO $ compileA libdir (hsc_NC hsc)
    liftIO $ addHomeModInfoToHug hmi (hsc_HUG hsc)

    evalInt "g" >>= liftIO . print
    evalInt "f" >>= liftIO . print

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
