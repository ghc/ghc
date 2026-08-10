{-# LANGUAGE LambdaCase #-}

-- Foreign stubs across a reload.
--
-- The capi import generates a C wrapper, so evaluating use runs Haskell ->
-- stub object -> libm. The module's bytecode carries the compiled stub
-- object. The replacement's stub defines the same C symbol; on a static
-- interpreter this only resolves because the old stub's symbols were
-- purged first. Finally unloadModules [C] drops the module and its stubs.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
import GHC.Driver.Backend (bytecodeBackend)
import GHC.Driver.Env (hscInterp, hsc_HUG, hsc_home_unit, hsc_NC)
import GHC.Linker.Loader (getLoaderState, unloadModules)
import GHC.Linker.Types
import GHC.Types.Name.Cache (NameCache)
import GHC.Unit.Home (mkHomeModule)
import GHC.Unit.Home.Graph (addHomeModInfoToHug, lookupHugByModule)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.Env (moduleEnvKeys)
import GHC.Unit.Types (isInteractiveModule)

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import System.Directory (copyFile)
import System.Environment (getArgs)
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  [libdir] <- getArgs
  writeC 1
  runGhc (Just libdir) $ do
    setupSession "C.hs"
    _ <- load LoadAllTargets
    setContext [ IIDecl (simpleImportDecl (mkModuleName "C")) ]

    evalInt "use" >>= liftIO . print

    liftIO $ writeC 2
    hsc <- getSession
    hmi <- liftIO $ compileC libdir (hsc_NC hsc)
    liftIO $ addHomeModInfoToHug hmi (hsc_HUG hsc)

    evalInt "use" >>= liftIO . print

    liftIO $ unloadModules (hscInterp hsc) hsc [mi_module (hm_iface hmi)]
    printLoaded

writeC :: Int -> IO ()
writeC n = writeFile "C.hs" $ unlines
  [ "{-# LANGUAGE CApiFFI #-}"
  , "module C where"
  , "import Foreign.C.Types"
  , "foreign import capi \"math.h cos\" c_cos :: CDouble -> CDouble"
  , "use :: Int"
  , "use = round (c_cos 0) + " ++ show n
  ]

setupSession :: String -> Ghc ()
setupSession target = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df { backend = bytecodeBackend, ghcLink = LinkInMemory }
  t <- guessTarget target Nothing Nothing
  setTargets [t]

-- Compile the new C in a second session and hand back its HomeModInfo. The
-- second session shares our name cache, so the Names in the result are our
-- Names.
compileC :: String -> NameCache -> IO HomeModInfo
compileC libdir nc = runGhc (Just libdir) $ do
  getSession >>= \h -> setSession h { hsc_NC = nc }
  setupSession "C.hs"
  ok <- load LoadAllTargets
  when (failed ok) $ error "compileC: load failed"
  hsc <- getSession
  let cMod = mkHomeModule (hsc_home_unit hsc) (mkModuleName "C")
  liftIO (lookupHugByModule cMod (hsc_HUG hsc)) >>= \case
    Just hmi -> liftIO (rescueForeigns hmi)
    Nothing -> error "compileC: C not in the home package table"

-- Stub objects are temp files of the session that compiled them, and the
-- second session deletes its temp files on exit. Copy them next to the
-- test files and point the linkable at the copies.
rescueForeigns :: HomeModInfo -> IO HomeModInfo
rescueForeigns hmi = do
  bc <- traverse (traverse go) (homeMod_bytecode (hm_linkable hmi))
  pure hmi { hm_linkable = (hm_linkable hmi) { homeMod_bytecode = bc } }
  where
    go mbc = do
      fps <- forM (zip [0 :: Int ..] (gbc_foreign_files mbc)) $ \(i, fp) -> do
        let dst = "stub_" ++ show i ++ ".o"
        copyFile fp dst
        pure dst
      pure mbc { gbc_foreign_files = fps }

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
