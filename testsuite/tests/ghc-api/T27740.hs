{-# LANGUAGE LambdaCase #-}

-- unloadModules must remove the static pointer table entries of the
-- modules it drops.
--
-- Load S, take the key of its static pointer, drop S. The key must no
-- longer resolve.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
import GHC.Driver.Backend (bytecodeBackend)
import GHC.Driver.Env (hscInterp, hsc_home_unit)
import GHC.Linker.Loader (unloadModules)
import GHC.StaticPtr (StaticKey, StaticPtr, unsafeLookupStaticPtr)
import GHC.Unit.Home (mkHomeModule)

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  [libdir] <- getArgs
  writeS
  runGhc (Just libdir) $ do
    setupSession "S.hs"
    _ <- load LoadAllTargets
    setContext [ IIDecl (simpleImportDecl (mkModuleName "GHC.StaticPtr"))
               , IIDecl (simpleImportDecl (mkModuleName "S")) ]

    key <- evalKey "staticKey sp"
    lookupKey key >>= liftIO . print

    hsc <- getSession
    let sMod = mkHomeModule (hsc_home_unit hsc) (mkModuleName "S")
    liftIO $ unloadModules (hscInterp hsc) hsc [sMod]

    lookupKey key >>= liftIO . print

writeS :: IO ()
writeS = writeFile "S.hs" $ unlines
  [ "{-# LANGUAGE StaticPointers #-}"
  , "module S where"
  , "import GHC.StaticPtr"
  , "sp :: StaticPtr Int"
  , "sp = static (5 :: Int)"
  ]

setupSession :: String -> Ghc ()
setupSession target = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df { backend = bytecodeBackend, ghcLink = LinkInMemory }
  t <- guessTarget target Nothing Nothing
  setTargets [t]

evalKey :: String -> Ghc StaticKey
evalKey e = unsafeCoerce <$> compileExpr e

lookupKey :: StaticKey -> Ghc Bool
lookupKey key = liftIO $
  isJust <$> (unsafeLookupStaticPtr key :: IO (Maybe (StaticPtr ())))
