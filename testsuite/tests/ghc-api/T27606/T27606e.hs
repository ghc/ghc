{-# LANGUAGE LambdaCase #-}

-- The T27606a scenario under the object suffix substitution: GHC itself is
-- dynamically linked, the session builds static objects with dynamic-too, so
-- the interpreter loads the .dyn_o files while the home unit graph has the
-- .o hashes.
--
-- B appends to g.log each time a fresh copy computes g. The count must be
-- exactly two: the initial load, and one reload after A changes. If the
-- dyn_o hash were used as the identity, the count would grow with every
-- evaluation. The log is a file because the loaded dynamic code has its own
-- copy of base, whose buffered stdout is never flushed.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
import GHC.Driver.Env (hsc_HUG, hsc_home_unit, hsc_NC)
import GHC.Driver.Session (gopt_set, GeneralFlag (..))
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
  writeB
  runGhc (Just libdir) $ do
    setupSession "B.hs"
    _ <- load LoadAllTargets
    setContext [ IIDecl (simpleImportDecl (mkModuleName "A"))
               , IIDecl (simpleImportDecl (mkModuleName "B")) ]

    evalInt "g" >>= liftIO . print
    evalInt "g" >>= liftIO . print

    liftIO $ writeA 3
    hsc <- getSession
    hmi <- liftIO $ compileA libdir (hsc_NC hsc)
    liftIO $ addHomeModInfoToHug hmi (hsc_HUG hsc)

    evalInt "g" >>= liftIO . print
    evalInt "g" >>= liftIO . print

    -- Bytes in g.log = times a fresh copy of B computed g.
    liftIO $ readFile "g.log" >>= print . length

writeA :: Int -> IO ()
writeA n = writeFile "A.hs" ("module A where\nf :: Int\nf = " ++ show n ++ "\n")

writeB :: IO ()
writeB = writeFile "B.hs" $ unlines
  [ "module B where"
  , "import A"
  , "import System.IO.Unsafe (unsafePerformIO)"
  , "g :: Int"
  , "{-# NOINLINE g #-}"
  , "g = unsafePerformIO (appendFile \"g.log\" \"x\" >> pure (f + 1))"
  ]

setupSession :: String -> Ghc ()
setupSession target = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags (gopt_set df Opt_BuildDynamicToo) { ghcLink = LinkInMemory }
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
