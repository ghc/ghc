{-# LANGUAGE LambdaCase #-}

-- The T27606a scenario compiled to object code, with an extra module C that
-- does not depend on A.
--
-- After A changes, B must be reloaded with it: B's object resolved its
-- references to A's old addresses when it was loaded. C must be left alone:
-- it appends to c.log each time a fresh copy computes its value, and the
-- count must stay at one.
-- See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
module Main where

import GHC
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
  writeC
  runGhc (Just libdir) $ do
    setupSession ["B.hs", "C.hs"]
    _ <- load LoadAllTargets
    setContext [ IIDecl (simpleImportDecl (mkModuleName "Prelude"))
               , IIDecl (simpleImportDecl (mkModuleName "A"))
               , IIDecl (simpleImportDecl (mkModuleName "B"))
               , IIDecl (simpleImportDecl (mkModuleName "C")) ]

    evalInt "g + c" >>= liftIO . print
    evalInt "g + c" >>= liftIO . print

    liftIO $ writeA 3
    hsc <- getSession
    hmi <- liftIO $ compileA libdir (hsc_NC hsc)
    liftIO $ addHomeModInfoToHug hmi (hsc_HUG hsc)

    evalInt "g + c" >>= liftIO . print
    evalInt "g + c" >>= liftIO . print

    -- Bytes in c.log = times a fresh copy of C computed its value.
    liftIO $ readFile "c.log" >>= print . length

writeA :: Int -> IO ()
writeA n = writeFile "A.hs" ("module A where\nf :: Int\nf = " ++ show n ++ "\n")

writeC :: IO ()
writeC = writeFile "C.hs" $ unlines
  [ "module C where"
  , "import System.IO.Unsafe (unsafePerformIO)"
  , "c :: Int"
  , "{-# NOINLINE c #-}"
  , "c = unsafePerformIO (appendFile \"c.log\" \"x\" >> pure 100)"
  ]

setupSession :: [String] -> Ghc ()
setupSession targets = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df { ghcLink = LinkInMemory }
  ts <- mapM (\t -> guessTarget t Nothing Nothing) targets
  setTargets ts

-- Compile the new A in a second session and hand back its HomeModInfo. The
-- second session shares our name cache, so the Names in the result are our
-- Names.
compileA :: String -> NameCache -> IO HomeModInfo
compileA libdir nc = runGhc (Just libdir) $ do
  getSession >>= \h -> setSession h { hsc_NC = nc }
  setupSession ["A.hs"]
  ok <- load LoadAllTargets
  when (failed ok) $ error "compileA: load failed"
  hsc <- getSession
  let aMod = mkHomeModule (hsc_home_unit hsc) (mkModuleName "A")
  liftIO (lookupHugByModule aMod (hsc_HUG hsc)) >>= \case
    Just hmi -> pure hmi
    Nothing -> error "compileA: A not in the home package table"

evalInt :: String -> Ghc Int
evalInt e = unsafeCoerce <$> compileExpr e
