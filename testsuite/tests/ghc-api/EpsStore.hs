-- | GHC.setExternalPackageStore: a GHC API user can install their own
-- External Package State store. Here: a store which counts the calls of GHC's
-- in-memory store, and a store which drops the declarations of the modules it
-- is given and rehydrates them on demand with 'epm_rehydrate'.
-- See Note [Abstract External Package State] in GHC.Unit.External.Store.
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import System.Environment

import GHC
import GHC.Iface.Syntax (IfaceDecl)
import GHC.Types.Name (nameOccName, nameModule_maybe)
import GHC.Types.Name.Occurrence (OccEnv, emptyOccEnv, extendOccEnvList, lookupOccEnv)
import GHC.Unit.External.InMemory (newInMemoryEps)
import GHC.Unit.External.Store
import GHC.Unit.Module.Env

data Counts = Counts { added :: !Int, lookups :: !Int, rehydrated :: !Int }

-- | Delegates to the in-memory store, but keeps the 'IfaceDecl's of a module
-- instead of its typechecked declarations (without any caching).
rehydratingStore :: IORef Counts -> IO ExternalPackageState
rehydratingStore counts = do
  inner <- newInMemoryEps
  decls <- newIORef (emptyModuleEnv :: ModuleEnv (OccEnv IfaceDecl, IfaceDecl -> IO [(Name, TyThing)]))
  let bump f = atomicModifyIORef' counts (\c -> (f c, ()))
      add epm = do
        bump (\c -> c { added = added c + 1 })
        -- Index every name the declarations bind, via a first rehydration.
        idx <- fmap concat $ forM (epm_iface_decls epm) $ \(_, d) -> do
          things <- epm_rehydrate epm d
          pure [ (nameOccName n, d) | (n, _) <- things ]
        atomicModifyIORef' decls $ \m ->
          (extendModuleEnv m (epm_module epm)
             (extendOccEnvList emptyOccEnv idx, epm_rehydrate epm), ())
        eps_addModule inner epm { epm_decls = [] }
      lookup_thing name = do
        bump (\c -> c { lookups = lookups c + 1 })
        m <- readIORef decls
        case nameModule_maybe name >>= lookupModuleEnv m of
          Nothing -> eps_lookupTyThing inner name
          Just (occs, rehydrate) -> case lookupOccEnv occs (nameOccName name) of
            Nothing -> pure Nothing
            Just d -> do
              bump (\c -> c { rehydrated = rehydrated c + 1 })
              Prelude.lookup name <$> rehydrate d
  pure inner { eps_addModule = add, eps_lookupTyThing = lookup_thing }

compileWith :: String -> FilePath -> ExternalPackageState -> IO Bool
compileWith libdir file eps = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags { backend = noBackend, ghcLink = NoLink }
  setExternalPackageStore eps
  target <- guessTarget file Nothing Nothing
  setTargets [target]
  succeeded <$> load LoadAllTargets

main :: IO ()
main = do
  [libdir] <- getArgs
  counts <- newIORef (Counts 0 0 0)
  eps <- rehydratingStore counts
  ok <- compileWith libdir "EpsStore_A.hs" eps
  print ok
  c <- readIORef counts
  putStrLn ("modules added: " ++ show (added c > 0))
  putStrLn ("lookups went through the store: " ++ show (lookups c > 0))
  putStrLn ("declarations rehydrated: " ++ show (rehydrated c > 0))
